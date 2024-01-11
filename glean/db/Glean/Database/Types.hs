{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Types (
  Writing(..), OpenDB(..), DBState(..),
  Write(..), WriteContent(..),
  DB(..),
  Env(..), WriteQueues(..), WriteQueue(..), WriteJob(..),
  Derivation(..),
  EnableRecursion(..),
  JanitorRunResult(..), JanitorException(..),
) where

import Control.DeepSeq
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar)
import Control.Exception
import Control.Trace (Tracer)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Time
import System.Clock

import Data.RateLimiterMap
import Util.EventBase (EventBaseDataplane)
import Util.STM

import Glean.Angle.Types
import qualified Glean.Database.Backup.Backend as Backup
import Glean.Database.Catalog (Catalog)
import Glean.Database.Config
import Glean.Database.Meta
import Glean.Database.Schema.Types
import Glean.Database.Storage (Database, Storage, describe)
import Glean.Database.Trace
import Glean.Database.Work.Heartbeat (Heartbeats)
import Glean.Database.Work.Queue (WorkQueue)
import Glean.Logger.Server (GleanServerLogger)
import Glean.Logger.Database (GleanDatabaseLogger)
import Glean.RTS.Foreign.LookupCache (LookupCache)
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Ownership (Ownership, Slice, DefineOwnership)
import Glean.RTS.Foreign.Subst (Subst)
import Glean.RTS.Types (Fid(..))
import qualified Glean.Recipes.Types as Recipes
import qualified Glean.ServerConfig.Types as ServerConfig
import qualified Glean.Types as Thrift
import Glean.Util.Metric (Point)
import Glean.Util.Mutex
import Glean.Util.Observed
import Glean.Util.ShardManager
import Glean.Util.Some
import Glean.Util.Time
import Glean.Util.Trace (Listener)
import Glean.Util.Warden
import Glean.Write.Stats (Stats)

-- Write caches
data Writing = Writing
  { -- Write lock
    wrLock :: Mutex ()

    -- First free Id in the write pipeline
  , wrNextId :: IORef Fid

    -- Write cache
  , wrLookupCache :: LookupCache
  , wrLookupCacheAnchorName :: TVar (Maybe Text)

    -- Queue of writes to this DB
  , wrQueue :: WriteQueue
  }

-- An open database
data OpenDB = forall storage. Storage storage => OpenDB
  { -- The database handle
    odbHandle :: Database storage

    -- Write queue, caches etc. Nothing means DB is read only.
  , odbWriting :: Maybe Writing

    -- Database schema
  , odbSchema :: DbSchema

    -- When was the database last used
  , odbIdleSince :: TVar TimePoint

    -- For a stacked DB, keep track of the slices of the base DBs.
    -- The list starts with this DB's base, then the base's base etc.
  , odbBaseSlices :: [Maybe Slice]

    -- ownership data from the DB
  , odbOwnership :: TVar (Maybe Ownership)
  }

-- State of a databases
data DBState
    -- In the process of being open
  = Opening

    -- Currently open
  | Open OpenDB

    -- In the process of being closed
  | Closing

    -- Currently closed
  | Closed

-- A known database
data DB = DB
  { -- The repo the database refers to
    dbRepo :: Thrift.Repo

    -- Database state
  , dbState :: TVar DBState

    -- Number of users
  , dbUsers :: TVar Int
  }

-- | A Write in progress that we can query via pollBatch
data Write = Write
  { writeWait :: MVar (Either SomeException Subst)
  , writeTimeout :: TimePoint
  }

-- | What we are going to write into the DB
data WriteContent = WriteContent
  { writeBatch :: !Thrift.Batch
  , writeOwnership :: Maybe DefineOwnership
  , writeSubst :: Subst -> Subst
  }

-- | A Write on the WriteQueue
data WriteJob
  = WriteJob
    { writeSize :: {-# UNPACK #-} !Int
    , writeContentIO :: IO WriteContent
    , writeDone :: MVar (Either SomeException Subst)
    , writeStart :: Point
    }
  | WriteCheckpoint
    { writeCheckpoint :: IO ()
      -- ^ invoke this action when all the preceding writes on the
      -- queue have completed, including those that are in progress.
    }

-- | The queue of WriteJobs and their total size
data WriteQueue = WriteQueue
  { writeQueue :: TQueue WriteJob
  , writeQueueActive :: TVar Int
     -- the number of active jobs on this write queue, used to
     -- implement checkpoints.
  , writeQueueCount :: TVar Int
    -- The number of 'WriteJob' in queue (excluding WriteCheckpoint)
  , writeQueueSize :: TVar Int
  , writeQueueLatency :: TVar TimeSpec
    -- latency of most recent write to this queue
  }

-- | So that we can round-robin writes to repos, have a queue of write queues
data WriteQueues = WriteQueues
  { writeQueues :: TQueue (Thrift.Repo, WriteQueue)
  , writeQueuesSize :: TVar Int
  }

-- | Information about a derived stored predicate being derived
data Derivation = Derivation
  { derivationStart :: TimePoint
  , derivationFinished :: Bool
  , derivationStats :: Thrift.UserQueryStats
  , derivationPendingWrites :: [Thrift.Handle]
  , derivationError :: Maybe (TimePoint, SomeException)
  , derivationHandle :: Thrift.Handle
  }

instance NFData Derivation where
  rnf Derivation{..} =
    derivationStart
    `seq` derivationFinished
    `seq` rnf derivationStats
    `seq` rnf derivationPendingWrites
    `seq` maybe () (`seq` ()) derivationError
    `seq` derivationHandle
    `seq`()

data EnableRecursion
  = EnableRecursion
  | DisableRecursion

data JanitorRunResult
  = JanitorRunSuccess
  | JanitorRunFailure JanitorException
  | JanitorTimeout
  | JanitorStuck
  | JanitorDisabled
  deriving Show

data JanitorException
  = OtherJanitorException SomeException
  | JanitorFetchBackupsFailure SomeException
    -- ^ Raised only when no remote db list available
  deriving (Typeable, Show)

instance Exception JanitorException

data Env = forall storage. Storage storage => Env
  { envEventBase :: EventBaseDataplane
  , envServerLogger :: Some GleanServerLogger
  , envDatabaseLogger :: Some GleanDatabaseLogger
  , envLoggerRateLimit :: RateLimiterMap Text
  , envCatalog :: Catalog
  , envStorage :: storage
  , envSchemaSource :: Observed SchemaIndex
    -- ^ The schema source, and its parsed/resolved form are both cached here.
  , envDbSchemaCache :: MVar DbSchemaCache
  , envUpdateSchema :: Bool
  , envSchemaUpdateSignal :: TMVar ()
  , envSchemaId :: Maybe Thrift.SchemaId
  , envRecipeConfig :: Observed Recipes.Config
  , envServerConfig :: Observed ServerConfig.Config
  , envBackupBackends :: Backup.Backends
  , envActive :: TVar (HashMap Thrift.Repo DB)
  , envDeleting :: TVar (HashMap Thrift.Repo (Async ()))
  , envCompleting :: TVar (HashMap Thrift.Repo (Async ()))
  , envCompletingDerived ::
      TVar (HashMap Thrift.Repo (HashMap PredicateId (Async ())))
  , envReadOnly :: Bool
  , envMockWrites :: Bool
  , envStats :: Stats
  , envLookupCacheStats :: LookupCache.Stats
  , envWarden :: Warden
  , envDatabaseJanitor :: TVar (Maybe (UTCTime, JanitorRunResult))
  , envDatabaseJanitorPublishedCounters :: TVar (HashSet ByteString)
  , envCachedRestorableDBs :: TVar (Maybe (UTCTime, [(Thrift.Repo, Meta)]))
  , envCachedAvailableDBs :: TVar (HashSet Thrift.Repo)
  , envWorkQueue :: WorkQueue
  , envHeartbeats :: Heartbeats
  , envWrites :: TVar (HashMap Text Write)
  , envDerivations :: TVar (HashMap (Thrift.Repo, PredicateId) Derivation)
  , envWriteQueues :: WriteQueues
  , envListener :: Listener
      -- ^ A 'Listener' which might get notified about various events. This is
      -- for testing support only.
  , envGetCurrentTime :: IO UTCTime
      -- ^ Yield the current time. Is normally getCurrentTime but
      -- can be changed for testing
  , envShardManager :: SomeShardManager
  , envEnableRecursion :: EnableRecursion
      -- ^ Experimental support for recursive queries. For testing only.
  , envFilterAvailableDBs :: [Thrift.Repo] -> IO [Thrift.Repo]
    -- ^ Filter out DBs not currently available in the server tier
  , envTracer :: Tracer GleanTrace
  }

instance Show Env where
  show Env{..} = unwords [ "Glean.Database.Types.Env {",
    "envStorage: " <> describe envStorage, "}" ]

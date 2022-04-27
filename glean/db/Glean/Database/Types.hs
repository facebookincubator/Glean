{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Types (
  Writing(..), OpenDB(..), DBState(..),
  Write(..),
  Tailer(..), TailerKey, DB(..),
  Env(..), WriteQueues(..), WriteQueue(..), WriteJob(..),
  Derivation(..)
) where

import Control.DeepSeq
import Control.Concurrent.Async
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Control.Exception
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time
import System.Clock

import Logger.IO
import Data.RateLimiterMap
import Util.EventBase (EventBaseDataplane)

import Glean.Angle.Types
import qualified Glean.Database.Backup.Backend as Backup
import Glean.Database.Catalog (Catalog)
import Glean.Database.Schema.Types
import Glean.Database.Stats (Stats)
import Glean.Database.Storage (Database, Storage)
import Glean.Database.Work.Heartbeat (Heartbeats)
import Glean.Database.Work.Queue (WorkQueue)
import Glean.RTS.Foreign.LookupCache (LookupCache)
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Ownership (Ownership, Slice)
import Glean.RTS.Foreign.Subst (Subst)
import Glean.RTS.Types (Fid(..))
import Glean.Schema.Resolve (Schemas)
import qualified Glean.Recipes.Types as Recipes
import qualified Glean.ServerConfig.Types as ServerConfig
import qualified Glean.Tailer as Tailer
import qualified Glean.Types as Thrift
import Glean.Util.Metric (Point)
import Glean.Util.Mutex
import Glean.Util.Observed
import Glean.Util.Time
import Glean.Util.Trace (Listener)
import Glean.Util.Warden

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

    -- for a stacked update DB, keep track of the slice of the base DB
  , odbBaseSlice :: Maybe Slice

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

-- | An active tailer
data Tailer = Tailer
  { tailerRepo :: Thrift.Repo
  , tailerTask :: Text
  , tailerCategory :: Text
  , tailerBucket :: Maybe Int
  , tailerThread :: Async ()
  }

-- A known database
data DB = DB
  { -- The repo the database refers to
    dbRepo :: Thrift.Repo

    -- Database state
  , dbState :: TVar DBState

    -- Number of users
  , dbUsers :: TVar Int

    -- Tailers associated with their tasks
  , dbTailers :: TVar (HashMap Text Tailer)
  }

-- | A Write in progress that we can query via pollBatch
data Write = Write
  { writeWait :: MVar (Either SomeException Subst)
  , writeTimeout :: TimePoint
  }

-- | A Write on the WriteQueue
data WriteJob
  = WriteJob
    { writeSize :: {-# UNPACK #-} !Int
    , writeTask :: Point -> IO Subst
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

-- | Uniquely identifies a tailer by the repo name, category, and bucket
type TailerKey = (Text, Text, Maybe Int)

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

data Env = forall storage. Storage storage => Env
  { envEventBase :: EventBaseDataplane
  , envLogger :: Logger
  , envLoggerRateLimit :: RateLimiterMap Text
  , envRoot :: FilePath
  , envCatalog :: Catalog
  , envStorage :: storage
  , envSchemaSource :: Observed (SourceSchemas, Schemas)
  , envSchemaUpdateSignal :: TMVar ()
    -- ^ The schema source, and its parsed/resolved form are both cached here.
  , envSchemaOverride :: Bool
  , envSchemaVersion :: Maybe Version
  , envRecipeConfig :: Observed Recipes.Config
  , envServerConfig :: Observed ServerConfig.Config
  , envBackupBackends :: Backup.Backends
  , envActive :: TVar (HashMap Thrift.Repo DB)
  , envDeleting :: TVar (HashMap Thrift.Repo (Async ()))
  , envCompleting :: TVar (HashMap Thrift.Repo (Async ()))
  , envReadOnly :: Bool
  , envMockWrites :: Bool
  , envStats :: Stats
  , envLookupCacheStats :: LookupCache.Stats
  , envWarden :: Warden
  , envDatabaseJanitor :: TVar (Maybe UTCTime)
  , envLastBackupsSync :: TVar (Maybe UTCTime)
  , envWorkQueue :: WorkQueue
  , envHeartbeats :: Heartbeats
  , envWrites :: TVar (HashMap Text Write)
  , envDerivations :: TVar (HashMap (Thrift.Repo, PredicateRef) Derivation)
  , envWriteQueues :: WriteQueues
  , envTailerOpts :: Tailer.TailerOptions
  , envTailers :: TVar (HashMap TailerKey Tailer)
      -- ^ Tailers keyed on (repo_name, category). This is a temporary field
      -- for killing duplicate tailers until we implement a better solution.
  , envListener :: Listener
      -- ^ A 'Listener' which might get notified about various events. This is
      -- for testing support only.
  , envGetCreationTime :: IO UTCTime
      -- ^ Yield the creation time for a DB. Is normally getCurrentTime but
      -- can be changed for testing where 1s granularity might not be enough
      -- to distinguish databases.
  }

instance Show Env where
  show env = unwords [ "Glean.Database.Types.Env {",
    "envRoot: " <> envRoot env, "}" ]

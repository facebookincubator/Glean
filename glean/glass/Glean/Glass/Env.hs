{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DuplicateRecordFields #-}
module Glean.Glass.Env
  (
    -- * Read-only configuration
    Config(..),
    setSnapshotBackend,
    setSourceControl,
    setHaxlState,
    setTracer,
    setAllocationLimit,
    updateWelcomeMessage,

    -- * Session resources
    Env,
    Env'(..),
    setUseSnapshotsForSymbolsList,
    withAllocationLimit,
  ) where

import Data.Int
import Data.Text (Text)

import Facebook.Fb303 (Fb303State)
import qualified Haxl.Core as Haxl
import Logger.IO (Logger)
import Util.AllocLimit
import Util.EventBase (EventBaseDataplane)
import Util.STM ( TVar )

import qualified Glean
import Glean.Impl.ConfigProvider (ConfigAPI)
import qualified Glean.LocalOrRemote as Glean
import Glean.Util.Some ( Some )
import Util.Time ( DiffTimePoints )

import Glean.Glass.Base (RepoMapping)
import Glean.Glass.Repos (GleanDBInfo)
import Glean.Glass.SnapshotBackend ( SnapshotBackend(..) )
import Glean.Glass.SourceControl
import Glean.Glass.Tracing (Tracer, GlassTrace)

-- | Init-time configuration
data Config trace = Config
  { listenPort :: Int
  , configKey :: Text
  , gleanService :: Glean.Service
  , serviceName :: Text
  , refreshFreq :: DiffTimePoints
      -- ^ refresh glean repos on this frequency
  , listDatabasesRetry :: Maybe Int
      -- ^ whether to trust listDatabases and how often to wait to retry N times
  , numWorkerThreads :: Maybe Int
  , snapshotBackend :: EventBaseDataplane -> IO (Some SnapshotBackend)
  , sourceControl :: EventBaseDataplane -> IO (Some SourceControl)
  , haxlState :: EventBaseDataplane -> IO Haxl.StateStore
      -- ^ Haxl datasource state to use with runHaxl, e.g. for sourceControl
  , tracer :: Tracer trace
  , welcomeMessage :: forall a. EventBaseDataplane -> Config a -> IO Text
  , useSnapshotsForSymbolsList :: IO Bool
  , allocationLimit :: IO (Maybe Int64)
  }

setSnapshotBackend
  :: (EventBaseDataplane -> IO(Some SnapshotBackend)) -> Config a -> Config a
setSnapshotBackend snapshotBackend config =
  config { snapshotBackend = snapshotBackend }

setSourceControl
  :: (EventBaseDataplane -> IO (Some SourceControl))
  -> Config a -> Config a
setSourceControl sourceControl config =
  config { sourceControl = sourceControl }

setHaxlState
  :: (EventBaseDataplane -> IO Haxl.StateStore)
  -> Config a -> Config a
setHaxlState st config = config { haxlState = st }

setTracer :: Tracer trace -> Config trace -> Config trace
setTracer tracer' Config{..}= Config{ tracer = tracer <> tracer', .. }

setUseSnapshotsForSymbolsList :: IO Bool -> Config trace -> Config trace
setUseSnapshotsForSymbolsList check Config{..} =
  Config { useSnapshotsForSymbolsList = check, .. }

setAllocationLimit :: IO (Maybe Int64) -> Config a -> Config a
setAllocationLimit l config = config { allocationLimit = l }

updateWelcomeMessage
  :: ( forall a. (EventBaseDataplane -> Config a -> IO Text)
      -> EventBaseDataplane -> Config a -> IO Text)
  -> Config a
  -> Config a
updateWelcomeMessage f config =
  config{ welcomeMessage = f (welcomeMessage config)}

-- | Read-only, scoped, dynamic resources.
type Env = Env' GlassTrace

data Env' trace = Env
  { evp :: EventBaseDataplane
  , cfgapi :: ConfigAPI
  , logger :: Logger
  , gleanBackend :: Some Glean.Backend
  , fb303 :: Fb303State
  , latestGleanRepos :: TVar GleanDBInfo
  , snapshotBackend :: Some SnapshotBackend
  , gleanDB :: Maybe Glean.Repo -- if provided, use as target Glean DB
  , repoMapping :: RepoMapping
  , sourceControl :: Some SourceControl
  , haxlState :: Haxl.StateStore
  , tracer :: Tracer trace
  , useSnapshotsForSymbolsList :: IO Bool
  , allocationLimit :: IO (Maybe Int64)
  }

withAllocationLimit :: Env' t -> IO a -> IO a
withAllocationLimit Env{..} act = do
  l <- allocationLimit
  maybe id limitAllocsThrow l act

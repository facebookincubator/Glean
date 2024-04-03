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

    -- * Session resources
    Env(..),
    IndexBackend(..),
  ) where

import Data.Text (Text)

import Facebook.Fb303 (Fb303State)
import Logger.IO (Logger)
import Util.EventBase (EventBaseDataplane)
import Util.STM ( TVar )

import qualified Glean
import Glean.Impl.ConfigProvider (ConfigAPI)
import qualified Glean.LocalOrRemote as Glean
import Glean.Remote (ThriftBackend)
import Glean.Util.Some ( Some )
import Glean.Util.Time ( DiffTimePoints )

import Glean.Glass.Base (RepoMapping)
import Glean.Glass.Repos (GleanDBInfo)
import Glean.Glass.SnapshotBackend ( SnapshotBackend(..) )

-- | Init-time configuration
data Config = Config
  { listenPort :: Int
  , configKey :: Text
  , gleanService :: Glean.Service
  , serviceName :: Text
  , refreshFreq :: DiffTimePoints
      -- ^ refresh glean repos on this frequency
  , listDatabasesRetry :: Maybe Int
      -- ^ whether to trust listDatabases and how often to wait to retry N times
  , numWorkerThreads :: Maybe Int
  , snapshotBackend :: EventBaseDataplane -> Some SnapshotBackend
  }

setSnapshotBackend
  :: (EventBaseDataplane -> Some SnapshotBackend) -> Config -> Config
setSnapshotBackend snapshotBackend config =
  config { snapshotBackend = snapshotBackend }

-- | Read-only, scoped, dynamic resources.
data Env = Env
  { evp :: EventBaseDataplane
  , cfgapi :: ConfigAPI
  , logger :: Logger
  , gleanBackend :: Some Glean.Backend
  , fb303 :: Fb303State
  , latestGleanRepos :: TVar GleanDBInfo
  , gleanIndexBackend :: IndexBackend
  , snapshotBackend :: Some SnapshotBackend
  , gleanDB :: Maybe Glean.Repo -- if provided, use as target Glean DB
  , repoMapping :: RepoMapping
  }

-- | A backend to create incremental databases
newtype IndexBackend = IndexBackend (Maybe ThriftBackend)

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Env
  (
    -- * Read-only configuration
    Config(..),

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
import qualified Glean.Repo as Glean
import qualified Glean.LocalOrRemote as Glean
import Glean.Remote (ThriftBackend)
import Glean.Util.Some ( Some )
import Glean.Util.Time ( DiffTimePoints )

import Glean.Glass.Repos (ScmRevisions)
import Glean.Glass.SnapshotBackend ( SnapshotBackend, SnapshotTier )

-- | Init-time configuration
data Config = Config
  { listenPort :: Int
  , configKey :: Text
  , gleanService :: Glean.Service
  , serviceName :: Text
  , refreshFreq :: DiffTimePoints -- ^ refresh glean repos on this frequency
  , numWorkerThreads :: Maybe Int
  , snapshotTier :: SnapshotTier
  }

-- | Read-only, scoped, dynamic resources.
data Env = Env
  { evp :: EventBaseDataplane
  , cfgapi :: ConfigAPI
  , logger :: Logger
  , gleanBackend :: Some Glean.Backend
  , fb303 :: Fb303State
  , latestGleanRepos :: TVar Glean.LatestRepos
  , repoScmRevisions :: TVar ScmRevisions
  , gleanIndexBackend :: IndexBackend
  , snapshotBackend :: SnapshotBackend
  , gleanDB :: Maybe Glean.Repo -- if provided, use as target Glean DB
  }

-- | A backend to create incremental databases
newtype IndexBackend = IndexBackend (Maybe ThriftBackend)

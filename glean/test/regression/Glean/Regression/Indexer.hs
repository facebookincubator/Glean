{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Indexer
  ( withTestBackend
  , withTestBackendWithSettings
  , closeTestDatabaseIfLocal
  , runIndexerForTest
  ) where

import Data.Maybe
import qualified Data.Text as Text
import System.FilePath

import Glean.Database.Config
import Glean.Database.Close (closeDatabase)
import Glean.LocalOrRemote
import Glean.Database.Test
import Glean.Indexer
import Glean.Regression.Config
import Glean.Types (Repo)
import Glean.Util.Some

-- | Set up a Backend for test runs
withTestBackend
  :: TestConfig
  -> (Some LocalOrRemote -> IO a)
  -> IO a
withTestBackend test action =
  withTestBackendWithSettings [] test action

withTestBackendWithSettings
  :: [Setting]
  -> TestConfig
  -> (Some LocalOrRemote -> IO a)
  -> IO a
withTestBackendWithSettings extraSettings test action =
  withTestEnv settings (action . Some)
  where
  settings = [ setRoot $ testOutput test </> "db" ] <>
    map (setSchemaLocation . SchemaLocation_dir . Text.pack)
      (maybeToList (testSchema test)) <>
    extraSettings

-- | Close a local test database so its completion metadata is reloaded on the
-- next access. Remote backends manage their own lifecycle.
closeTestDatabaseIfLocal :: Some LocalOrRemote -> Repo -> IO ()
closeTestDatabaseIfLocal backend repo =
  case backendKind backend of
    BackendEnv env -> closeDatabase env repo
    BackendThrift _ -> pure ()

runIndexerForTest :: Some LocalOrRemote -> RunIndexer -> TestConfig -> IO ()
runIndexerForTest backend indexer test =
  indexer backend (testRepo test) params
  where
  params = IndexerParams {
    indexerRoot = testRoot test,
    indexerProjectRoot = testProjectRoot test,
    indexerOutput = testOutput test,
    indexerGroup = testGroup test
    }

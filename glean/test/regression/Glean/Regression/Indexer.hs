{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Indexer
  ( withTestBackend
  , withTestDatabase
  ) where

import System.Exit
import System.FilePath

import Glean
import Glean.LocalOrRemote
import Glean.Database.Test
import Glean.Indexer
import Glean.Regression.Config
import Glean.Util.Some

-- | Set up a Backend for test runs
withTestBackend
  :: TestConfig
  -> (Some LocalOrRemote -> IO a)
  -> IO a
withTestBackend test action = withTestEnv settings (action . Some)
  where settings = [ setRoot $ testOutput test </> "db" ]

-- | Run the supplied Indexer to populate a temporary DB
withTestDatabase
  :: Some LocalOrRemote
  -> RunIndexer
  -> TestConfig
  -> (Repo -> IO a)
  -> IO a
withTestDatabase backend indexer test action = do
  let
    repo = testRepo test
    ver = fromIntegral <$> testSchemaVersion test
    params = IndexerParams {
      indexerRoot = testRoot test,
      indexerProjectRoot = testProjectRoot test,
      indexerOutput = testOutput test,
      indexerGroup = testGroup test
    }
  fillDatabase backend ver repo "" (die "repo already exists") $
    indexer backend repo params
  action repo

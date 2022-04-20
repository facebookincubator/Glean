{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Indexer
  ( withTestDatabase
  ) where

import System.Exit
import System.FilePath

import Glean
import Glean.Database.Test
import Glean.Indexer
import Glean.Regression.Config
import Glean.Util.Some

-- | Run the supplied Indexer to populate a temporary DB
withTestDatabase
  :: RunIndexer
  -> TestConfig
  -> (Some Backend -> Repo -> IO a)
  -> IO a
withTestDatabase indexer test action =
  withTestEnv settings $ \backend -> do
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
    indexer (Some backend) repo params
  action (Some backend) repo
  where
    settings =
        [ setRoot $ testOutput test </> "db" ]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Run a snapshot test with an external indexer

module Glean.Regression.Driver.External (main) where

import System.Environment

import Util.Log

import Glean.Indexer
import Glean.Indexer.List (cmdLineParser)

import Glean.Regression.Snapshot (testMain)
import Glean.Regression.Snapshot.Driver

snapshotDriver :: Driver RunIndexer
snapshotDriver = driverFromIndexer
  Indexer {
    indexerShortName = "multiple",
    indexerDescription = "Choose an indexer to run",
    indexerOptParser = cmdLineParser,
    indexerRun = id
  }

main :: IO ()
main = do
  args <- getArgs
  logInfo $ "args: " <> show args
    -- useful for debugging issues in surrounding scripts that pass
    -- arguments in.
  testMain snapshotDriver

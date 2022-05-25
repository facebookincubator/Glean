{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Lsif.Main ( main ) where

import System.Environment

import Glean.Indexer.LSIF as Lsif
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver
import System.FilePath

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path, "--input", path </> indexFile]) $
    testMain (driverFromIndexer Lsif.indexer)
  where
    -- we have snapshots of all the main LSIF indexers
    -- to spot any regression in our LSIF translator
    path = "glean/lang/lsif/tests/cases/lsif-tsc"
    indexFile = "lsif/dump.lsif"

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.HackXLang (main) where

import qualified Glean.Regression.Snapshot.Driver as Glean
import Glean.Indexer.Fbthrift as Fbthrift (indexer)
import Glean.Glass.Regression.Snapshot (mainGlassSnapshotXLang)
import Glean.Indexer.Hack as Hack

main :: IO ()
main = mainGlassSnapshotXLang
  testName
    testPath (d, "test-xlang-source") (Fbthrift.indexer, "test-xlang-target")
  where
    d = Glean.driverFromIndexer Hack.indexer
    testName = "glass-regression-hack-xlang"
    testPath = "glean/glass/test/regression/tests/hack-xlang"

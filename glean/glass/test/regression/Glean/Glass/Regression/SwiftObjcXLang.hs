{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.SwiftObjcXLang (main) where

import qualified Glean.Regression.Snapshot.Driver as Glean
import Glean.Indexer.Swift as Swift
import Glean.Indexer.Cpp as Cpp
import Glean.Glass.Regression.Snapshot (mainGlassSnapshotXLang)

main :: IO ()
main =
  mainGlassSnapshotXLang
    testName
    testPath
    (Glean.driverFromIndexer Swift.indexer, "test-xlang-source")
    (Cpp.indexerNoDeriv, "test-xlang-target")
  where
    testName = "glass-regression-swift-objc-xlang"
    testPath = "glean/glass/test/regression/tests/swift-objc-xlang"

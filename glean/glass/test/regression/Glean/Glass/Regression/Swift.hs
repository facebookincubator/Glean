{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Swift (main) where

import Glean.Indexer.Swift as Swift
import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-swift"
    testPath = "glean/glass/test/regression/tests/swift"
    testIndexer = Swift.indexer

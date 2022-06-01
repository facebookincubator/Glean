{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.JavaLsif (main) where

import Glean.Indexer.JavaLsif as JavaLsif
import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-java-lsif"
    testPath = "glean/glass/test/regression/tests/java-lsif"
    testIndexer = JavaLsif.indexer

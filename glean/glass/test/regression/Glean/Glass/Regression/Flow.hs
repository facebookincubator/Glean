{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Flow (main) where

import Glean.Indexer.Flow as Flow ( indexer )
import Glean.Glass.Regression.Snapshot ( mainGlassSnapshot )

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-flow"
    testPath = "glean/glass/test/regression/tests/flow"
    testIndexer = Flow.indexer

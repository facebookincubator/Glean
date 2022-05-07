{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Go (main) where

import Glean.Indexer.Go as Go
import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-go"
    testPath = "glean/glass/test/regression/tests/go"
    testIndexer = Go.indexer

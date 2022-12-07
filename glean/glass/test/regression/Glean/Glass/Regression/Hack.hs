{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Hack (main) where

import Glean.Indexer.Hack as Hack
import Glean.Glass.Regression.Snapshot as Glass

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-hack"
    testPath = "glean/glass/test/regression/tests/hack"
    testIndexer = Hack.indexer

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Skycastle (main) where

import Test.HUnit

import Glean.Glass.Regression.Snapshot as Glass
import Glean.Glass.Regression.Tests
import Glean.Glass.Types
import Glean.Indexer.Python as Python


main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-skycastle"
    testPath = "glean/glass/test/regression/tests/skycastle"
    testIndexer = Python.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "sky_project/main.sky") get
  , testDocumentSymbolListX (Path "sky_project/workflow.sky") get
  ]

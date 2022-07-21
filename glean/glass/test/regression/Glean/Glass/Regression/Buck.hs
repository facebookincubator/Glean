{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Buck (main) where

import Test.HUnit

import Glean.Glass.Regression.Snapshot as Glass
import Glean.Glass.Regression.Tests
import Glean.Glass.Types
import Glean.Indexer.Python as Python


main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-buck"
    testPath = "glean/glass/test/regression/tests/buck"
    testIndexer = Python.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "buck_project/TEST_TARGETS") get
  ]

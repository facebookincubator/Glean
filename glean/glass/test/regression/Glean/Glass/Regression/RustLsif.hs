{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.RustLsif (main) where

import Glean.Indexer.RustLsif as RustLsif
import Test.HUnit

import Glean.Glass.Regression.Snapshot
import Glean.Glass.Regression.Tests
import Glean.Glass.Types

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-rust-lsif"
    testPath = "glean/glass/test/regression/tests/rust-lsif"
    testIndexer = RustLsif.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "src/lib.rs") get
  ]

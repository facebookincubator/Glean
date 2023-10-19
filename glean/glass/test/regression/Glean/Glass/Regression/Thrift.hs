{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Thrift (main) where

import Test.HUnit

import Glean.Indexer.Fbthrift as Fbthrift

import Glean.Glass.Types
import Glean.Glass.Regression.Snapshot
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-thrift"
    testPath = "glean/glass/test/regression/tests/thrift"
    testIndexer = Fbthrift.indexer

unitTests :: Getter -> [Test]
unitTests get = [testDocumentSymbolListX (Path "lib.thrift") get]

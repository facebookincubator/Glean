{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.TypeScript (main) where

import Test.HUnit

import Glean.Glass.Regression.Snapshot as Glass
import Glean.Glass.Regression.Tests
import Glean.Glass.Types
import Glean.Indexer.TypescriptLsif as Typescript

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-typescript"
    testPath = "glean/glass/test/regression/tests/typescript"
    testIndexer = Typescript.indexer

-- some legacy unit tests
unitTests :: Glass.Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX path get
  , testResolveSymbol sym path get
  , testDescribeSymbolMatchesPath sym path get
  ]
  where
    path = Path "glean/lang/typescript/tests/cases/xrefs/example.ts"
    sym = SymbolId "test/ts/lsif/example%3AcreateTempRepo"

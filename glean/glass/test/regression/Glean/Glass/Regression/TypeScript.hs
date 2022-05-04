{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.TypeScript (main) where

import Test.HUnit

import Glean.Indexer.Typescript as Typescript
import Glean.Regression.Test

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainTestIndex "glass-regression-typescript" Typescript.indexer $ \get -> TestList
  [ testDocumentSymbolListX path get
  , testResolveSymbol sym path get
  , testDescribeSymbolMatchesPath sym path get
--  , testSearchSymbolByName "createTempRepo" sym get
  ]
  where
    path = Path "glean/lang/typescript/tests/cases/xrefs/example.ts"
    sym = SymbolId "test/ts/lsif/example%3AcreateTempRepo"

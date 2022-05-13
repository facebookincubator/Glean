{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Flow (main) where

import Test.HUnit
import Data.Text (Text)

import Glean.Indexer.Flow as Flow

import Glean
import Glean.Util.Some
import Glean.Glass.Types
import Glean.Glass.Regression.Tests

import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-flow"
    testPath = "glean/glass/test/regression/tests/flow"
    testIndexer = Flow.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "test/imports.js") get
  , testSymbolIdLookup get
  ]

testSymbolIdLookup :: IO (Some Backend, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  "test/js/test/es_exports.js.flow/foo" --> "test/es_exports.js.flow",
  "test/js/test/es_exports.js.flow/bor" --> "test/es_exports.js.flow",
  "test/js/test/es_exports.js.flow/d" --> "test/es_exports.js.flow",
  "test/js/test/es_exports.js.flow/C" --> "test/es_exports.js.flow",
  "test/js/test/es_exports.js.flow/num" --> "test/es_exports.js.flow",
  "test/js/test/cjs_exports.js/plus" --> "test/cjs_exports.js",
  "test/js/test/imports.js/a" --> "test/imports.js",
  "test/js/test/imports.js/caz" --> "test/imports.js",
  "test/js/test/imports.js/s" --> "test/imports.js",
  "test/js/test/imports.js/qux" --> "test/imports.js",
  "test/js/test/imports.js/str" --> "test/imports.js",
  "test/js/test/imports.js/fn" --> "test/imports.js"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

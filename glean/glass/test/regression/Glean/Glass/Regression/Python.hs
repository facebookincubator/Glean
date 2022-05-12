{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Python (main) where

import Data.Bifunctor
import Data.Text (Text)
import Test.HUnit

import Glean
import Glean.Indexer.Python as Python
import Glean.Regression.Test
import Glean.Util.Some

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainTestIndex "glass-regression-python" Python.indexer $ \get -> TestList
  [ testDocumentSymbolListX (Path "all.py") get
  , testSymbolIdLookup get
  , testPythonFindReferences get
  -- Python doesn't support comments yet, so no testDescribeSymbolComments
  ]

testSymbolIdLookup :: IO (Some Backend, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  -- class
  "test/py/big_lib.big_func" --> "big_lib/__init__.py",
  "test/py/big_lib.big_var" --> "big_lib/__init__.py",
  "test/py/lib.HelperClass" --> "lib.py",
  "test/py/lib.HelperClass.value" --> "lib.py",
  "test/py/lib.HelperClass.method" --> "lib.py",
  "test/py/lib.HelperClass.another_method" --> "lib.py",
  "test/py/lib.helper_func" --> "lib.py",
  "test/py/main.f" --> "main.py",
  "test/py/main.g" --> "main.py",
  "test/py/shadow.ShadowClass" --> "shadow.py",
  "test/py/shadow.ShadowClass.big_func" --> "shadow.py",
  "test/py/shadow.ShadowClass.other_var" --> "shadow.py",
  "test/py/shadow.local_var" --> "shadow.py",
  "test/py/shadow.big_func" --> "shadow.py",
  "test/py/shadow.other_var" --> "shadow.py",
  "test/py/all.LocalClass" --> "all.py"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

testPythonFindReferences :: IO (Some Backend, Repo) -> Test
testPythonFindReferences get = TestLabel "findReferences" $ TestList [
  "test/py/big_lib.big_func" --> [
    ("all.py",2),
    ("big_lib/relative.py",2),
    ("lib.py",1),
    ("main.py",2),
    ("shadow.py",4) ],
  "test/py/big_lib.big_var" --> [
    ("all.py",3),
    ("as.py",1),
    ("big_lib/__init__.py",1),
    ("shadow.py",4) ],
  "test/py/lib.HelperClass" --> [
    ("all.py",5),
    ("as.py",1),
    ("lib.py",5),
    ("main.py",4) ],
  "test/py/lib.HelperClass.value" --> [],
  "test/py/lib.HelperClass.method" --> [],
  "test/py/lib.HelperClass.another_method" --> [],
  "test/py/lib.helper_func" --> [("main.py",2)],
  "test/py/main.f" --> [("main.py",1)],
  "test/py/main.g" --> [],
  "test/py/shadow.ShadowClass" --> [],
  "test/py/shadow.ShadowClass.big_func" --> [("shadow.py",1)],
  "test/py/shadow.ShadowClass.other_var" --> [],
  "test/py/shadow.local_var" --> [("shadow.py",3)],
  "test/py/shadow.big_func" --> [("shadow.py",3)],
  "test/py/shadow.other_var" --> [],
  "test/py/all.LocalClass" --> [("all.py",1)]
  ]
  where
    (-->) :: Text -> [(Text,Int)] -> Test
    sym --> expected =
      testFindReferences
        (SymbolId sym)
        (map (first Path) expected)
        get

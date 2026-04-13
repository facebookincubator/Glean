{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.PythonPyrefly (main) where

import Data.Bifunctor
import Data.Text (Text)
import Test.HUnit

import Glean
import Glean.LocalOrRemote
import Glean.Indexer.PythonPyrefly as PythonPyrefly
import Glean.Util.Some

import Glean.Glass.Regression.Snapshot
import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-python-pyrefly"
    testPath = "glean/glass/test/regression/tests/python-pyrefly"
    testIndexer = PythonPyrefly.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py") get
  , testSymbolIdLookup get
  , testPythonFindReferences get
  -- Python doesn't support comments yet, so no testDescribeSymbolComments
  ]

testSymbolIdLookup :: IO (Some LocalOrRemote, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  -- class
  "test/py/glean/big_lib.big_func" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/big_lib/__init__.py",
  "test/py/glean/big_lib.big_var" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/big_lib/__init__.py",
  "test/py/glean/lib.HelperClass" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",
  "test/py/glean/lib.HelperClass.value" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",
  "test/py/glean/lib.HelperClass.method" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",
  "test/py/glean/lib.HelperClass.another_method" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",
  "test/py/glean/lib.helper_func" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",
  "test/py/glean/main.f" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",
  "test/py/glean/main.g" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",
  "test/py/glean/shadow.ShadowClass" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/shadow.ShadowClass.big_func" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/shadow.ShadowClass.other_var" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/shadow.local_var" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/shadow.big_func" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/shadow.other_var" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",
  "test/py/glean/all.LocalClass" --> "glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

testPythonFindReferences :: IO (Some LocalOrRemote, Repo) -> Test
testPythonFindReferences get = TestLabel "findReferences" $ TestList [
  "test/py/glean/big_lib.big_func" --> [
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py",2),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/big_lib/relative.py",2),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",1),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",4),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",3)],
  "test/py/glean/big_lib.big_var" --> [
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py",3),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/as.py",1),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/big_lib/__init__.py",1),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",3)],
  "test/py/glean/lib.HelperClass" --> [
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py",5),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/as.py",1),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/lib.py",5),
    ("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",4) ],
  "test/py/glean/lib.HelperClass.value" --> [],
  "test/py/glean/lib.HelperClass.method" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",1)],
  "test/py/glean/lib.HelperClass.another_method" --> [],
  "test/py/glean/lib.helper_func" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",2)],
  "test/py/glean/main.f" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/main.py",1)],
  "test/py/glean/main.g" --> [],
  "test/py/glean/shadow.ShadowClass" --> [],
  "test/py/glean/shadow.ShadowClass.big_func" --> [],
  "test/py/glean/shadow.ShadowClass.other_var" --> [],
  "test/py/glean/shadow.local_var" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",2)],
  "test/py/glean/shadow.big_func" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/shadow.py",2)],
  "test/py/glean/shadow.other_var" --> [],
  "test/py/glean/all.LocalClass" --> [("glean/lang/python-pyrefly/tests/regression/without_dynamic_import/core/xrefs/all.py",1)]
  ]
  where
    (-->) :: Text -> [(Text,Int)] -> Test
    sym --> expected =
      testFindReferences
        (SymbolId sym)
        (map (first Path) expected)
        get

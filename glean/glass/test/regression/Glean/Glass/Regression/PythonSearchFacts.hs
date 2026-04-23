{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.PythonSearchFacts (main) where

import Data.Text (Text)
import Test.HUnit

import Glean
import Glean.LocalOrRemote
import Glean.Indexer.Python as Python
import Glean.Util.Some

import Glean.Glass.Regression.Snapshot
import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-python-search-facts"
    testPath = "glean/glass/test/regression/tests/python_search_facts"
    testIndexer = Python.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "example.py") get
  , testSymbolIdLookup get
  ]

testSymbolIdLookup :: IO (Some LocalOrRemote, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  -- classes
  "test/py/example.AppConfig" --> "example.py",
  "test/py/example.AppConfig.Nested" --> "example.py",
  "test/py/models.base.BaseModel" --> "models/base.py",
  "test/py/models.user.User" --> "models/user.py",
  "test/py/helpers.Formatter" --> "helpers.py",
  -- methods
  "test/py/example.AppConfig.validate" --> "example.py",
  "test/py/models.base.BaseModel.save" --> "models/base.py",
  "test/py/models.base.BaseModel.delete" --> "models/base.py",
  "test/py/models.user.User.get_display_name" --> "models/user.py",
  "test/py/helpers.Formatter.format_name" --> "helpers.py",
  -- functions
  "test/py/example.initialize" --> "example.py",
  "test/py/example.shutdown" --> "example.py",
  "test/py/models.user.find_user" --> "models/user.py",
  "test/py/helpers.format_list" --> "helpers.py",
  "test/py/helpers.identity" --> "helpers.py",
  -- fields
  "test/py/example.AppConfig.debug" --> "example.py",
  "test/py/example.AppConfig.max_retries" --> "example.py",
  "test/py/models.base.BaseModel.id" --> "models/base.py",
  "test/py/models.user.User.name" --> "models/user.py",
  "test/py/helpers.Formatter.separator" --> "helpers.py",
  -- module-level variables
  "test/py/example.app_name" --> "example.py",
  "test/py/example.version" --> "example.py",
  "test/py/models.base.MODEL_REGISTRY" --> "models/base.py",
  "test/py/models.user.DEFAULT_USER" --> "models/user.py",
  "test/py/helpers.MAX_ITEMS" --> "helpers.py"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

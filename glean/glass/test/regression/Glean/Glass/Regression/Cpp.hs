{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Cpp (main) where

import qualified Glean.Clang.Test.DerivePass as DerivePass
import Test.HUnit

import Data.Bifunctor
import Data.Text (Text)

import qualified Glean.Regression.Driver.DeriveForCodemarkup as Code
import Glean.Glass.Types
import Glean.Glass.Regression.Tests
import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshotGeneric testName testPath driver unitTests
  where
    driver = DerivePass.driver Code.codemarkupDerivePasses
    testName = "glass-regression-cpp"
    testPath = "glean/glass/test/regression/tests/cpp"

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "test.cpp") get
  , testCppFindReferences get
  , testSymbolIdLookup get
  , testCppDescribeSymbolComments get
  , testCppSearchRelated get
  ]

testCppFindReferences :: Getter -> Test
testCppFindReferences get = TestLabel "findReferences" $ TestList [
  "test/cpp//foo/S/.decl" --> [("test.cpp", 1)],
  "test/cpp//foo/bar/T/.decl" --> [("test.cpp", 1)],
  "test/cpp//foo/f/.decl" --> [("test.cpp", 1)],
  "test/cpp//foo/bar/g/.decl" --> [("test.cpp", 1)],
  -- a definition occurrence is find-refereable now
  "test/cpp//h" --> [("test.cpp", 1)]
  ]
  where
    (-->) :: Text -> [(Text,Int)] -> Test
    sym --> expected =
      testFindReferences
        (SymbolId sym)
        (map (first Path) expected)
        get

testSymbolIdLookup :: Getter -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  "test/cpp//foo/.decl" --> "test.cpp",
  "test/cpp//foo/f/.decl" --> "test.cpp",
  "test/cpp//foo/S/.decl" --> "test.cpp",
  "test/cpp//foo/bar/.decl" --> "test.cpp",
  "test/cpp//foo/bar/T/.decl" --> "test.cpp",
  "test/cpp//foo/bar/g/.decl" --> "test.cpp"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

testCppDescribeSymbolComments :: Getter -> Test
testCppDescribeSymbolComments get = TestLabel "describeSymbolComments" $
  TestList [
    "test/cpp//foo/f/.decl" --> (13,1)
  ]
  where
    (-->) :: Text -> (Int,Int) -> Test
    sym --> expected =
      testDescribeSymbolComments
        (SymbolId sym)
        expected
        get

testCppSearchRelated :: Getter -> Test
testCppSearchRelated get = TestLabel "searchRelated" $ TestList $
  "test/cpp//foo/.decl" `contains` "test/cpp//foo/S/.decl"
  where
    contains :: Text -> Text -> [Test]
    contains = contains' False
    contains' :: Bool -> Text -> Text -> [Test]
    contains' recurse parent child =
      [ testSearchRelated
          (SymbolId parent)
          recurse
          RelationDirection_Child
          RelationType_Contains
          (SymbolId parent, SymbolId child)
          get
      ]

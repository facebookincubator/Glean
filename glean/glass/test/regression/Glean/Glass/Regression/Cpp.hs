{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Cpp (main) where

import Derive.Lib (DerivePass(..))
import qualified Glean.Clang.Test.DerivePass as DerivePass

import Test.HUnit

import Glean.Regression.Test

import Data.Bifunctor
import Data.Text (Text)

import Glean
import Glean.Util.Some

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = do
  let driver = DerivePass.driver [DeriveTargetUses, DeriveDeclFamilies]
  let name = "glass-regression-cpp"
  mainTestIndexGeneric driver (pure ()) name $ \_ _ _ _ get ->
    TestList
      [ testDocumentSymbolListX (Path "test.cpp") get
      , testCppFindReferences get
      , testSymbolIdLookup get
      , testCppDescribeSymbolComments get
      ]

testCppFindReferences :: IO (Some Backend, Repo) -> Test
testCppFindReferences get = TestLabel "findReferences" $ TestList [
  "test/cpp/foo/S" --> [("test.cpp", 1)],
  "test/cpp/foo/bar/T" --> [("test.cpp", 1)],
  "test/cpp/foo/f" --> [("test.cpp", 1)],
  "test/cpp/foo/bar/g" --> [("test.cpp", 1)],
  "test/cpp/h" --> []
  ]
  where
    (-->) :: Text -> [(Text,Int)] -> Test
    sym --> expected =
      testFindReferences
        (SymbolId sym)
        (map (first Path) expected)
        get

testSymbolIdLookup :: IO (Some Backend, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  "test/cpp/foo" --> "test.cpp",
  "test/cpp/foo/f" --> "test.cpp",
  "test/cpp/foo/S" --> "test.cpp",
  "test/cpp/foo/bar" --> "test.cpp",
  "test/cpp/foo/bar/T" --> "test.cpp",
  "test/cpp/foo/bar/g" --> "test.cpp"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

testCppDescribeSymbolComments :: IO (Some Backend, Repo) -> Test
testCppDescribeSymbolComments get = TestLabel "describeSymbolComments" $
  TestList [
    "test/cpp/foo/f" --> (13,1)
  ]
  where
    (-->) :: Text -> (Int,Int) -> Test
    sym --> expected =
      testDescribeSymbolComments
        (SymbolId sym)
        expected
        get

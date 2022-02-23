{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Hack (main) where

import Data.Bifunctor
import Data.Text (Text)
import Test.HUnit

import Glean
import Glean.Regression.Test
import Glean.Util.Some

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainTestIndexExternal "glass-regression-hack" $ \get -> TestList
  [ testDocumentSymbolListX (Path "www/RefClass.php") get
  , testSymbolIdLookup get
  , testHackFindReferences get
  , testHackDescribeSymbolComments get
  , testHackAnnotations get
  ]

testSymbolIdLookup :: IO (Some Backend, Repo) -> Test
testSymbolIdLookup get = TestLabel "describeSymbol" $ TestList [
  -- class
  "test/php/SourceClass" --> "www/SourceClass.php",
  -- class method
  "test/php/SourceClass/bar" --> "www/SourceClass.php",
  -- class constant
  "test/php/SuperClass/BAZ" --> "www/SuperClass.php",
  -- property
  "test/php/SourceClass/daz" --> "www/SourceClass.php",
  -- class typedef
  "test/php/SourceClass/T" --> "www/SourceClass.php",
  -- interface
  "test/php/SourceInterface" --> "www/SourceInterface.php",
  -- interface method
  "test/php/SourceInterface/foo" --> "www/SourceInterface.php",
  -- trait
  "test/php/SourceTrait" --> "www/SourceTrait.php",
  -- trait method
  "test/php/SourceTrait/quux" --> "www/SourceTrait.php",
  -- global function
  "test/php/corge" --> "www/TopLevel.php",
  -- global constant
  "test/php/WALDO" --> "www/TopLevel.php",
  -- enum decl
  "test/php/Position" --> "www/TopLevel.php",
  -- enum value
  "test/php/Position/Top" --> "www/TopLevel.php"
  ]
  where
    (-->) :: Text -> Text -> Test
    sym --> expected =
      testDescribeSymbolMatchesPath
        (SymbolId sym)
        (Path expected)
        get

testHackFindReferences :: IO (Some Backend, Repo) -> Test
testHackFindReferences get = TestLabel "findReferences" $ TestList [
  "test/php/SourceClass" --> [("www/RefClass.php", 4)],
  "test/php/SuperClass/BAZ" --> [],
  "test/php/SourceClass/daz" --> [("www/RefClass.php",1)],
  "test/php/SourceClass/T" --> [("www/RefClass.php",1)],
  "test/php/SourceTrait" --> [("www/RefClass.php", 1)],
  "test/php/SourceTrait/quux" --> [("www/RefClass.php", 1)],
  "test/php/SourceInterface" --> [("www/RefClass.php", 1)],
  "test/php/corge" --> [("www/SourceClass.php",1),("www/RefClass.php", 4)],
  "test/php/WALDO" --> [("www/RefClass.php", 2), ("www/TopLevel.php",1)],
  "test/php/Position" --> [("www/RefClass.php", 2)],
  "test/php/Position/Right" --> [("www/RefClass.php", 1)]
  ]
  where
    (-->) :: Text -> [(Text,Int)] -> Test
    sym --> expected =
      testFindReferences
        (SymbolId sym)
        (map (first Path) expected)
        get

testHackDescribeSymbolComments :: IO (Some Backend, Repo) -> Test
testHackDescribeSymbolComments get = TestLabel "describeSymbolComments" $
  TestList [
    "test/php/SourceClass" --> (10,1)
  ]
  where
    (-->) :: Text -> (Int,Int) -> Test
    sym --> expected =
      testDescribeSymbolComments
        (SymbolId sym)
        expected
        get

testHackAnnotations :: IO (Some Backend, Repo) -> Test
testHackAnnotations get = TestLabel "annotations" $ TestList [
  "test/php/SourceClass" --> [("TestAnnotation", "TestAnnotation(\"Text\")")]
  ]
  where
    (-->) :: Text -> [(Text, Text)] -> Test
    sym --> expected =
      testDescribeSymbolHasAnnotations
        (SymbolId sym)
        expected
        get

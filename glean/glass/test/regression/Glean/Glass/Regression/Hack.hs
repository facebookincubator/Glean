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

import Glean.Indexer.Hack as Hack
import Glean.Glass.Types
import Glean.Glass.Regression.Tests
import Glean.Glass.Regression.Snapshot as Glass

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer unitTests
  where
    testName = "glass-regression-hack"
    testPath = "glean/glass/test/regression/tests/hack"
    testIndexer = Hack.indexer

unitTests :: Getter -> [Test]
unitTests get =
  [ testDocumentSymbolListX (Path "www/RefClass.php") get
  , testSymbolIdLookup get
  , testHackFindReferences get
  , testHackDescribeSymbolComments get
  , testHackAnnotations get
  , testHackVisibility get
  , testHackSearchRelated get
  ]

testSymbolIdLookup :: Getter -> Test
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

testHackFindReferences :: Getter -> Test
testHackFindReferences get = TestLabel "findReferences" $ TestList [
  "test/php/SourceClass" --> [("www/RefClass.php", 4),("www/SourceClass.php", 1)],
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

testHackDescribeSymbolComments :: Getter -> Test
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

testHackAnnotations :: Getter -> Test
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

testHackVisibility :: Getter -> Test
testHackVisibility get = TestLabel "visibility" $ TestList [
  "test/php/SourceClass/daz" --> Visibility_Public
  ]
  where
    (-->) :: Text -> Visibility -> Test
    sym --> expected =
      testDescribeSymbolHasVisibility
        (SymbolId sym)
        expected
        get

testHackSearchRelated :: Getter -> Test
testHackSearchRelated get = TestLabel "searchRelated" $ TestList $ concat [
  "test/php/RefClass" `extends` "test/php/SourceInterface",
  "test/php/RefClass" `extends` "test/php/SourceTrait",
  "test/php/SubClass" `extends` "test/php/SourceClass",
  "test/php/SourceClass" `extends` "test/php/SuperClass",
  "test/php/SubClass" `extendsRec` "test/php/SuperClass",
  "test/php/SourceInterface" `contains` "test/php/SourceInterface/foo",
  "test/php/NS" `contains` "test/php/NS/NamespaceClass",
  "test/php/NS/NamespaceClass" `contains` "test/php/NS/NamespaceClass/class_function",
  "test/php/NS" `containsRec` "test/php/NS/NamespaceClass/class_function"
  ]
  where
    extendsRec :: Text -> Text -> [Test]
    extendsRec = extends' True
    extends :: Text -> Text -> [Test]
    extends = extends' False
    extends' :: Bool -> Text -> Text -> [Test]
    extends' recurse child parent =
      [ testSearchRelated
          (SymbolId parent)
          recurse
          RelationDirection_Child
          RelationType_Extends
          (SymbolId parent, SymbolId child)
          get
      , testSearchRelated
          (SymbolId child)
          recurse
          RelationDirection_Parent
          RelationType_Extends
          (SymbolId parent, SymbolId child)
          get
      ]
    containsRec :: Text -> Text -> [Test]
    containsRec = contains' True
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

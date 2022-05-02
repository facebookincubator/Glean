{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.ArrayTest (main) where

import Data.Text (Text)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Types

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "array" $ angleArray id
  , TestLabel "array/page" $ angleArray (limit 1)
  ]

angleArray :: (forall a . Query a -> Query a) -> Test
angleArray modify = TestList
  [ TestLabel "generators" $ angleArrayGenerator modify
  , TestLabel "prefix" $ angleArrayPrefix modify
  ]

angleArrayGenerator :: (forall a . Query a -> Query a) -> Test
angleArrayGenerator modify = dbTestCase $ \env repo -> do
  -- fetch all elements of an array
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { array_of_pred = Arr };
      Arr [..]
    |]
  print results
  assertEqual "angle - array generator 1" 2 (length results)

  -- match on elements of an array
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
      P = glean.test.Predicate { array_of_nat = Arr };
      3 = Arr [..]  # any P with a 3 in array_of_nat
    |]
  print results
  assertEqual "angle - array generator 2" 1 (length results)

  -- test that a generator on the left gets compiled correctly
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
      P = glean.test.Predicate { array_of_pred = Arr };
      { nat = 42 } = Arr [..]  # any P with a nat = 4
    |]
  print results
  assertEqual "angle - array generator 3" 1 (length results)

  results <- runQuery_ env repo $ modify $
    angleData @(Text, Nat)
    [s|
      [ { "a",1 }, { "b",2 } ] [..]
    |]
  print results
  assertEqual "angle - array generator 4"
    [ ("a", Nat 1), ("b", Nat 2) ] results


angleArrayPrefix :: (forall a . Query a -> Query a) -> Test
angleArrayPrefix modify = TestList
  [ TestLabel "nat" $ TestList
    [ TestLabel "nested" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_nat = [3,4, ..] }
          |]
        assertEqual "angle - array prefix" 1 (length results)
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_nat = [3, ..] }
          |]
        assertEqual "angle - array prefix" 1 (length results)
    , TestLabel "flat" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_nat = A };
            [3, ..] = A
          |]
        assertEqual "angle - array prefix" 1 (length results)
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_nat = A };
            [3,4, ..] = A
          |]
        assertEqual "angle - array prefix" 1 (length results)
    ]
  , TestLabel "pred" $ TestList
    [ TestLabel "nested" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_pred = [glean.test.Predicate _, ..] }
          |]
        assertEqual "angle - array prefix" 2 (length results)
    , TestLabel "flat" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_pred = A };
            [_, ..] = A
          |]
        assertEqual "angle - array prefix" 2 (length results)
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_pred = A };
            [glean.test.Predicate _, ..] = A
          |]
        assertEqual "angle - array prefix" 2 (length results)
    ]
  , TestLabel "string" $ TestList
    [ TestLabel "nested" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_string = ["abba","baba", ..] }
          |]
        assertEqual "angle - array prefix" 2 (length results)
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_string = ["abba", ..] }
          |]
        assertEqual "angle - array prefix" 2 (length results)
    ]
  , TestLabel "nat and string" $ TestList
    [ TestLabel "nested" $ dbTestCase $ \env repo -> do
        results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
          [s|
            P where
            P = glean.test.Predicate { array_of_nat = [3, ..], array_of_string = ["abba", ..] }
          |]
        assertEqual "angle - array prefix" 1 (length results)
    ]
  ]

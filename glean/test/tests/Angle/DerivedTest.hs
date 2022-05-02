{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.DerivedTest (main) where

import Data.List
import Data.Text (Text)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed hiding (end)

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "derived" $ derivedTest id
  , TestLabel "derived/page" $ derivedTest (limit 1)
  ]

-- | Tests for derived predicates using Angle syntax. See
-- JSONQueryTest.jsonDerivedTest for the Thrift-query-type versions of
-- these tests.
derivedTest :: (forall a . Query a -> Query a) -> Test
derivedTest modify = dbTestCase $ \env repo -> do
  -- Several tests with the trivial derived predicate: IsThree ::= 3
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      glean.test.IsThree _
    |]
  print results
  assertEqual "derived - IsThree 1" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      Y where Y = glean.test.IsThree X; X = 4
    |]
  print results
  assertEqual "derived - IsThree 2" 0 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      glean.test.IsThree 3
    |]
  print results
  assertEqual "derived - IsThree 3" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      glean.test.IsThree 4
    |]
  print results
  assertEqual "derived - IsThree 4" 0 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      X = 3; glean.test.IsThree X
    |]
  print results
  assertEqual "derived - IsThree 5" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsThree
    [s|
      X = 4; glean.test.IsThree X
    |]
  print results
  assertEqual "derived - IsThree 6" 0 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.IsGlean
    [s|
      glean.test.IsGlean "gl"..
    |]
  print results
  assertEqual "derived - IsGlean 1" 1 (length results)

  -- Tests with RevStringPair
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
      glean.test.RevStringPair _
    |]
  print results
  assertBool "derived 1" $ length results == 6

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
      glean.test.RevStringPair {"a",_}
    |]
  print results
  assertBool "derived 2" $ length results == 1

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
      glean.test.RevStringPair {_,"a"}
    |]
  print results
  assertBool "derived 3" $ case results of
    [one] | Just key <- getFactKey one ->
      "a" == Glean.Test.revStringPair_key_snd key
    _otherwise -> False

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
      glean.test.RevStringPair {X,X}
    |]
  print results
  assertBool "derived 4" $ length results == 2

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevRevStringPair
    [s|
      glean.test.RevRevStringPair _
    |]
  print results
  assertBool "derived 5" $ length results == 6

  -- The Ids of derived fact better be distinct, even when we page the results
  assertBool "derived 5: distinct Ids" $ length (nub (map getId results)) == 6

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevRevStringPair
    [s|
      glean.test.RevRevStringPair {"a",_}
    |]
  print results
  assertBool "derived 6" $ case results of
    [one] | Just key <- getFactKey one ->
      "a" == Glean.Test.revRevStringPair_key_fst key
    _otherwise -> False

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.ReflStringPair
    [s|
      glean.test.ReflStringPair "x"..
    |]
  print results
  assertBool "derived 7" $ length results == 1

  -- check that we can recursively expand the results of a derived query
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.DualStringPair
    [s|
      glean.test.DualStringPair _
    |]
  print results
  assertBool "derived 8" $ (==4) $ length
    [ fst
    | Just key <- map getFactKey results
    , Just fst <- [getFactKey $ Glean.Test.dualStringPair_key_fst key] ]

  -- a pattern on the LHS of a statement that arises due to
  -- substitution of a derived predicate
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.DualStringPair
    [s|
      P = glean.test.StringPair _; glean.test.DualStringPair {P,_}
    |]
  print results
  assertBool "derived 10" $ (==4) $ length results

  -- More complicated matching
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.RevStringPair
    [s|
      glean.test.StringPair X;
      glean.test.RevStringPair X
      # generates all RevStringPairs and matches each one against X
    |]
  print results
  assertEqual "derived match filter" 4 (length results)

  -- deriving a key-only predicate from a key-value predicate
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue
    [s|
      glean.test.DerivedKeyValue _
    |]
  print results
  assertEqual "angle - derived key/value 1" 3 (length results)

  -- match on the key
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue
    [s|
      glean.test.DerivedKeyValue { kstring = "foo" }
    |]
  print results
  assertEqual "angle - derived key/value 2" 2 (length results)

  -- match on the value
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue
    [s|
      glean.test.DerivedKeyValue { vnat = 5 }
    |]
  print results
  assertEqual "angle - derived key/value 3" 1 (length results)

  -- deriving a key-value predicate from a key-only predicate which is
  -- derived from a key-value predicate
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue2
    [s|
      glean.test.DerivedKeyValue2 _
    |]
  print results
  assertEqual "angle - derived key/value 4" 3 (length results)

  -- match on the key
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue2
    [s|
      glean.test.DerivedKeyValue2 { kstring = "foo" } -> _
    |]
  print results
  assertEqual "angle - derived key/value 5" 2 (length results)

  -- match on the value
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DerivedKeyValue2
    [s|
      glean.test.DerivedKeyValue2 _ -> { vnat = 5 }
    |]
  print results
  assertEqual "angle - derived key/value 6" 1 (length results)

  -- a derived predicate defined as a record, not a tuple
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPairRec
    [s|
      glean.test.RevStringPairRec {_, "a"}
    |]
  print results
  assertBool "angle - derived with record" $ length results == 1

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPairRec
    [s|
      glean.test.RevStringPairRec { snd = "a" }
    |]
  print results
  assertBool "angle - derived with record 2" $ length results == 1

  -- tests that require inserting derived facts into the temporary environment
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
      X where X = glean.test.RevStringPair _
    |]
  print results
  assertEqual "angle - tmp environment" 6 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPair
    [s|
       P where
       P = glean.test.RevStringPair {A,"a"};
       glean.test.RevStringPair {_,A}
    |]
  print results
  assertEqual "angle - tmp environment 2" 1 (length results)

  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.RevStringPairs
    [s|
      glean.test.RevStringPairs { x = "a" }
    |]
  print results
  assertEqual "angle - tmp environment 3" 1 (length results)

  -- non-bindable or-pattern in the key of a derived predicate. This
  -- is a tricky case to handle: the query engine will have to expand
  -- and substitute the derived predicate twice.
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.RevStringPairs
    [s|
      glean.test.RevStringPairs { x = "a".. | "b".. }
    |]
  print results
  assertEqual "angle - derived + or pattern" 2 (length results)

  results <- runQuery_ env repo $ modify $ recursive $
    angleData @[Text]
    [s|
      [X1, X2] where glean.test.RevStringPair{X1, X2};
    |]
  print results
  assertEqual "angle - derived returning array" 6 (length results)

  -- sum type in the derived predicate head
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.MatchOneAlt
    [s|
      glean.test.MatchOneAlt { { tue = 3 }, _ }
    |]
  print results
  assertEqual "angle - MatchOneAlt 1" 1 (length results)

  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.MatchOneAlt
    [s|
      glean.test.MatchOneAlt { { mon = 3 }, _ }
    |]
  print results
  assertEqual "angle - MatchOneAlt 2" 0 (length results)

  -- Test for a bug
  r <- runQuery_ env repo $ modify $ angle @Glean.Test.LeftOr $
    [s|
      glean.test.LeftOr { x = "c".. }
    |]
  print r
  assertEqual "angle - LeftOr" 1 (length r)

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.LeftOr2 $
    [s|
      glean.test.LeftOr2 { x = "c".. }
    |]
  print r
  assertEqual "angle - LeftOr2" 1 (length r)

  -- Test for another bug
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.RevStringPairs
    [s|
      glean.test.RevStringPairs { X, {_, X} }
    |]
  print results
  assertEqual "angle - RevStringPairs " 6 (length results)

  -- flipping statements
  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Unbound2
    [s|
      glean.test.Unbound2 { _, "e" }
    |]
  print r
  assertEqual "angle - flip statement 1" 1 (length r)

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Unbound2
    [s|
      glean.test.Unbound2 { "e", _ }
    |]
  print r
  assertEqual "angle - flip statement 2" 1 (length r)

  -- test for repeated variables in the derived predicate
  r <- runQuery_ env repo $ modify $ angle @Glean.Test.SameString
    [s|
      glean.test.SameString { "a", "b" }
    |]
  print r
  assertEqual "angle - repeated vars 1" 0 (length r)

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.SameString
    [s|
      glean.test.SameString { "a", "a" }
    |]
  print r
  assertEqual "angle - repeated vars 2" 1 (length r)

  -- unification
  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Unbound2
    [s|
      glean.test.Unbound2 _
    |]
  print r
  assertEqual "angle - unification" 2 (length r)

  -- or-statements and derived predicates
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      N where
        glean.test.RevStringPair { N, _ } |
        glean.test.RevRevStringPair { N, _ }
    |]
  assertEqual "angle - or-statements derived" 8 (length (nub r))

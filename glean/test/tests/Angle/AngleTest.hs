{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.AngleTest (main) where

import Control.Exception
import Data.Default
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.Builtin.Types as Builtin
import qualified Glean.Schema.Sys.Types as Sys
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed hiding (end)
import Glean.Types

import TestData
import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "angle" $ angleTest id
  , TestLabel "angle/page" $ angleTest (limit 1)
  , TestLabel "angleDot" angleDotTest
  , TestLabel "angleNegation" $ angleNegationTest id
  , TestLabel "angleNegation/page" $ angleNegationTest (limit 1)
  , TestLabel "angleIfThenElse" $ angleIfThenElse id
  , TestLabel "angleIfThenElse/page" $ angleIfThenElse (limit 1)
  , TestLabel "angleTypeTest" angleTypeTest
  , TestLabel "angleSet" angleSetTest
  ]

ignorePredK :: Glean.Test.KitchenSink -> Glean.Test.KitchenSink
ignorePredK k = k {
  Glean.Test.kitchenSink_pred = def,
  Glean.Test.kitchenSink_sum_ = def }

angleTest :: (forall a . Query a -> Query a) -> Test
angleTest modify = dbTestCase $ \env repo -> do
  -- match zero results
  results <- runQuery_ env repo $ modify $ angle @Sys.Blob
    [s|
      sys.Blob "nomatch"
    |]
  print results
  assertEqual "no results" results []

  -- match all results (one)
  results <- runQuery_ env repo $ modify $ angle @Sys.Blob
    [s|
      sys.Blob "hello"
    |]
  print results
  sysBlobId <-
    case results of
      [Sys.Blob{..}] -> return blob_id
      _ -> assertFailure "angle - sys.Blob"

  -- query that matches everything
  results <- runQuery_ env repo $ modify $ angle @Sys.Blob
    [s|
      sys.Blob _
    |]
  print results
  assertBool "angle - sys.Blob match all" $ length results == 2

  -- match one result of many
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
       glean.test.Predicate { named_sum_ = { tue = 37 } }
    |]
  print results
  case results of
    [Glean.Test.Predicate{Glean.Test.predicate_key = Just k}] ->
      assertEqual "angle - glean.test.Predicate 1"
        (ignorePredK kitchenSink1) (ignorePredK k)
    _ -> assertBool "angle - glean.test.Predicate 1" False

  -- match all results (two)
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
       glean.test.Predicate _
    |]
  print results
  assertEqual "angle - glean.test.Predicate 2" 4 (length results)

  -- match one nested pattern
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      Blob = sys.Blob "hello";
      glean.test.Predicate { pred = Blob }
    |]
  print results
  assertBool "angle - glean.test.Predicate nested pattern" $
    let correct Glean.Test.Predicate { predicate_key = Just key }
          = Sys.blob_id (Glean.Test.kitchenSink_pred key) == sysBlobId
        correct _ = False
    in not (null results) && all correct results

  -- match a maybe that's missing
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { maybe_ = nothing }
    |]
  print results
  assertEqual "angle - maybe = nothing" 3 (length results)

  -- match a maybe that's present
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { maybe_ = { just = _ } }
    |]
  print results
  assertEqual "angle - maybe = just" 1 (length results)

  -- match multiple alternatives of a sum type.
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      Blob = sys.Blob "hello";
      T1 = glean.test.Predicate { sum_ = { d = Blob } };
      T1 | glean.test.Predicate { sum_ = { c = T1 } }
    |]
  print results
  assertBool "angle - sum - multiple alts" $
    case results of
      [_one, _two, _three, _four] -> True
      _ -> False

  -- sum type: one branch matches nothing
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      ( Blob = sys.Blob "hello";
        glean.test.Predicate { sum_ = { d = Blob } } ) |
      ( X = glean.test.Predicate { sum_ = d } |
             ( Y = glean.test.Predicate { nat = 99 };
               glean.test.Predicate { sum_ = { c = Y } } );
        glean.test.Predicate { sum_ = { c = X } }
      )
    |]
  print results
  assertBool "angle - sum - one branch matches nothing" $
    case results of
      [_one, _two, _three, _four] -> True
      _ -> False

  -- match mutliple alternatives of a sum type, with a refutable pattern
  -- in one alternative.
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { sum_ = d } |
      ( X = glean.test.Predicate { nat = 42 };
        glean.test.Predicate { sum_ = { c = X } } )
    |]
  assertBool "angle - sum - multiple alts (refutable)" $
    case results of
      [_one, _two, _three] -> True
      _ -> False

  -- match an array
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { array_of_nat = [1,2] }
    |]
  assertEqual "angle - array - exact" 1 (length results)

  -- match against a string
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { string_ = "Hello\u0000world!\u0000" }
    |]
  print results
  assertEqual "angle - string" 1 (length results)

  -- escaped characters (matches nothing, just testing parsing)
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { string_ = "!\\\"#$&'()*+,\n" }
    |]
  print results
  assertBool "angle - string - escaped characters" $
    null results

  -- nested sum types (matches nothing, just testing parsing/typechecking)
  results <- runQuery_ env repo $ modify $ angle @Cxx.TargetUses
    [s|
      N = cxx1.Name "initFacebook";
      F = cxx1.FunctionName { name = N };
      Q = cxx1.FunctionQName { name = F };
      D = cxx1.FunctionDeclaration { name = Q };
      cxx1.TargetUses { target = { declaration = { function_ = D }}, file = _ }
    |]
  assertBool "angle - nested sums" $ null results

  -- support for patterns on the LHS of a statement
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.StringPair
    [s|
      P where P = glean.test.StringPair X; {_, "x"..} = X
    |]
  print results
  assertEqual "angle - lhs patterns" 2 (length results)

  -- key/value query
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.KeyValue
    [s|
      glean.test.KeyValue _ -> {24,_}
    |]
  print results
  assertEqual "angle - key/value 1" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.KeyValue
    [s|
      glean.test.KeyValue { kstring = "hello" } -> { vstring = "world" }
    |]
  print results
  assertEqual "angle - key/value 2" 1 (length results)

  -- test matching a variable in the value
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.KeyValue
    [s|
      glean.test.KeyValue {"foo",X} -> {X,_}
    |]
  print results
  assertEqual "angle - key/value 3" 1 (length results)

  -- lhs pattern with a named record type & enum
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
      P = glean.test.Predicate { named_record_ = R, nat = 42 };
      glean.test.Rec { alpha = red } = R
    |]
  print results
  assertEqual "angle - typed pat" 1 (length results)

  -- lhs pattern that doesn't match
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
      P = glean.test.Predicate { named_record_ = R };
      glean.test.Rec { alpha = green } = R
    |]
  print results
  assertEqual "angle - typed pat 2" 0 (length results)


  -- unbound variable which can be resolved by adding a generator
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StringPairBox
    [s|
      glean.test.StringPairBox _
    |]
  print results
  assertEqual "angle - resolvable unbound" 6 $
    (length . nub . map Glean.Test.stringPairBox_key) results

  -- another instance of an unbound variable: wildcard as a query
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StringPair
      [s| _ : glean.test.StringPair |]
  print results
  assertEqual "angle - resolvable unbound 2" 6 $
    (length . nub . map Glean.Test.stringPair_key) results

  -- we should be able to resolve wildcards by adding a fact generator
  -- (query has no results, but we're testing that it doesn't fail with
  -- an unbound variable)
  results <- runQuery_ env repo $ modify $
    angleData @Glean.Test.EdgeSum
    [s|
      X where ( { fst = _ } | { snd = _ } ) = X : glean.test.EdgeSum
    |]
  assertEqual "angle - resolvable wildcard" 0 (length results)

  -- matching bools
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { bool_ = true } |
      glean.test.Predicate { bool_ = false }
    |]
  assertEqual "angle - bool" 4 (length results)

  -- matching enums
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { enum_ = e }
    |]
  assertEqual "angle - enum" 4 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { enum_ = f }
    |]
  assertEqual "angle - enum 2" 0 (length results)

  -- test fact lookups
  results <- runQuery_ env repo $ modify $ angle @Cxx.Name
    [s|
      N where
      cxx1.FunctionName { name = N };  # for each FunctionName { name = N }
      N = cxx1.Name "an"..         # lookup N and pattern match against "an"..
    |]
  print results
  assertEqual "angle - lookup" 2 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.KeyValue
    [s|
      X where
      X = glean.test.KeyValue _;
      # lookup X and match its value
      X = glean.test.KeyValue _ -> { vstring = "bar" }
    |]
  print results
  assertEqual "angle - lookup 2" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      X where
      glean.test.Predicate { sum_ = { c = X } };
      X = glean.test.Predicate _
    |]
  print results
  assertEqual "angle - lookup 3" 2 (length results)

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Node
    [s|
      N where
      E = glean.test.Edge _;
      E = glean.test.Edge { parent = N } # this is a lookup
    |]
  print r
  assertBool "angle - lookup 4" (length r `elem` [3,4])
    -- paging prevents de-duping and gives more results

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
      glean.test.RefRef R;
      R = glean.test.Ref P
    |]
  print r
  assertEqual "angle - lookup 5" 2 (length r)

  -- Test literal fact Ids ($<predicate> <id>,2)
  names <- runQuery_ env repo $ allFacts @Cxx.Name
  let factId x = Text.pack (show (fromFid (idOf (getId x))))

  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
      "$cxx1.Name " <> factId (head names)
  assertEqual "angle - single fact id" 1 (length r)

  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
      "$" <> factId (head names) <> ": cxx1.Name"
  assertEqual "angle - single fact id" 1 (length r)

  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
    "[" <>
      Text.intercalate "," [ "$" <> factId x <> " : cxx1.Name" | x <- names ] <>
    "] [..]"
  assertEqual "angle - array of fact ids" (length names) (length r)

  -- Literal fact ID with the wrong type
  r <- try $ runQuery_ env repo $ modify $ angle @Cxx.FunctionName $
      "$cxx1.FunctionName " <> factId (head names)
  print r
  assertBool "angle - fact id with wrong type" $
    case r of
      Left (SomeException x) -> "fact has the wrong type" `isInfixOf` show x
        -- it's actually a GleanFFIError when running locally,
        -- and probably a Thrift ApplicationError when running
        -- remotely. It should really be a BadQuery though.
      _ -> False

  -- Test literal untyped fact Ids ($<id>)
  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
    "[" <>
      -- we have to help the typechecker by using a typed fact Id for
      -- the first array element.
      "$cxx1.Name " <> factId (head names) <> "," <>
      Text.intercalate "," [ "$" <> factId x | x <- tail names ] <>
    "] [..]"
  assertEqual "angle - array of fact ids 2" (length names) (length r)

  -- Test embedded or-patterns
  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
    [s| cxx1.Name ("ab".. | "bl"..) |]
  print r
  assertEqual "angle - or-pattern 1" 4 (length r)

  -- Test or-pattern on a statement lhs
  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
    [s|
      N where
        N = cxx1.Name ("a"..X);
        ("b".. | "n"..) = X
    |]
  print r
  assertEqual "angle - or-pattern 2" 4 (length r)

  -- Test or-pattern in a query head
  r <- runQuery_ env repo $ modify $ angle @Cxx.Name $
    [s|
      N where
        Y = ("a"..X | "b"..X where cxx1.Name ("a"..X));
        N = cxx1.Name Y
    |]
  print r
  assertEqual "angle - or-pattern 3" 6 (length r)

  -- Test prim.toLower
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      prim.toLower "ABCabc123" |
      # @lint-ignore-every TXT5
      prim.toLower "\u0000\u0001ЖႠΓ"
    |]
  print r
  assertEqual "angle - toLower" [ "abcabc123", "\0\1жⴀγ" ] r

  -- Test prim.relToAbsByteSpans
  r <- runQuery_ env repo $ modify $ angleData @[(Nat, Nat)]
    [s|
      prim.relToAbsByteSpans [] |
      prim.relToAbsByteSpans [{1, 2}] |
      prim.relToAbsByteSpans [{1, 2}, {4, 2}]
    |]
  print r
  assertEqual "angle - relToAbsByteSpans"
    [ [],
      [(Nat 1, Nat 2)],
      [(Nat 1, Nat 2), (Nat 5, Nat 2)]] r

  -- Test prim.unpackByteSpans
  r <- runQuery_ env repo $ modify $ angleData @[(Nat, Nat)]
    [s|
      prim.unpackByteSpans [] |
      prim.unpackByteSpans [{2, [1]}] |
      prim.unpackByteSpans [{2, [1, 4]}] |
      prim.unpackByteSpans [{3, [1, 4]}, {2, [1]}, {3, [2, 4, 3]}]
    |]
  print r
  assertEqual "angle - unpackByteSpans"
    [ [],
      [(Nat 1, Nat 2)],
      [(Nat 1, Nat 2), (Nat 5, Nat 2)],
      [(Nat 1, Nat 3), (Nat 5, Nat 3),
       (Nat 6, Nat 2),
       (Nat 8, Nat 3), (Nat 12, Nat 3), (Nat 15, Nat 3)] ] r

  -- Test numeric comparison primitives
  r <- runQuery_ env repo $ angleData @() "prim.gtNat 2 1"
  print r
  assertEqual "angle - gtNat 2 1" 1 (length r)

  r <- runQuery_ env repo $ angleData @() "1 > 1"
  print r
  assertEqual "angle - 1 > 1" 0 (length r)

  r <- runQuery_ env repo $ angleData @() "prim.geNat 1 1"
  print r
  assertEqual "angle - geNat 1 1" 1 (length r)

  r <- runQuery_ env repo $ angleData @() "1 >= 2"
  print r
  assertEqual "angle - 1 >= 2" 0 (length r)

  r <- runQuery_ env repo $ angleData @() "prim.ltNat 2 1"
  print r
  assertEqual "angle - prim.ltNat 2 1" 0 (length r)

  r <- runQuery_ env repo $ angleData @() "1 < 2"
  print r
  assertEqual "angle - 1 < 2" 1 (length r)

  r <- runQuery_ env repo $ angleData @() "prim.leNat 1 2"
  print r
  assertEqual "angle - prim.leNat 1 2" 1 (length r)

  r <- runQuery_ env repo $ angleData @() "2 <= 1"
  print r
  assertEqual "angle - 2 <= 1" 0 (length r)

  r <- runQuery_ env repo $ angleData @() "prim.neNat 1 2"
  print r
  assertEqual "angle - prim.neNat 1 2" 1 (length r)

  r <- runQuery_ env repo $ angleData @() "1 !== 1"
  print r
  assertEqual "angle - 1 !== 1" 0 (length r)

  r <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
      P where
        P = glean.test.Predicate { nat = N };
        N !== 2
    |]
  print r
  assertEqual "angle - N !== 2 in predicate" 4 (length r)

  r <- runQuery_ env repo $ angleData @() "\"a\" != \"b\""
  print r
  assertEqual "angle - inequality - \"a\" != \"b\"" 1 (length r)

  r <- runQuery_ env repo $ modify $
    angleData @(Glean.Test.KitchenSink, Glean.Test.KitchenSink)
    [s|
      { A, B } where
        glean.test.Predicate A;
        glean.test.Predicate B;
        A != B;
    |]
  print r
  all <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    "glean.test.Predicate _;"
  let uniques = length all
      uniquePairs = uniques * (uniques - 1)
  assertEqual "angle - inequality - type != type" uniquePairs (length r)

  r <- runQuery_ env repo $ angleData @Nat "prim.addNat 23 31"
  print r
  assertEqual "angle - prim.addNat 23 31" [54] (map unNat r)

  r <- runQuery_ env repo $ angleData @Builtin.Unit "1 + 2 + 3 < 4 + 5"
  print r
  assertEqual "angle - 1 + 2 + 3 < 4 + 5" 1 (length r)

  -- Test rename
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      X = "a" | (X where cxx1.Name X)
    |]
  print r
  assertEqual "angle - rename" 12 (length r)

  -- Test for paging with repeated facts. This exposed a bug at one point.
  r <- runQuery_ env repo $ modify $ angle @Cxx.Name
    [s|
      X where
        X = cxx1.Name _;
        glean.test.StringPair _
    |]
  assertEqual "angle - page repeated" (length (nub r)) 11

  -- Testing or-statements
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      N where cxx1.Name N | src.File N
    |]
  assertEqual "angle - or-statements" 11 (length r)

  -- Fix for a bug in fact traversal
  r <- runQuery_ env repo $ modify $ recursive $ angleData @(Nat, Maybe Nat)
    [s|
      { 4, { just = 3 } } : { x : nat, y : maybe nat }
    |]
  assertEqual "angle - traverse bug" 1 (length r)

  -- test for bugs in the handling of {} in the code generator
  r <- runQuery_ env repo $ modify $ recursive $ angleData @()
    [s|
       X where (X = {}:{}) | (X = {}:{}); (X = {}:{}) | (X = {}:{})
    |]
  assertBool "angle - empty tuples" $
    let l = length r in l >= 1 && l <= 4

  r <- runQuery_ env repo $ modify $ angleData @[(Nat, Nat)]
      "prim.zip X X where glean.test.Predicate { nat = 42, array_of_nat = X }"
  print r
  assertEqual "zipping - zipping an array with itself"
    [[(Nat 1, Nat 1), (Nat 2, Nat 2)]] r

  r <- runQuery_ env repo $ modify $ angleData @[Nat]
      "prim.concat X X where glean.test.Predicate { nat = 42, array_of_nat = X }"
  assertEqual "concat - concatenating an array with itself"
    [[Nat 1, Nat 2, Nat 1, Nat 2]] r


angleDotTest :: Test
angleDotTest = dbTestCase $ \env repo -> do

  -- record selection
  r <- runQuery_ env repo $ angleData @Text
    "X.string_ where glean.test.Predicate X; X.nat = 42"
  assertEqual "dot record" ["Hello\0world!\0"] r

  -- select from a predicate type
  r <- runQuery_ env repo $ angleData @Text
    "(glean.test.Predicate _).string_"
  assertEqual "deref predicate" (length r) 4

  -- chain of selections: record, predicate, sum type, record
  r <- runQuery_ env repo $ angleData @Text
    "X.sum_.c?.string_ where glean.test.Predicate X"
  assertEqual "dot record.sum.record" ["Hello\0world!\0", "acca"] r

  -- maybe
  r <- runQuery_ env repo $ angleData @Text
    "X.string_ where (X : glean.test.Predicate).maybe_.just?"
  assertEqual "dot maybe" ["Hello\0world!\0"] r

  -- error: field not found
  r <- try $ runQuery env repo $ angleData @Text
    "X.notfound where glean.test.Predicate X"
  assertBool "field not found" $
    case r of
      Left (BadQuery x) -> "does not contain the field" `Text.isInfixOf` x
      _ -> False

  -- error: using .nat? to select from a record
  r <- try $ runQuery env repo $ angleData @Text
    "X.nat? where glean.test.Predicate X"
  assertBool "not a union" $
    case r of
      Left (BadQuery x) ->
        "expression is a record, use '.nat' not '.nat?'" `Text.isInfixOf` x
      _ -> False

  -- error: using . to select from a union
  r <- try $ runQuery env repo $ angleData @Text
    "X.sum_.c where glean.test.Predicate X"
  assertBool "not a record" $
    case r of
      Left (BadQuery x) ->
        "expression is a union type, use '.c?' not '.c'"
          `Text.isInfixOf` x
      _ -> False

  -- error: type error
  r <- try $ runQuery env repo $ angleData @Text
    "X.string_ where glean.test.Predicate X; X.nat = \"x\""
  print r
  assertBool "not a record" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  -- infer the lhs of the dot
  r <- runQuery env repo $ angle @Glean.Test.Predicate
    "X where X.sum_.c?.nat = 42; X : glean.test.Predicate"
  assertEqual "infer lhs" 1 (length r)

  -- ensure that we catch wrong usage of .field?
  r <- try $ runQuery env repo $ angleData @Text
    "X where X.sum_.c.nat = 42; X : glean.test.Predicate"
  print r
  assertBool "infer lhs error 1" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery env repo $ angleData @Text
    "X where X.sum_.c?.nat? = 42; X : glean.test.Predicate"
  print r
  assertBool "infer lhs error 2" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  r <- runQuery_ env repo $ angleData @Nat
    "X.*.sum_.c?.*.nat where X : glean.test.Predicate"
  print r
  assertEqual "deref 1" 2 (length r)

  r <- runQuery_ env repo $ angleData @Text
    "X.* where X = glean.test.IsGlean _"
  print r
  assertEqual "deref 2" 1 (length r)

  r <- try $ runQuery_ env repo $ angleData @Text "3.*"
  print r
  assertBool "deref error 1" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False


-- if statements
angleIfThenElse :: (forall a . Query a -> Query a) -> Test
angleIfThenElse modify = dbTestCase $ \env repo -> do

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if never : {} then 1 else 2"
  print r
  assertEqual
    "if statement - returns the else branch when matching fails"
    [Nat 2] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if (A = (0 | 1 | 2); A > 0) then A else 2"
  print r
  assertEqual
    "if statement - returns the 'then' branch if any matching succeeds"
    [Nat 1, Nat 2] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if (0 where 0 = 0) then 1 else 2"
  print r
  assertEqual
    "if statement - works when condition has return type"
    [Nat 1] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if (0 = 0) then 1 else 2"
  print r
  assertEqual
    "if statement - works when condition is subquery without return type"
    [Nat 1] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if (0 = 0) then 1 else if (0 = 0) then 2 else 3"
  print r
  assertEqual
    "if statement - works when nested if's condition is subquery without return type"
    [Nat 1] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if glean.test.IsGlean _ then 1 else 2"
  print r
  assertEqual
    "if statement - works when condition is not subquery"
    [Nat 1] r

  r <- runQuery_ env repo $ modify $ angleData @Nat
    "if (A = 1) then A else 2"
  print r
  assertEqual
    "if statement - variables bound in condition are available in 'then' branch"
    [Nat 1] r

  r <- try $ runQuery_ env repo $ angleData @Nat
    "if (A = 1) then A else A+1"
  print r
  assertBool
    "if statement - variables bound in condition are not available in 'else' branch" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A+1 where if (A = 1) then A else 2;
    |]
  print r
  assertBool
    "if statement - variables bound in condition are not available outside of if statement" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A+1 where if 1 then (A = 1) else 2;
    |]
  print r
  assertBool
    "if statement - variables in 'then' branch only are not available outside" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A+1 where if never : {} then 1 else (A = 1) ;
    |]
  print r
  assertBool
    "if statement - variables in 'else' branch only are not available outside" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- runQuery_ env repo $ modify $ angleData @Nat
      "A where if 1 then (A = 1) else (A = 1)"
  print r
  assertEqual
    "if statement - variables bound in both branches are available outside."
    [Nat 1] r

  r <- runQuery_ env repo $ modify $ angleData @(Nat, Nat)
      "B = if (A = 1) then 2 else (A = 2); { A, B }"
  print r
  assertEqual
    "if statement - variables bound in condition and 'else' are available outside."
    [(Nat 1, Nat 2)] r

  -- should be able to rearrange this one so that the variables are
  -- bound in order
  r <- runQuery_ env repo $ modify $ angleData @(Nat, Nat)
      "(X = (Y:nat|2); Y = (7|8); {X,Y}) | {1,2}"
  assertEqual "reordering disjunctions" 5 (length r)


angleNegationTest :: (forall a . Query a -> Query a) -> Test
angleNegationTest modify = dbTestCase $ \env repo -> do
  -- Negation

  -- negating a term fails
  r <- runQuery_ env repo $ modify $ angleData @() "!1"
  print r
  assertEqual "negation - term 1" 0 (length r)

  -- negating the negation of a term succeeds
  r <- runQuery_ env repo $ modify $ angleData @() "!(!1)"
  print r
  assertEqual "negation - term 2" 1 (length r)

  -- negating a false statement succeeds
  r <- runQuery_ env repo $ modify $ angleData @() "!(1 = 2)"
  print r
  assertEqual "negation - term 3" 1 (length r)

  -- a negated subquery has type unit
  r <- runQuery_ env repo $ modify $ angleData @() "A = !(1 = 2); A"
  print r
  assertEqual "negation - term 3" 1 (length r)

  -- negated queries do not bind variables to the parent scope
  r <- try $ runQuery_ env repo $ modify $ angleData @()
    [s|
      A+1 where !(A = 0);
    |]
  print r
  assertBool "negation - scope" $
    case r of
      Left (SomeException x) ->
        "One or more variables were not bound anywhere" `isInfixOf` show x
      _ -> False

  -- negated queries do constrain the type of variables in the parent scope
  r <- try $ runQuery_ env repo $ modify $ angleData @Nat
    [s|
       A where
        !(A = glean.test.IsGlean "not-glean");
        A = 2;
    |]
  print r
  assertBool "negation - scope 2" $
    case r of
      Left (SomeException x) -> "type error" `isInfixOf` show x
      _ -> False

  -- variables bound before are available
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
       A where
        A = "not-glean";
        !(glean.test.IsGlean A);
    |]
  print r
  assertEqual "negation - scope 3" 1 (length r)

  -- variables can be local to the negated subquery
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
       A where
        A = "glean";
        !(!(glean.test.IsGlean B; V = [B]; A = V[..]));
    |]
  print r
  assertEqual "negation - scope 4" 1 (length r)

  -- variables local to earlier non-overlapping scopes with
  -- the same name as negation variables do not interfere.
  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      !(A = 1; A = 2); (A = "A") | "B"
    |]
  print r
  assertEqual "negation - scope 5" 2 (length r)

  r <- runQuery_ env repo $ modify $ angleData @Text
    [s|
      !(A = 1; (A > 2 | A < 0));
      "A"
    |]
  print r
  assertEqual "negation - scope 6" 1 (length r)

  -- a negated query's head is replaced with {}
  r <- runQuery_ env repo $ modify $ angleData @Nat
    [s|
        {} = !(A where A = 1; A = 2;);
        1
    |]
  print r
  assertEqual "negation -  3" 1 (length r)


-- type checking
angleTypeTest :: Test
angleTypeTest = dbTestCase $ \env repo -> do
    -- Test for correct handling of maybe, bool, and enums in the type checker
  r <- runQuery_ env repo $ recursive $ angleData @Nat
    [s|
      true = true;
      true : bool = true;
      true = true : bool;
      true : bool = true : bool;
      nothing = nothing : maybe nat;
      nothing : maybe nat = nothing : maybe nat;
      { just = 3 } : maybe nat = { just = 3 } : maybe nat;
      { just = 3 } = { just = 3 } : maybe nat;
      { just = 3 } = { just = 3 };
      mon = mon : glean.test.Sum;
      mon : glean.test.Sum = mon : glean.test.Sum;
      3
    |]
  assertEqual "angle - eqType maybe" 1 (length r)

  -- Test that records are not compared structurally
  r <- try $ runQuery_ env repo $ angleData @Nat
    [s| A : { a : nat, b : nat } = { c = 1, d = 1 } : { c : nat, d : nat };
        { N, _ } = A;
        N
    |]
  print r
  assertBool
    "eqType - field versions matter when comparing record types" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  -- X = Y and Y = X should typecheck in the same way
  r <- runQuery_ env repo $ angleData @(Nat,Text)
    [s|
      {X,Y} where X = 1; "a" = Y
    |]
  assertEqual "angle - statement L/R" 1 (length r)

  r <- runQuery_ env repo $ angleData @Nat
    [s|
      Y where X = { nat = Y }; glean.test.Predicate X
    |]
  assertEqual "angle - inference 1" 3 (length r)

  r <- runQuery_ env repo $ angleData @Text
    [s|
      Y where
        X = { sum_ = { c = { string_ = Y }}};
        X = { nat = 0 };
        glean.test.Predicate X
    |]
  print r
  assertEqual "angle - inference 2" 2 (length r)

  r <- runQuery_ env repo $ angleData @Nat
    [s|
        X.nat where
        X : glean.test.Predicate;
        X.sum_.c?.string_ = "acca"
    |]
  print r
  assertEqual "angle - inference 3" 1 (length r)

  r <- runQuery_ env repo $ angle @Glean.Test.Tree
    [s|
      Z where
        X : glean.test.Tree;
        Y = X.left.just?;
        Z = Y.right.just?;
        Z.node = { label = "d" }
    |]
  print r
  assertEqual "angle - inference 4" 1 (length r)

  r <- runQuery_ env repo $ angleData @Text
    [s|
      X.a.b.label where
        X = { a = Y };
        Y = { b = { label = "a" }};
        X : { a: { b: glean.test.Node }}
    |]
  print r
  assertEqual "angle - inference 5" 1 (length r)

  -- order of statements shouldn't matter
  r <- runQuery_ env repo $ angleData @Text
    [s|
      X.a.b.label where
        Y = { b = { label = "a" }};
        X = { a = Y };
        X : { a: { b: glean.test.Node }}
    |]
  print r
  assertEqual "angle - inference 6" 1 (length r)

  -- default unbound hasTy
  r <- runQuery_ env repo $ angleData @Text
    [s|
      "ok" where {} = {}
    |]
  print r
  assertEqual "angle - inference 7" 1 (length r)

  r <- runQuery_ env repo $ angleData @Text
    [s|
      "ok" where {a=2} = Y; Y.a?=2
    |]
  print r
  assertEqual "angle - inference 8" 1 (length r)

angleSetTest :: Test
angleSetTest = dbTestCase $ \env repo -> do
  r <- runQuery_ env repo $ angleData @Glean.Test.Predicate
    [s| glean.test.Predicate { set_of_string = all ("bepa" | "apa") } |]
  print r
  assertEqual "angle - set matching" 2 (length r)

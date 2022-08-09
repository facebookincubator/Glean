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
  ]

ignorePredK :: Glean.Test.KitchenSink_1 -> Glean.Test.KitchenSink_1
ignorePredK k = k { Glean.Test.kitchenSink_1_pred = def }

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
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
       glean.test.Predicate.1 { named_sum_ = { tue = 37 } }
    |]
  print results
  assertBool "angle - glean.test.Predicate 1" $
    case results of
      [Glean.Test.Predicate_1{Glean.Test.predicate_1_key = Just k}] ->
        ignorePredK k == ignorePredK kitchenSink1
      _ -> False

  -- match all results (two)
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
       glean.test.Predicate.1 _
    |]
  print results
  assertBool "angle - glean.test.Predicate 2" $
    case results of
      [_, _] -> True
      _ -> False

  -- match one nested pattern
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      Blob = sys.Blob "hello";
      glean.test.Predicate.1 { pred = Blob }
    |]
  print results
  assertBool "angle - glean.test.Predicate nested pattern" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_id (Glean.Test.kitchenSink_1_pred key) == sysBlobId ->
          True
      _ -> False

  -- match a maybe that's missing
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate.1 { maybe_ = nothing }
    |]
  print results
  assertBool "angle - maybe = nothing" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_id (Glean.Test.kitchenSink_1_pred key) == sysBlobId ->
          True
      _ -> False

  -- match a maybe that's present
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate.1 { maybe_ = { just = _ } }
    |]
  print results
  assertBool "angle - maybe = just" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_id (Glean.Test.kitchenSink_1_pred key) /= sysBlobId -> True
      _ -> False

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
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate.1 { array_of_nat = [99,98] }
    |]
  assertBool "angle - array - exact" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_id (Glean.Test.kitchenSink_1_pred key) /= sysBlobId -> True
      _ -> False

  -- match against a string
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate.1 { string_ = "Hello\u0000world!\u0000" }
    |]
  print results
  assertBool "angle - string" $
    length results == 1

  -- escaped characters (matches nothing, just testing parsing)
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate.1 { string_ = "!\\\"#$&'()*+,\n" }
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
      !(A = glean.test.IsGlean "not-glean");
      A;
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
      Left (SomeException x) -> "type mismatch for variable" `isInfixOf` show x
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

  -- a negated query's head is replaced with {}
  r <- runQuery_ env repo $ modify $ angleData @Nat
    [s|
        {} = !(A where A = 1; A = 2;);
        1
    |]
  print r
  assertEqual "negation -  3" 1 (length r)

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

  -- Test for correct handling of maybe, bool, and enums in the type checker
  r <- runQuery_ env repo $ modify $ recursive $ angleData @Nat
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

  -- test for bugs in the handling of {} in the code generator
  r <- runQuery_ env repo $ modify $ recursive $ angleData @()
    [s|
       X where (X = {}:{}) | (X = {}:{}); (X = {}:{}) | (X = {}:{})
    |]
  assertBool "angle - empty tuples" $
    let l = length r in l >= 1 && l <= 4

  -- if statements

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
    "if (A = 1) then A else A"
  print r
  assertBool
    "if statement - variables bound in condition are not available in 'else' branch" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A where if (A = 1) then A else 2;
    |]
  print r
  assertBool
    "if statement - variables bound in condition are not available outside of if statement" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A where if 1 then (A = 1) else 2;
    |]
  print r
  assertBool
    "if statement - variables in 'then' branch only are not available outside" $
    case r of
      Left (BadQuery x) -> "not bound anywhere: A" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat
    [s|
      A where if never : {} then 1 else (A = 1) ;
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

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module AngleTest (main) where

import Control.Exception
import Data.Default
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Backend as Backend
import Glean.Init
import Glean.Query.Angle as Angle
import Glean.Query.Thrift as Thrift
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.Builtin.Types as Builtin
import qualified Glean.Schema.Sys.Types as Sys
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed hiding (end)
import Glean.Types

import TestData
import TestDB


ignorePredK :: Glean.Test.KitchenSink_1 -> Glean.Test.KitchenSink_1
ignorePredK k = k { Glean.Test.kitchenSink_1_pred = def }

-- No need to re-run the error tests with paging, so we separate them out.
angleErrorTests  :: Test
angleErrorTests = dbTestCase $ \env repo -> do
  -- sum type: error to give multiple alternatives
  results <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { sum_ = { d = _, c = _ } }
    |]
  print results
  assertBool "angle - sum - too many alts" $
    case results of
      Left (BadQuery s) -> "sum type" `Text.isInfixOf` s
      _ -> False

  -- check that we get an error if there's a version mismatch between
  -- query and call site.
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate { }
    |]
  assertBool "angle - type error" $
    case r of
      Left BadQuery{} -> True
      _ -> False

  -- type error when using a type annotation on the lhs
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate_1
    [s|
      P where
      P = glean.test.Predicate { array_of_pred = X };
      glean.test.KitchenSink { nat = 42 } = X
    |]
  print r
  assertBool "angle - type error 2" $
    case r of
      Left (BadQuery s) -> "type error" `Text.isInfixOf` s
      _ -> False

  -- using the wrong predicate name is an error in a nested query
  r <- try $ runQuery_ env repo $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = sys.Blob "an".. }
    |]
  print r
  assertBool "angle - nested 3" $
    case r of
      Left (BadQuery x) -> "expected type: cxx1.Name" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      N = cxx1.Name "abc";
      N[..]
    |]
  print r
  assertBool "angle - array generator type error" $
    case r of
      Left (BadQuery x) -> "array element generator" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      unknown.predicate "abc"
    |]
  print r
  assertBool "angle - unknown predicate" $
    case r of
      Left (BadQuery x) -> "unknown type or predicate" `Text.isInfixOf` x
      _ -> False

  -- general type error
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      cxx1.Name 3
    |]
  print r
  assertBool "angle - type error" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  -- types of generators in sequence don't match
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      (cxx1.Name "abc") | (cxx1.Type "def")
    |]
  print r
  assertBool "angle - mismatched generator types" $
    case r of
      Left (BadQuery x) ->
        "expected type: cxx1.Name" `Text.isInfixOf` x
      _ -> False

  -- types of array elements don't match
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      [A,B] where A = cxx1.Name "abc"; B = cxx1.Type "def"
    |]
  print r
  assertBool "angle - mismatched array elements" $
    case r of
      Left (BadQuery x) -> "type mismatch for variable B" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      _ = prim.length (_ : [string]); cxx1.Name "xyz"
    |]
  print r
  assertBool "angle - wildcard in expr" $
    case r of
      Left (BadQuery x) -> "cannot use a wildcard in an expression"
        `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      x = 1; glean.test.Predicate { nat = x }
    |]
  print r
  assertBool "angle - lowercase variable" $
    case r of
      Left (BadQuery x) -> "variable does not begin with an upper-case"
        `Text.isInfixOf` x
      _ -> False


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


  -- Test literal fact Ids ($<predicate> <id>)
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
  r <- try $ runQuery_ env repo $ modify $ angle @Cxx.Type $
      "$cxx1.Type " <> factId (head names)
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


angleArray :: (forall a . Query a -> Query a) -> Test
angleArray modify = dbTestCase $ \env repo -> do
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


angleDSL :: (forall a . Query a -> Query a) -> Test
angleDSL modify = dbTestCase $ \env repo -> do
  results <- runQuery env repo $ modify $ Angle.query $
    predicate @Glean.Test.Predicate $
      rec $
        field @"string_" "acca" $
        field @"nat" (nat 42) $
        field @"array_of_nat" (array [nat 3, nat 4, nat 5]) $
        field @"bool_" false $
      end
  assertEqual "angle - DSL 0" 1 (length results)

  results <- runQuery env repo $ modify $ Angle.query $
    var $ \p -> p `where_`
      [ wild .= predicate @Glean.Test.Predicate (
          rec $
            field @"sum_" (
              alt @"c" (asPredicate p))
          end)
      , p .= predicate @Glean.Test.Predicate (
          rec $
            field @"string_" "abba"
          end)
      ]
  assertEqual "angle - DSL 1" 1 (length results)

  results <- runQuery env repo $ modify $ Angle.query $
    predicate @Glean.Test.Tree $
      rec $
        field @"left" (just wild) $
        field @"right" nothing
      end
  assertEqual "angle - DSL 2" 1 (length results)

  results <- runQuery_ env repo $ modify $ Angle.data_ $
    vars $ \x y ->
      tuple (x,y) `where_` [
        x .= nat 1,
        y .= nat 2
      ]
  assertEqual "angle - DSL 3" [(toNat 1, toNat 2)] results


-- nested patterns
angleNested :: (forall a . Query a -> Query a) -> Test
angleNested modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate_1 $
    [s|
      glean.test.Predicate.1 { pred = "hello" }
    |]
  assertEqual "angle - nested 0" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = cxx1.Name "an".. }
    |]
  print results
  assertEqual "angle - nested 1" 2 (length results)

  -- the predicate name is optional, since the type tells us what it is:
  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "an".. }
    |]
  print results
  assertEqual "angle - nested 2" 2 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DualStringPair
    [s|
      glean.test.DualStringPair {{"a",_},_}
    |]
  print results
  assertEqual "angle - nested 4" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPairs
    [s|
      glean.test.RevStringPairs { x = "a", r = {_,"a"} }
    |]
  -- this test is on slightly shaky ground, because it generates
  -- derived RevStringPair facts in two places (nested in the query,
  -- and again in the body of RevStringPairs), which get de-duped by
  -- the fact Environment in the query. But paging the query breaks
  -- de-duping because we use a new Environment each time we restart
  -- the query, so we can get a different number of results when
  -- executing this query paged vs. unpaged. To see this, remove
  -- the x = "a" part of the pattern above. (TODO)
  print results
  assertEqual "angle - nested 5" 1 (length results)

  -- or-patterns
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.StringPair
    [s|
      glean.test.StringPair {"a" | "b", _}
    |]
  print results
  assertEqual "angle - nested 6" 2 (length results)

  -- nested generator in an expression
  r <- runQuery_ env repo $ angle @Cxx.Name
    [s|
      (cxx1.Name "foo" where _ = "")
    |]
  print r
  assertEqual "angle - nested predicate pattern in expression" 1 (length r)

  -- nested generator with a type signature
  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "an".. : cxx1.Name }
    |]
  print results
  assertEqual "angle - nested type sig" 2 (length results)

  -- we can force a variable to match the key, not the fact ID
  results <- runQuery_ env repo $ modify $ angle @Cxx.Name
    [s|
      cxx1.FunctionName { name = X : string };
      cxx1.Name X
    |]
  print results
  assertEqual "angle - nested type sig 2" 11 (length results)

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


scopingTest :: Test
scopingTest = dbTestCase $ \env repo -> do
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      cxx1.Name X
    |]
  print r
  assertBool "angle - unbound var" $
    case r of
      Left (BadQuery x) -> "mentioned only once: X" `Text.isInfixOf` x
      _ -> False

  -- bind a variable in multiple branches
  r <- runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {N,X} where X = (1 where N = "abc") | (2 where N = "def")
    |]
  print r
  assertEqual "angle - scoping" 2 (length r)

  -- must bind the variable in *all* branches
  r <- try $ runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {N,X} where X = (1 where N = "abc") | 2
    |]
  print r
  assertBool "angle - scoping 2" $ case r of
    Left e@BadQuery{} -> "not bound anywhere: N" `isInfixOf` show e
    _ -> False

  -- it's OK to not bind the variable in every branch of | if it is
  -- bound outside
  r <- runQuery_ env repo $ angleData @Text
    [s|
      N where N = (X : string) | "abc"; X = "a"
    |]
  print r
  assertEqual "angle - scoping 3" 2 (length r)

  -- local variables can have different types in disjoint branches of |
  r <- runQuery_ env repo $ angleData @(Nat, Text)
    [s|
      ({X,Y} where X = 3; Y = "a") | ({Y,X} where Y = 3; X = "a")
    |]
  print r
  assertEqual "angle - scoping 4" 1 (length r)

  -- but this is an error if the variable(s) are visible outside |
  r <- try $ runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {X,Y} where
        {X,Y} = ({X,Y} where X = 3; Y = "a") |
                ({Y,X} where Y = 3; X = "a")
    |]
  print r
  assertBool "angle - scoping 5" $ case r of
    Left e@BadQuery{} -> "type mismatch" `isInfixOf` show e
    _ -> False

  r <- runQuery_ env repo $ Angle.query $
    predicate @Glean.Test.NothingTest wild
  print (r :: [Glean.Test.NothingTest])
  assertEqual "angle - nothingTest" 1 (length r)

-- Test storing derived facts
angleStored :: (forall a . Query a -> Query a) -> Test
angleStored modify = dbTestCaseWritable $ \env repo -> do
  -- initially zero facts
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 0" 0 (length results)

  -- still zero facts since StoredRevStringPair is not derived yet
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 1" 0 (length results)

  -- no facts derived due to the above
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 2" 0 (length results)

  -- compute and store, returns 6 facts
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 3" 6 (length results)

  -- now there should be 6 facts stored in the DB
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 4" 6 (length results)

  -- StoredRevStringPair is now derived
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 5" 1 (length results)

  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 6" 1 (length results)

justKeys :: (forall a . Query a -> Query a) -> Test
justKeys modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ keys $ allFacts @Cxx.Name
  assertEqual "angle - justKeys" 11 (length results)
  assertBool "angle - justKeys" $ "abba" `elem` results

angleDataTest :: (forall a . Query a -> Query a) -> Test
angleDataTest modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ angleData
    [s| { 3, false } : { x : nat, y : bool } |]
  assertEqual "angleData 1" [(Nat 3, False)] results

  results :: [Text] <- runQuery_ env repo $ modify $ angleData
    [s| X where Y = cxx1.Name "ab"..; Y = cxx1.Name X |]
  assertEqual "angleData 2" ["abba", "abcd"] (sort results)


queryStats
  :: forall q backend . (Backend backend)
  => backend
  -> Repo
  -> Query q
  -> IO ([q], Maybe UserQueryStats)

queryStats be repo (Query query decoder) = do
  let
    opts = fromMaybe def (userQuery_options query)
    query' = query
      { userQuery_encodings = [UserQueryEncoding_bin def]
      , userQuery_options = Just
        opts { userQueryOptions_collect_facts_searched = True }
      }
  UserQueryResults{..} <- userQuery be repo query'
  results <- decodeResults userQueryResults_results decoder
  return (results, userQueryResults_stats)


factsSearched
  :: PredicateRef
  -> Map PredicateRef Int64
  -> Maybe UserQueryStats
  -> Maybe Int
factsSearched ref lookupPid maybeStats = do
  pid <- Map.lookup ref lookupPid
  stats <- maybeStats
  searched <- userQueryStats_facts_searched stats
  count <- Map.lookup pid searched
  return (fromIntegral count)

optTest :: Test
optTest = dbTestCase $ \env repo -> do
  si <- getSchemaInfo env repo
  let lookupPid = Map.fromList
        [ (ref,pid) | (pid,ref) <- Map.toList (schemaInfo_predicateIds si) ]

  -- Test unification: this query should be as efficient as simply
  --    cxx1.Name "an"..
  (_, stats) <- queryStats env repo $ angle @Cxx.Name
    [s|
      N where
        {Y,_} = W : {a : string, b : nat};
        N = cxx1.Name X;
        X = Y;
        W = {"an".., 3}
    |]
  assertEqual "opt 1" (Just 2) $
    factsSearched (PredicateRef "cxx1.Name" 1) lookupPid stats

  -- test that unification can optimise A = B | C, unifying A/B and A/C
  (_, stats) <- queryStats env repo $ angle @Cxx.Name
    [s|
      N where
        {"an".., N} =
          {X:string, cxx1.Name X} |
          {"def", cxx1.Name "def"}
    |]
  assertEqual "opt 2" (Just 2) $
    factsSearched (PredicateRef "cxx1.Name" 1) lookupPid stats

  -- test for recursive substitution
  r <- runQuery_ env repo $ angleData @Nat $
    [s|
      X where X = Y : nat; Y = X; X = 3
    |]
  assertEqual "opt 3" [Nat 3] r

  -- test for optimised ordering of nested fact patterns
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      S where
        glean.test.Predicate {
          string_ = "clobber",
          sum_ = { c = { string_ = S }}
        }
    |]
  assertEqual "opt 4" (Just 4) $
    factsSearched (PredicateRef "glean.test.Predicate" 4) lookupPid stats

  -- Test for optimising away unmatchable alternatives.
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      B where
      { "a", B } =
          # this should be optimised away:
        ( { A, B } where glean.test.StringPair { A, B }; A = "b") |
          # leaving this only:
        ( { A, B } where glean.test.StringPair { A, B }; A = "a")
     |]
  -- we can tell the first alternative was optimised away by counting
  -- the number of facts searched for glean.test.StringPair.
  assertEqual "optimise or 1" (Just 1) $
    factsSearched (PredicateRef "glean.test.StringPair" 1) lookupPid stats

  -- Test that unification works properly with literal fact IDs
  result : _ <- runQuery_ env repo $ (allFacts :: Query Glean.Test.StringPair)
  let fid = factId (getId result)
  results <- runQuery_ env repo $ Angle.query $ fid `where_` [ fid .= fid ]
  assertEqual "unify fact Id" 1 (length results)


{-
  Test reordering of nested matches using a simple DAG:

      "a"
      / \
    "b" "c"
      \ /
      "d"
-}
reorderTest :: Test
reorderTest = dbTestCase $ \env repo -> do
  si <- getSchemaInfo env repo
  let lookupPid = Map.fromList
        [ (ref,pid) | (pid,ref) <- Map.toList (schemaInfo_predicateIds si) ]

  -- Inner match is not in a prefix position: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "result" where glean.test.Tree { "b", _, { just = { "d", _, _ } } }
    |]
  assertEqual "reorder nested 1" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats

  -- Inner match is in a prefix position but irrefutable: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      X where glean.test.Tree { "b", { just = { X, _, _ } }, _ }
    |]
  assertEqual "reorder nested 2" Nothing $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats

  -- Inner match is in a prefix position and refutable: do it first
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "result" where glean.test.Tree { "b", { just = { "d", _, _ } }, _ }
    |]
  assertEqual "reorder nested 3" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats

  -- inner match is in a prefix position (conditional on L being
  -- bound, which it is), and refutable, so do it first.
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "d" where
         L = glean.test.Tree { "d", _, _ };
         glean.test.Tree { "b", { just = L }, { just = { "d", _, _ } } }
    |]
  assertEqual "reorder nested 4" (Just 2) $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats

  -- point match in a non-prefix position: do it first
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { sum_ = { d = "hello" } }
    |]
  assertEqual "reorder nested 5" (Just 1) $
    factsSearched (PredicateRef "sys.Blob" 1) lookupPid stats

  -- nested (almost) irrefutable inner match: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      X where
        glean.test.Tree {
          "a",
          _,
          { just = { _, { just = { X, _, _ } }, _ }}
        }
    |]
  assertEqual "reorder nested 6" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats

  -- Nested matches on the rhs of a lookup should become lookups
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Tree
    [s|
      L where
        L = glean.test.Tree { "b", _, _ };
        L = glean.test.Tree { "b", { just = { "d", _, _ }}, _}
          # L is bound, so even though the nested match { "d", _, _ }
          # is in a prefix position and would normally be done first,
          # in this case we want to do it afterwards because it's a lookup
          # and a lookup is always cheaper than a search.
    |]
  assertEqual "reorder nested 7" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 4) lookupPid stats


angleRecExpansion :: (forall a . Query a -> Query a) -> Test
angleRecExpansion modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.TreeToTree
    [s|
      glean.test.TreeToTree { node = "a" }
    |]
  assertBool "recursive expand key" $ case results of
    [ Glean.Test.TreeToTree
      { treeToTree_key =
          Just Glean.Test.Tree
            { tree_key =
              Just Glean.Test.Tree_key
                { tree_key_node = "a"
                , tree_key_left =
                    Just Glean.Test.Tree
                      { tree_key =
                          Just Glean.Test.Tree_key
                            { tree_key_node = "b"
                            , tree_key_right =
                                Just Glean.Test.Tree
                                  { tree_key =
                                      Just Glean.Test.Tree_key
                                        { tree_key_node = "d"}
                                  }
                            }
                      }
                }
            }
      , treeToTree_value =
          Just Glean.Test.Tree
            { tree_key =
              Just Glean.Test.Tree_key
                { tree_key_node = "e"
                , tree_key_left =
                    Just Glean.Test.Tree
                      { tree_key =
                          Just Glean.Test.Tree_key
                            { tree_key_node = "f"
                            , tree_key_right =
                                Just Glean.Test.Tree
                                  { tree_key =
                                      Just Glean.Test.Tree_key
                                        { tree_key_node = "g"}
                                  }
                            }
                      }
                }
            }
      }] -> True
    _ -> False

  results <- runQuery_ env repo $ modify $ recursive $
    angle @Glean.Test.FooToFoo
    [s|
      glean.test.FooToFoo "foo1"
    |]
  assertBool "recursive expand value" $ case results of
    [ Glean.Test.FooToFoo
        { fooToFoo_key =
            Just Glean.Test.Foo
              { foo_key = Just "foo1"
              , foo_value =
                  Just Glean.Test.Bar
                    { bar_key = Just "bar1"
                    , bar_value =
                      Just Glean.Test.Qux  { qux_key = Just "qux1"}
                    }
              }
        , fooToFoo_value =
            Just Glean.Test.Foo
              { foo_key = Just "foo2"
              , foo_value =
                  Just Glean.Test.Bar
                    { bar_key = Just  "bar2"
                    , bar_value =
                      Just Glean.Test.Qux  { qux_key = Just "qux2"}
                    }
              }
        }] -> True
    _ -> False

angleQueryOptions :: Test
angleQueryOptions = dbTestCase $ \env repo -> do
  let omitResults omit (Query query decoder) = Query q decoder
        where
          opts = fromMaybe def (userQuery_options query)
          q = query
            { userQuery_options = Just opts
              { userQueryOptions_omit_results = omit }
            }

      counts (_, Nothing) = error "No query stats"
      counts (results, Just UserQueryStats{..}) =
        ( length results
        , userQueryStats_result_count
        )

  r <- queryStats env repo $ omitResults True $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "ab"..  }
    |]
  assertEqual "queryOptions - omitting results" (counts r) (0, 2)

  r <- queryStats env repo $ omitResults False $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "ab"..  }
    |]
  assertEqual "queryOptions - not omitting results" (counts r) (2, 2)


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "angle" $ angleTest id
  , TestLabel "angle/page" $ angleTest (limit 1)
  , TestLabel "error" angleErrorTests
  , TestLabel "nested" $ angleNested id
  , TestLabel "nested/page" $ angleNested (limit 1)
  , TestLabel "derived" $ derivedTest id
  , TestLabel "derived/page" $ derivedTest (limit 1)
  , TestLabel "stored" $ angleStored id
  , TestLabel "stored/page" $ angleStored (limit 1)
  , TestLabel "array" $ angleArray id
  , TestLabel "array/page" $ angleArray (limit 1)
  , TestLabel "justKeys" $ justKeys id
  , TestLabel "justKeys/page" $ justKeys (limit 1)
  , TestLabel "angleData" $ angleDataTest id
  , TestLabel "angleData/page" $ angleDataTest (limit 1)
  , TestLabel "opt" optTest
  , TestLabel "reorder" reorderTest
  , TestLabel "scoping" scopingTest
  , TestLabel "dsl" $ angleDSL id
  , TestLabel "recExpansion" $ angleRecExpansion id
  , TestLabel "recExpansion/page" $ angleRecExpansion (limit 1)
  , TestLabel "queryOptions" angleQueryOptions
  ]

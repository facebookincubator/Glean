{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module Schema.Evolves (main) where

import Data.ByteString (ByteString)
import Control.Exception
import Data.Bifunctor (first)
import Data.List
import qualified Data.Map as Map
import Data.Text (pack, unpack)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Angle.Types (latestAngleVersion, Type_(..))
import Glean.Database.Schema.Types
import Glean.Init
import qualified Glean.RTS as RTS
import qualified Glean.RTS.Term as RTS
import qualified Glean.RTS.Types as RTS
import Glean.Schema.Util
import Glean.Types as Thrift

import Schema.Lib

schemaEvolves :: Test
schemaEvolves = TestList
  [ TestLabel "cycles are errors" $ TestCase $ do
    -- no cycles created by evolves
    withSchema latestAngleVersion
      [s|
        schema test.1 {}
        schema test.2 {}
        schema test.2 evolves test.1
        schema test.1 evolves test.2
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "throws cycle error" $
        case r of
          Left err -> "cycle" `isInfixOf` show err
          Right _ -> False

  , TestLabel "many may not evolve one" $ TestCase $ do
    -- multiple schemas cannot evolve the same schema
    withSchema latestAngleVersion
      [s|
        schema test.1 {}
        schema test.2 {}
        schema test.3 {}
        schema test.2 evolves test.1
        schema test.3 evolves test.1
        schema all.1 : test.1, test.2, test.3 {}
      |]
      $ \r ->
      assertBool "throws multiple schemas evolve error" $
        case r of
          Left err -> "multiple schemas evolve test.1" `isInfixOf` show err
          Right _ -> False

  , TestLabel "one may evolve many" $ TestCase $ do
    -- one schema can evolve multiple other schemas
    withSchema latestAngleVersion
      [s|
        schema test.1 {}
        schema test.2 {}
        schema test.3 {}
        schema test.3 evolves test.1
        schema test.3 evolves test.2
        schema all.1 : test.1, test.2, test.3 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can add optional field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat }
        }
        schema test.2 {
          predicate P : { a : nat, b: maybe string }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can add defaultable field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat }
        }
        schema test.2 {
          predicate P : { a : nat, b: { c : string, d : bool } }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "cannot remove required field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate R : string
          predicate P : { a : nat, b: R }
        }
        schema test.2 {
          predicate R : string
          predicate P : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "throws error stating missing field" $
        case r of
          Left err ->
            "cannot evolve predicate test.P.2: missing required field: b"
            `isInfixOf` show err
          Right _ -> False

  , TestLabel "can remove optional field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat, b: maybe string }
        }
        schema test.2 {
          predicate P : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can remove defaultable field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat, b: { c: string, d: bool } }
        }
        schema test.2 {
          predicate P : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can change order of fields" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat, b: string }
        }
        schema test.2 {
          predicate P : { b: string, a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

   , TestLabel "can add predicate" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat }
        }
        schema test.2 {
          predicate P : { a : nat }
          predicate Q : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can remove a predicate" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat, b: string }
        }
        schema test.2 {}
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can add option" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : enum { a | b }
        }
        schema test.2 {
          predicate P : enum { a | b | c }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating the schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can add alternative" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat | }
        }
        schema test.2 {
          predicate P : { a : nat | b : string | }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating the schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can remove option" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : enum { a | b | c }
        }
        schema test.2 {
          predicate P : enum { a | b }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False


  , TestLabel "can change order of options" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : enum { a | b }
        }
        schema test.2 {
          predicate P : enum { b | a }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "types are transparent" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          type T = { a : nat }
          predicate P : T
        }
        schema test.2 {
          predicate P : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can evolve field types" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema x.1 {
          predicate P: { a : nat }
        }

        schema y.1 {
          import x.1
          predicate Q: { a : x.P }
        }

        schema x.2 {
          predicate P: { a : nat, b : maybe nat }
        }

        schema y.2 {
          import x.2
          predicate Q: { a : x.P }
        }

        schema x.2 evolves x.1
        schema y.2 evolves y.1
        schema all.1 : x.1, x.2, y.1, y.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "can evolve field types transitively" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema x.1 {
          predicate P: { a : nat }
        }

        schema x.2 {
          predicate P: { a : nat, b : maybe nat }
        }

        schema x.3 {
          predicate P: { a : nat, b : maybe nat, c : maybe nat }
        }

        schema y.1 {
          import x.1
          predicate Q: { a : x.P }
        }

        schema y.2 {
          import x.3
          predicate Q: { a : x.P }
        }

        schema x.2 evolves x.1
        schema x.3 evolves x.2
        schema y.2 evolves y.1
        schema all.1 : x.1, x.2, x.3, y.1, y.2 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "handles type imports" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema base.1 {
          predicate P : nat
          type T = { a : P }
        }

        schema x.2  {
          import base.1
          predicate Q : { x: base.T }
        }

        schema x.3 : x.2 {
          import base.1
          predicate Q : { x: base.T, y: maybe base.T }
        }

        schema x.3 evolves x.2
        schema all.1 : base.1, x.2, x.3 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False

  , TestLabel "re-exported predicates" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema x.1 {
          predicate P : { x : nat }
          predicate Q : { x : P }
        }

        schema x.2 : x.1  {
        }
        schema x.2 evolves x.1

        schema x.3 {
          predicate P : { x: nat, y: nat }
          predicate Q : { x: P }
        }
        schema x.3 evolves x.2
        schema all.1 : x.1, x.2, x.3 {}
      |]
      $ \r ->
      assertBool "succeeds creating schema" $
        case r of
          Right _ -> True
          Left _ -> False
  ]

schemaEvolvesTransformations :: Test
schemaEvolvesTransformations =
  let nothing = RTS.Alt 0 unit
      unit = RTS.Tuple []
  in
  TestList
  [ TestLabel "backcompat - remove optional field" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : nat }
        }
        schema x.2 {
          predicate P: { a : nat, b: maybe string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 2, "b": "val" } }|] ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "backcompat - fill optional field's default value" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : nat, b: maybe string }
        }
        schema x.2 {
          predicate P: { a : nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 2 } }|] ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "forwardcompat - maps new optional field to default value" $
    TestCase $
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : string }
        }
        schema x.2 {
          predicate P : { a : string, b : maybe string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": "A" } }|]
          ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content" [RTS.Tuple [RTS.String "A", nothing]] facts

  , TestLabel "forwardcompat - matches new field against default value" $
    TestCase $
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : string }
        }
        schema x.2 {
          predicate P : { a : string, b : maybe string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": "A" } }|]
          , [s|{ "key": { "a": "B" } }|]
          ]
      ]
      -- should match { "A", nothing}, but not { "B", { just = "A" }}
      [s| x.P.2 { "A", nothing } |
          x.P.2 { "B", { just = "A" } }
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content" [RTS.Tuple [RTS.String "A", nothing]] facts

  , TestLabel "forwardcompat - binds new field's default value" $
    TestCase $
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : string }
        }
        schema x.2 {
          predicate P : { a : string, b : maybe string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": "A" } }|]
          ]
      ]
      [s| X where x.P.2 { _, X } |]
      $ \_ response _ -> do
        facts <- decodeResultsAsTy (MaybeTy StringTy) response
        assertEqual "result content" [nothing] facts

  , TestLabel "backcompat - change field order" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : string, b: nat }
        }
        schema x.2 {
          predicate P: { b: nat, a : string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": "one1", "b": 16 } }|]
          , [s|{ "key": { "a": "one2", "b": 32 } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "forwardcompat - change field order" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : string, b: nat }
        }
        schema x.2 {
          predicate P: { b: nat, a : string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "b": 16, "a": "one1" } }|]
          , [s|{ "key": { "b": 32, "a": "one2" } }|]
          ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "backcompat - change alternative order" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : string | b: nat }
        }
        schema x.2 {
          predicate P: { b: nat | a : string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": "A" } }|]
          , [s|{ "key": { "b": 1 } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "forwardcompat - change alternative order" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : string | b: nat }
        }
        schema x.2 {
          predicate P: { b: nat | a : string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": "A" } }|]
          , [s|{ "key": { "b": 1 } }|]
          ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "backcompat - maps new sum alternatives to unknown values" $
    TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { b : string | }
        }
        schema x.2 {
          predicate P : { a : nat | b : string | c : bool | }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 1 } }|]
          , [s|{ "key": { "b": "A" } }|]
          , [s|{ "key": { "c": true } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        -- the unknown alternative has an index one greater than
        -- the last alternative index of x.P.1.
        let unknown = RTS.Alt 1 unit
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count"
          [unknown, RTS.Alt 0 (RTS.String "A"), unknown] facts

  , TestLabel "backcompat - maps new enum alternatives to unknown values" $
    TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { b | }
        }
        schema x.2 {
          predicate P : { a | b | c }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": {} } }|]
          , [s|{ "key": { "b": {} } }|]
          , [s|{ "key": { "c": {} } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        -- the unknown alternative has an index one greater than
        -- the last alternative index of x.P.1.
        let unknown = RTS.Alt 1 unit
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count"
          [unknown, RTS.Alt 0 (RTS.Tuple []), unknown] facts

  , TestLabel "transform nested facts" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: { a : Q }
          predicate Q: { x: string }
        }
        schema x.2 {
          predicate P: { a: Q, b: maybe string }
          predicate Q: { x: string, y: maybe nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.Q" 2)
          [ [s|{ "id": 1, "key": { "x": "A", "y": 1 } }|]
          , [s|{ "id": 2, "key": { "x": "B", "y": 2 } }|]
          ]
      , mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 1, "b": "A" } }|]
          , [s|{ "key": { "a": 2, "b": "B" } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)
        nested <- decodeNestedAs (SourceRef "x.Q" (Just 1)) byRef response
        assertEqual "nested count" 2 (length nested)

  , TestLabel "change within type" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P: T
          type T = { a: string | b : TT }
          type TT = { x: nat | y: string }
        }
        schema x.2 {
          predicate P: T
          type T = { b: TT | a: string }
          type TT = { y: string | x: nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": "A" } }|]
          , [s|{ "key": { "b": { "x" : 1 } } }|]
          , [s|{ "key": { "b": { "y" : "B" } } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 3 (length facts)

  , TestLabel "no mapping when schema has facts" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : nat }
        }
        schema x.2 {
          predicate P : { a : nat, b: maybe string }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": 1 } }|]
          ]
      , mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 1, "b": "A" } }|]
          , [s|{ "key": { "a": 2, "b": "B" } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "non-evolved derived predicate with imports" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : nat }
        }
        schema x.2 {
          predicate P : { a : nat, b: maybe string }
        }
        schema x.2 evolves x.1
        schema y.1 {
          import x.1
          predicate Q : { x : x.P }
            { x = V } where V = x.P _
        }
        schema all.1 : x.1, x.2, y.1 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 1, "b": "A" } }|]
          ]
      ]
      [s| y.Q.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "y.Q" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)
        nested <- decodeNestedAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "nested count" 1 (length nested)

  , TestLabel "non-evolved derived predicate with inheritance" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { a : nat }
        }
        schema x.2 {
          predicate P : { a : nat, b: maybe string }
        }
        schema x.2 evolves x.1

        schema y.1 : x.1 {
          predicate Q : { x : x.P }
            { x = V } where V = x.P _
        }
        schema all.1 : x.1, x.2, y.1 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": 1, "b": "A" } }|]
          ]
      ]
      [s| y.Q.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "y.Q" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)
        nested <- decodeNestedAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "nested count" 1 (length nested)

  , TestLabel "query matching order" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema base.1 {
          predicate N : nat
          predicate S : string
        }
        schema x.1 : base.1 {
          predicate P : { a : N, b : S }
        }
        schema x.2 : base.1 {
          predicate P : { b: S, a : N }
        }
        schema x.2 evolves x.1
        schema all.1 : base.1, x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "base.N" 1)
          [ [s|{ "id": 1, "key": 1 }|]
          ]
      , mkBatch (PredicateRef "base.S" 1)
          [ [s|{ "id": 2, "key": "A" }|]
          , [s|{ "id": 3, "key": "B" }|]
          ]
      , mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "b": 2, "a": 1 } }|]
          , [s|{ "key": { "b": 3, "a": 1 } }|]
          ]
      ]
      -- even though x.P.2 first field is base.S, X should be
      -- bound to the first field of x.P.1 which is base.N.
      [s| X where { X, _ } = x.P.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "base.N" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "query variable" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : nat
          predicate Q : { a: P, b : string }
        }
        schema x.2 {
          predicate P : nat
          predicate Q : { b: string, a: P }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "id": 1, "key": 1 }|]
          , [s|{ "id": 2, "key": 2 }|]
          ]
      , mkBatch (PredicateRef "x.Q" 2)
          [ [s|{ "key": { "b": "A", "a": 1 } }|]
          , [s|{ "key": { "b": "A", "a": 2 } }|]
          ]
      ]
      [s| Y where
            X = x.P.1 _;
            Y = x.Q.1 { X, _ };
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.Q" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)
        nested <- decodeNestedAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "nested count" 2 (length nested)

  , TestLabel "derived pred fields" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
          predicate R : { a :nat, b: string }
            { X, Y } where P { X, Y }
        }
        schema x.2 {
          predicate P : { x : nat, y : string }
          predicate R : { b: string, a :nat  }
            { Y, X } where P { X, Y }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "x": 1, "y": "A" } }|]
          , [s|{ "key": { "x": 2, "y": "B" } }|]
          ]
      ]
      [s| x.R.1 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.R" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "predicate derivation" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : nat
          predicate Q : nat
          predicate R : nat A where P A
        }
        schema x.2 {
          predicate P : nat
          predicate Q : nat
          predicate R : nat A where Q A
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": 1 }|]
          ]
      , mkBatch (PredicateRef "x.Q" 2)
          [ [s|{ "key": 3 }|]
          , [s|{ "key": 4 }|]
          ]
      ]
      [s| x.R.1 _ |]
      -- Derived predicates should not be substituted for their newest
      -- version but the query they flatten-out into should be evolved.
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.R" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "predicate value" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
          predicate Q : nat -> P
        }
        schema x.2 {
          predicate P : { y : string, x : nat }
          predicate Q : nat -> P
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "id": 1, "key": { "y": "A", "x": 1 } }|]
          ]

      , mkBatch (PredicateRef "x.Q" 2)
          [ [s|{ "key": 1, "value" : 1 }|]
          ]
      ]
      [s| P where x.Q.1 _ -> P |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "derived predicate value" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
          predicate Q : nat -> P
            X -> Y where Y = P { X, _ }
        }
        schema x.2 {
          predicate P : { y : string, x : nat }
          predicate Q : nat -> P
            X -> Y where Y = P { _, X }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "y": "A", "x": 1 } }|]
          , [s|{ "key": { "y": "B", "x": 2 } }|]
          ]
      ]
      [s| P where x.Q.1 _ -> P |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "subquery in primcall" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
        }
        schema x.2 {
          predicate P : { y : string, x : nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "y": "A", "x": 1 } }|]
          , [s|{ "key": { "y": "B", "x": 2 } }|]
          ]
      ]
      [s| x.P.1 { X, _ } where
            X = (Y where x.P.1 { Y, _ }) + (Z where x.P.1 { Z, _ })
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "array elements" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
        }
        schema x.2 {
          predicate P : { y : string, x : nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "y": "A", "x": 1 } }|]
          , [s|{ "key": { "y": "B", "x": 2 } }|]
          ]
      ]
      -- check that not only the type of elements inside the array
      -- changed but also that the pattern matched against the array
      -- is changed.
      [s| x.P.1 { N, _ } where
            A = [ x.P.1 { 1, _ }, x.P.1 { 2, _ } ];
            { N, _ } = A[..]
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "named type inside alts" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          type T = { a: string, b: nat }
          predicate P : { x : maybe T }
        }
        schema x.2 {
          type T = { b: nat, a: string }
          predicate P : { x : maybe T }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "x": { "a": "A", "b": 1 } } }|]
          , [s|{ "key": { "x": { "a": "B", "b": 2 } } }|]
          ]
      ]
      [s| x.P.1 { x = { just = { a = _ } } }|]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 2 (length facts)

  , TestLabel "negation" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x : nat, y : string }
        }
        schema x.2 {
          predicate P : { y : string, x : nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "y": "A", "x": 1 } }|]
          , [s|{ "key": { "y": "B", "x": 2 } }|]
          ]
      ]
      [s| X where
            X = x.P.1 _;
            !(X = x.P.1 { 1, _ })
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "multiple evolved, same pred" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate N : nat
        }
        schema x.2 {
          predicate N : nat
        }
        schema x.2 evolves x.1

        schema y.1 {
          import x.1
          import x.2

          predicate Q : { x: x.N.1, y: x.N.2 }
        }
        schema all.1 : x.1, x.2, y.1 {}
      |]
      [ mkBatch (PredicateRef "x.N" 2)
          [ [s|{ "key": 1 }|]
          ]
      ]
      [s| y.Q.1 _ |]
      $ \_ response _ ->
        case response of
          Right _ -> assertFailure "expected request to fail"
          Left badQuery ->
            assertBool "errors with multple versions of evolved predicate"
            ("multiple versions of evolved predicates"
              `isInfixOf` show badQuery)

  , TestLabel "explicit fact id" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x: nat }
        }
        schema x.2 {
          predicate P : { x: nat, y: maybe nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "id": 1, "key": { "x": 1, "y": 2 } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \byRef (Right results) runQuery -> do
        bin <- binResults results
        let factId = head $ Map.keys $ userQueryResultsBin_facts bin

        response <- runQuery $ "$" <> pack (show factId) <> " : x.P.1"
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "correct return type" $ TestCase $ do
    withSchemaAndFacts []
      [s|
        schema x.1 {
          predicate P : { x: nat }
        }
        schema x.2 {
          predicate P : { x: nat, y: maybe nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "id": 1, "key": { "x": 1, "y": 2 } }|]
          ]
      ]
      [s| x.P.1 _ |]
      $ \_ (Right results) _ -> do
        let Just ty = userQueryResults_type results
        assertEqual "result type" "x.P.1" ty
  ]
  where
    decodeResultsAs _ _ (Left err) = assertFailure $ "BadQuery: " <> show err
    decodeResultsAs ref byRef (Right results) =  do
      decodeResultFacts userQueryResultsBin_facts (keyType ref byRef) results

    decodeResultsAsTy _ (Left err) = assertFailure $ "BadQuery: " <> show err
    decodeResultsAsTy ty (Right results) =  do
      decodeResultFacts userQueryResultsBin_facts ty results

    decodeNestedAs _ _ (Left err) = assertFailure $ "BadQuery: " <> show err
    decodeNestedAs ref byRef (Right results) = do
      decodeResultFacts userQueryResultsBin_nestedFacts
        (keyType ref byRef) results

    decodeResultFacts f ty results = do
      bin <- binResults results
      let keys = fmap fact_key $ Map.elems $ f bin
      decoded <- sequence <$> mapM (decodeAs ty) keys
      case decoded of
        Right values -> return values
        Left err ->
          assertFailure $ "unable to decode : " <> show err

    binResults :: UserQueryResults -> IO UserQueryResultsBin
    binResults UserQueryResults{..} =
      case userQueryResults_results of
        UserQueryEncodedResults_bin b -> return b
        _ -> assertFailure "wrong encoding"

    keyType
      :: SourceRef
      -> DbSchema
      -> RTS.Type
    keyType ref dbSchema =
      case lookupPredicateSourceRef ref LatestSchemaAll dbSchema of
        Left err -> error $ "can't find predicate: " <>
          unpack (showRef ref) <> ": " <> unpack err
        Right details -> predicateKeyType details

    decodeAs :: RTS.Type -> ByteString -> IO (Either String RTS.Value)
    decodeAs ty bs = do
      print bs
      fmap (first showException) $ try $ evaluate
        $ RTS.toValue (withUnknown $ RTS.repType ty) bs
      where
        showException (RTS.DecodingException e) = e
        -- we want to decode binary values that contain the unknown alternative
        withUnknown rep = case rep of
          RTS.ByteRep -> rep
          RTS.NatRep -> rep
          RTS.ArrayRep elty -> RTS.ArrayRep $ withUnknown elty
          RTS.TupleRep tys -> RTS.TupleRep $ fmap withUnknown tys
          RTS.SumRep tys ->
            let unknown = RTS.TupleRep [] in
            RTS.SumRep $ fmap withUnknown tys ++ [unknown]
          RTS.StringRep -> rep
          RTS.PredicateRep _ -> rep

main :: IO ()
main = withUnitTest $ testRunner $ TestList $
  [ TestLabel "schemaEvolves" schemaEvolves
  , TestLabel "schemaEvolvesTransformations" schemaEvolvesTransformations
  ]

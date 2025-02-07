{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Schema.Evolves (main) where

import Control.Exception
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.List
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Angle.Types (latestAngleVersion, Type_(..))
import Glean.Database.Schema.Types
import Glean.Init
import Glean (userQuery, userQueryFacts)
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
            ("cannot evolve predicate test.P.1 " <>
              "into test.P.2: missing required field: b")
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
          predicate P : { a : string }
        }

        schema x.2 : x.1  {
        }
        schema x.2 evolves x.1

        schema x.3 {
          predicate P : { a: nat }
        }
        schema x.3 evolves x.2
        schema all.1 : x.1, x.2, x.3 {}
      |]
      $ \r ->
      -- should try to make x.P.3 evolve x.P.1
      assertBool "throws error stating missing field" $
        case r of
          Left err ->
            ("cannot evolve predicate x.P.1 into x.P.3: " <>
              "in field 'a', type changed")
            `isInfixOf` show err
          Right _ -> False

  , TestLabel "re-exported predicates reverse" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema x.1 {
          predicate Q : nat
          predicate P : { ref : Q }
        }

        schema x.2 {
          predicate Q : nat
          predicate P : { ref : Q }
        }

        schema x.3 : x.2 {
          predicate P : { ref : Q }
        }
        schema x.3 evolves x.1
        schema all.1 : x.1, x.2, x.3 {}
      |]
      $ \r ->
      -- x.Q.2 should evolve x.Q.1
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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

  , TestLabel "transform nested facts in userQueryFacts" $ TestCase $ do
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
      $ \env repo schema -> do
        -- get all P.2 facts
        response <- userQuery env repo $ def
          { userQuery_query = "x.P.2 _"
          , userQuery_options = Just def
            { userQueryOptions_syntax = QuerySyntax_ANGLE }
          , userQuery_encodings = [ UserQueryEncoding_bin def ]
          }
        fids <- factIds response

        -- ask for P.2 facts by Id as P.1
        response <- try $ userQueryFacts env repo $ def
          { userQueryFacts_facts =
              [ def
                  { factQuery_id = fromIntegral fid
                  , factQuery_predicate_version = Just 1
                  , factQuery_recursive = True
                  }
              | fid <- fids ]
          , userQueryFacts_encodings = [ UserQueryEncoding_bin def ]
          }

        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) schema response
        assertEqual "result count" 2 (length facts)
        nested <- decodeNestedAs (SourceRef "x.Q" (Just 1)) schema response
        assertEqual "nested count" 2 (length nested)

  , TestLabel "transform nested record" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          type T = { a: string }
          predicate P : { x: T, y: T  }
        }
        schema x.2 {
          type T = { a: string, b: maybe nat }
          predicate P : { x: T, y: T  }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": { "a": "A" }, "y": { "a": "B" }}}|]
          , [s|{ "key": { "x": { "a": "B" }, "y": { "a": "A" }}}|]
          ]
      ]
      [s| x.P.2 { X, Y };
          x.P.2 { Y, X }
      |]
      -- should add the 'maybe nat` field when binding X and Y, and then remove
      -- it again when matching X and Y against db values.
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result"
          [ RTS.Tuple
              [ RTS.Tuple [RTS.String "A", RTS.Alt 0 (RTS.Tuple [])]
              , RTS.Tuple [RTS.String "B", RTS.Alt 0 (RTS.Tuple [])]
              ]
          , RTS.Tuple
              [ RTS.Tuple [RTS.String "B", RTS.Alt 0 (RTS.Tuple [])]
              , RTS.Tuple [RTS.String "A", RTS.Alt 0 (RTS.Tuple [])]
              ]
          ] facts

  , TestLabel "transform deep nested record" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P : { x: nat, y: nat  }
        }

        schema x.2 {
          predicate P : { x: nat  }
        }
        schema x.2 evolves x.1

        schema y.1 {
          import x.2
          predicate P : x.P
            X where X = x.P _
        }

        schema z.1 {
          import y.1
          predicate P : y.P
            X where X = y.P _
        }

        schema all.1 : x.1, x.2, z.1, y.1 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": 1, "y": 2 }}|]
          ]
      ]
      [s| z.P _ |]
      -- should add the 'maybe nat` field when binding X and Y, and then remove
      -- it again when matching X and Y against db values.
      $ \schema response _ -> do
        _ <- decodeResultsAs (SourceRef "z.P" (Just 1)) schema response
        nested <- decodeNestedAs (SourceRef "x.P" (Just 2)) schema response
        assertEqual "nested" 1 (length nested)

  , TestLabel "uses transformed record for prefix search" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P: {x: nat, y: { a: string, b: enum { One | Two | Three }}}
        }
        schema x.2 {
          predicate P: {y: {  b: enum { Two | One }, a: string, }, x: nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": 1, "y": { "a": "A", "b": 0 }}}|]
          , [s|{ "key": { "x": 1, "y": { "a": "A", "b": 1 }}}|]
          , [s|{ "key": { "x": 1, "y": { "a": "A", "b": 2 }}}|]
          ]
      ]
      [s| x.P.2 { { One, "A"}, 1 }
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result" 1 (length facts)
        let searched =
              sum $ Map.elems $ fromMaybe mempty $
              userQueryStats_facts_searched $ fromMaybe def $
              userQueryResults_stats $ either (error "request failure") id
              response
        assertEqual "facts searched" 1 searched

  , TestLabel "change within type" $ TestCase $ do
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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

  , TestLabel "dependency without facts" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 { predicate P : { a : nat } }
        schema x.2 { predicate P : { b : string } }
        schema x.2 evolves x.1

        schema y.1 {
          import x.1
          predicate Q : [x.P.1]
        }

        schema y.2 {
          import x.2
          predicate Q : [x.P.2]
        }
        schema y.2 evolves y.1
        schema all.1 : x.1, x.2, y.1, y.2 {}
      |]
      [ mkBatch (PredicateRef "y.Q" 1)
          [ [s|{ "key": [] }|]
          ]
      ]
      [s| y.Q.2 _ |]
      -- there are no facts of x.1 or x.2 so there will be no evolution
      -- of the x schema. Nonetheless we should be able to evolve y.1
      -- into y.2 even though there is no evolution for one of its option.
      -- This is only the case because there are no facts for the option
      -- or its evolved counterpart.
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "y.Q" (Just 2)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "query matching order" $ TestCase $ do
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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

  , TestLabel "whole key assigned to variable - remove fields" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P : { a : string }
        }
        schema x.2 {
          predicate P : { a: string, b: maybe nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": { "a": "A", "b": 1  }}|]
          , [s|{ "key": { "a": "B", "b": 2  }}|]
          ]
      ]
      [s| X where
            X = x.P.1 Y;
            { a = "A" } = Y;
      |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result" [RTS.Tuple [RTS.String "A"]] facts

  , TestLabel "whole key assigned to variable - fill defaults" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P : { a : string }
          predicate Q : { a: string, b: maybe nat }
        }
        schema x.2 {
          predicate P : { a: string, b: maybe nat }
          predicate Q : { a: string, b: maybe nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "a": "A" }}|]
          , [s|{ "key": { "a": "B" }}|]
          ]
      , mkBatch (PredicateRef "x.Q" 1)
          [ [s|{ "key": { "a": "A" }}|]
          , [s|{ "key": { "a": "A", "b": 1  }}|]
          , [s|{ "key": { "a": "B", "b": 2  }}|]
          ]
      ]
      [s| x.P.2 X;
          x.Q.2 X
      |]
      -- should bind 'nothing' as the default value of the second field of
      -- x.P.2, causing values from x.Q.2 with a second value of 'just' to
      -- not match.
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.Q" (Just 2)) byRef response
        assertEqual "result"
          [RTS.Tuple [RTS.String "A", RTS.Alt 0 (RTS.Tuple [])]]
          facts

  , TestLabel "predicate derivation" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P : nat
          predicate Q : string
          predicate R : nat A where P A
        }
        schema x.2 {
          predicate P : nat
          predicate Q : string
          predicate R : string A where Q A
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "key": 1 }|]
          ]
      , mkBatch (PredicateRef "x.Q" 2)
          [ [s|{ "key": "A" }|]
          , [s|{ "key": "B" }|]
          ]
      ]
      [s| x.R.1 _ |]
      -- Derived predicates should not be substituted for their newest
      -- version but the query they flatten-out into should be evolved.
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.R" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "predicate value" $ TestCase $ do
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
    withSchemaAndFactsQ
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
        fids <- factIds results
        let factId = head fids

        response <- runQuery $ "$" <> pack (show factId) <> " : x.P.1"
        facts <- decodeResultsAs (SourceRef "x.P" (Just 1)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "correct return type" $ TestCase $ do
    withSchemaAndFactsQ
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

  , TestLabel "re-exported predicates" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate Q : nat
        }

        schema x.2 : x.1 {
          predicate P : { ref : Q.1 }
        }

        schema x.3 {
          predicate Q : nat
          predicate P : { ref : Q }
        }

        schema x.3 evolves x.2
        schema all.1 : x.1, x.2, x.3 {}
      |]
      [ mkBatch (PredicateRef "x.Q" 1)
          [ [s|{ "id": 1, "key": 1 }|]
          ]
      , mkBatch (PredicateRef "x.P" 2)
          [ [s|{ "id": 1, "key": { "ref": 1 } }|]
          ]
      ]
      -- should make x.Q.1, which is re-exported by x.3, evolve x.Q.3
      [s| x.P.3 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 3)) byRef response
        assertEqual "result count" 1 (length facts)

  , TestLabel "transform within array" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          type T = { a : nat }
          predicate P: { x : [T] }
        }
        schema x.2 {
          type T = { a : nat, b : maybe nat }
          predicate P: { x : [T] }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": [{ "a": 1 }, { "a": 2 }] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Tuple [RTS.Nat 1, nothing]
              , RTS.Tuple [RTS.Nat 2, nothing]
              ] ] ]
          facts
  , TestLabel "transform predicate within set" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          type T = { a : nat }
          predicate P: { x : set T }
        }
        schema x.2 {
          type T = { a : nat, b : maybe nat }
          predicate P: { x : set T }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "key": { "x": [{ "a": 1 }, { "a": 2 }] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Tuple [RTS.Nat 1, nothing]
              , RTS.Tuple [RTS.Nat 2, nothing]
              ] ] ]
          facts
  , TestLabel "evolve array to set of nat" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P: { x : [nat] }
        }
        schema x.2 {
          predicate P: { x : set nat }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "key": { "x": [ 1 , 1, 2 ] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Nat 1
              , RTS.Nat 2
              ] ] ]
          facts
  , TestLabel "evolve set to array of nat" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P: { x : set nat }
        }
        schema x.2 {
          predicate P: { x : [nat] }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "key": { "x": [ 1 , 1, 2 ] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Nat 1
              , RTS.Nat 2
              ] ] ]
          facts
  , TestLabel "evolve set to array of byte" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P: { x : set byte }
        }
        schema x.2 {
          predicate P: { x : [byte] }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "key": { "x": [ 1 , 1, 2 ] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.ByteArray "\SOH\STX" ] ]
          facts
  , TestLabel "evolve array to set of byte" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          predicate P: { x : [byte] }
        }
        schema x.2 {
          predicate P: { x : set byte }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "key": { "x": [ 1 , 1, 2 ] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array [RTS.Byte 1, RTS.Byte 2] ] ]
          facts
  , TestLabel "evolve array to set of pred" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          type T = { a : nat }
          predicate P: { x : [T] }
        }
        schema x.2 {
          type T = { a : nat, b : maybe nat }
          predicate P: { x : set T }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": [{ "a": 1 }, { "a": 2 }] } }|] ]

      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Tuple [RTS.Nat 1, nothing]
              , RTS.Tuple [RTS.Nat 2, nothing]
              ] ] ]
          facts
  , TestLabel "evolve set to array of pred" $ TestCase $ do
    withSchemaAndFactsQ
      [s|
        schema x.1 {
          type T = { a : nat }
          predicate P: { x : set T }
        }
        schema x.2 {
          type T = { a : nat, b : maybe nat }
          predicate P: { x : [T] }
        }
        schema x.2 evolves x.1
        schema all.1 : x.1, x.2 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": { "x": [{ "a": 1 }, { "a": 2 }] } }|] ]
      ]
      [s| x.P.2 _ |]
      $ \byRef response _ -> do
        facts <- decodeResultsAs (SourceRef "x.P" (Just 2)) byRef response
        assertEqual "result content"
          [ RTS.Tuple
            [ RTS.Array
              [ RTS.Tuple [RTS.Nat 1, nothing]
              , RTS.Tuple [RTS.Nat 2, nothing]
              ] ] ]
          facts
  ]
  where
    -- run a userQuery using the given schemas and facts
    withSchemaAndFactsQ
      :: String                  -- ^ schema
      -> [JsonFactBatch]         -- ^ db contents
      -> Text                    -- ^ initial query
      -> ( DbSchema
        -> Either BadQuery UserQueryResults                -- query response
        -> (Text -> IO (Either BadQuery UserQueryResults)) -- run more queries
        -> IO a )
      -> IO a
    withSchemaAndFactsQ schema facts query act =
      withSchemaAndFacts [] schema facts $ \env repo dbSchema -> do
      let run q = do
            response <- try $ runQuery env repo (encodeUtf8 q)
            print (response :: Either BadQuery UserQueryResults)
            return response
      res <- run query
      act dbSchema res run
      where
        runQuery env repo q = userQuery env repo $ def
          { userQuery_query = q
          , userQuery_options = Just def
            { userQueryOptions_syntax = QuerySyntax_ANGLE
            , userQueryOptions_recursive = True
            , userQueryOptions_collect_facts_searched = True
            , userQueryOptions_debug = def
              { queryDebugOptions_bytecode = False
              , queryDebugOptions_ir = False
              }
            }
          , userQuery_encodings = [ UserQueryEncoding_bin def ]
          }

    factIds :: UserQueryResults -> IO [Id]
    factIds results =
      case userQueryResults_results results of
        UserQueryEncodedResults_bin bin ->
          return $ Map.keys $ userQueryResultsBin_facts bin
        _ ->
          assertFailure "wrong encoding"

    decodeResultsAs
      :: SourceRef
      -> DbSchema
      -> Either BadQuery UserQueryResults
      -> IO [RTS.Value]
    decodeResultsAs ref schema eresults = do
      res <- decodeResults
        (keyType ref schema) userQueryResultsBin_facts eresults
      either assertFailure return res

    -- filter nested predicates by SourceRef before decoding
    decodeNestedAs ref schema eresults = do
      let hasPid (RTS.Pid pid) fact = fromIntegral pid == Thrift.fact_type fact
          factsOfType pid = Map.filterWithKey (const $ hasPid pid)
      pid <- either (assertFailure . unpack) (return . predicatePid) $
        lookupPredicateSourceRef ref LatestSchema schema
      putStrLn $ "Filtering for " <> show (showRef ref) <> " with Pid " <> show pid
      either assertFailure return =<< decodeResults
        (keyType ref schema)
        (factsOfType pid . userQueryResultsBin_nestedFacts)
        eresults


    decodeResultsAsTy ty eresults =  do
      res <- decodeResults ty userQueryResultsBin_facts eresults
      either assertFailure return res

    keyType
      :: SourceRef
      -> DbSchema
      -> RTS.Type
    keyType ref dbSchema =
      case lookupPredicateSourceRef ref LatestSchema dbSchema of
        Left err -> error $ "can't find predicate: " <>
          unpack (showRef ref) <> ": " <> unpack err
        Right details -> predicateKeyType details

main :: IO ()
main = withUnitTest $ testRunner $ TestList $
  [ TestLabel "schemaEvolves" schemaEvolves
  , TestLabel "schemaEvolvesTransformations" schemaEvolvesTransformations
  ]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module Schema.Multi (main) where

import Control.Exception
import Control.Monad
import Data.Either
import Data.Default (def)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.HUnit

import TestRunner
import Thrift.Util
import Util.String.Quasi

import Glean.Angle.Types (Type_(..), FieldDef_(..))
import Glean.Backend.Types (userQueryFacts, userQuery)
import Glean.Database.Config
import Glean.Database.Schema
import Glean.Database.Test
import Glean.Init
import qualified Glean.RTS.Term as RTS
import Glean.Types as Thrift
import qualified Glean.Internal.Types as Internal
import Glean.Write.JSON


import Schema.Lib

multiSchemaTest :: Test
multiSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema x.1 {
            predicate P : { a: string }
          }

          schema y.1 {
            import x.1
            predicate Q : { p : x.P }
          }

          schema z.1 {
            predicate R : string
          }

          schema derived.1 {
            import x.1
            predicate D : { a : string }
              { a = A } where x.P { a = A }

            type T = string
            predicate S : T
              stored X where x.P { a = X }
          }

          schema all.1 : x.1, y.1, z.1, derived.1 {}
        |]

      schema_v1_file = root </> "schema1"
      schema_v1 =
        [s|
          schema x.1 {
            # P has a new field
            predicate P : { a: string, b: nat }
          }

          schema y.1 {
            import x.1
            predicate Q : { p : x.P }
          }

          # z.1 has been deleted

          schema derived.1 {
            import x.1
            predicate D : { a : string }
              { a = A } where x.P { a = A, b = 3 }
              # now matches on the new field

            # check that we can change the definition of a stored predicate,
            # deleting a type synonym and using a different definition.
            predicate S : string
              stored X where x.P { a = X }
          }

          schema all.1 : x.1, y.1, derived.1 {}
        |]

      schema_index_file_0 = root </> "schema_index_0"
      schema_index_0 = Internal.SchemaIndex
        { schemaIndex_current = Internal.SchemaInstance
          { schemaInstance_versions = Map.fromList [ ("v0", 0) ]
          , schemaInstance_file = "schema0"
          }
        , schemaIndex_older = []
        }

      schema_index_file_1 = root </> "schema_index_1"
      schema_index_1 = Internal.SchemaIndex
        { schemaIndex_current = Internal.SchemaInstance
          { schemaInstance_versions = Map.fromList [ ("v1", 1) ]
          , schemaInstance_file = "schema1"
          }
        , schemaIndex_older = [
            Internal.SchemaInstance
              { schemaInstance_versions = Map.fromList [ ("v0", 1) ]
              , schemaInstance_file = "schema0"
              }
        ]
        }

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1
    saveJSON schema_index_file_0 schema_index_0
    saveJSON schema_index_file_1 schema_index_1

    let dbRoot = root </> "db"
    createDirectory dbRoot

    let
      mkRepo schema hash upd facts =
        withTestEnv [
            setRoot dbRoot,
            enableSchemaId,
            setSchemaIndex schema ] $ \env -> do
          let repo = Repo "test" hash
          kickOffTestDB env repo upd
          facts env repo
          completeTestDB env repo
          return repo

      testEnv schema schema_id act =
        withTestEnv
          [ setRoot dbRoot,
            setSchemaIndex schema,
            enableSchemaId,
            maybe id (setSchemaId . SchemaId) schema_id ]
          act

      testQuery name repo schema schema_id query result =
        testEnv schema schema_id $ \env -> do
          r <- try $ angleQuery env repo query
          case result of
            Just n -> case r :: Either BadQuery UserQueryResults of
              Right UserQueryResults{..} ->
                assertEqual name n (length userQueryResults_facts)
              _ -> assertFailure (name <> ": " <> show r)
            Nothing -> assertBool name $ case r of
              Left{} -> True
              _ -> False

      testQueryFacts name repo schema schema_id query ty results =
        testEnv schema schema_id $ \env -> do
          -- get fids
          fids <- do
            UserQueryResults{..} <- userQuery env repo def
              { userQuery_query = query
              , userQuery_options = Just def
                { userQueryOptions_syntax = QuerySyntax_ANGLE
                }
              , userQuery_encodings = [ UserQueryEncoding_bin def ]
              }
            UserQueryEncodedResults_bin b <- return userQueryResults_results
            return $ Map.keys (userQueryResultsBin_facts b)

          -- ask for facts
          r <- try $ userQueryFacts env repo def
            { userQueryFacts_facts =
                [ def { factQuery_id = fromIntegral fid } | fid <- fids ]
            , userQueryFacts_encodings = [ UserQueryEncoding_bin def ]
            }

          -- check that the result is as expected
          decoded <- decodeResults ty userQueryResultsBin_facts r
          case decoded of
            Left err -> assertFailure err
            Right values -> assertEqual name values results

      v0_facts =
        [ mkBatch (PredicateRef "z.R" 1)
            [ [s| { "key" : "abc" } |] ]
        , mkBatch (PredicateRef "x.P" 1)
            [ [s| { "key" : { "a" : "xyz" } } |] ]
        , mkBatch (PredicateRef "y.Q" 1)
            [ [s| { "key" : { "p" : { "key" : { "a" : "xyz" } }}} |] ]
        ]

      v1_facts =
        [ mkBatch (PredicateRef "x.P" 1)
            [ [s| { "key" : { "a" : "xyz", "b": 3 } } |] ]
        , mkBatch (PredicateRef "y.Q" 1)
            [ [s| { "key" : { "p" : { "key" : { "a" : "xyz", "b" : 3 } }}} |] ]
        ]

    -- create a DB using v0
    repo0 <- mkRepo schema_index_file_0 "0" id $ \env repo -> do
      void $ syncWriteJsonBatch env repo v0_facts Nothing

    -- create a DB using v1
    repo1 <- mkRepo schema_index_file_1 "1" id $ \env repo -> do
      void $ syncWriteJsonBatch env repo v1_facts Nothing

    -- create a DB using v0 by setting glean.schema_id
    let set kickOff = kickOff {
          kickOff_properties = HashMap.insert "glean.schema_id" "v0"
            (kickOff_properties kickOff) }
    repo2 <- mkRepo schema_index_file_1 "2" set $ \env repo -> do
      void $ syncWriteJsonBatch env repo v0_facts Nothing

    -- query repo0 with index 1, explicitly ask for schema v0
    testQuery "multi 0a" repo0 schema_index_file_1 (Just "v0")
      "z.R _" (Just 1)
    testQuery "multi 0b" repo0 schema_index_file_1 (Just "v0")
      "x.P _" (Just 1)
    testQuery "multi 0c" repo0 schema_index_file_1 (Just "v0")
      "derived.D _" (Just 1)

    -- query repo0 with index 1, explicitly ask for schema v1
    testQuery "multi 2a" repo0 schema_index_file_1 (Just "v1") "z.R _" Nothing
      -- we can't see z.R now
    testQuery "multi 2b" repo0 schema_index_file_1 (Just "v1") "x.P _" (Just 1)
      -- we can see x.P, but we'll get the new version
    testQuery "multi 2c" repo0 schema_index_file_1 (Just "v1")
      "y.Q { p = { b = 0 }}" (Just 1)
      -- this should give us 1 fact, because we're matching on b's default
    testQuery "multi 2d" repo0 schema_index_file_1 (Just "v1")
      "derived.D _" (Just 0)

    -- query repo1 with index 1, explicitly ask for schema v0
    testQuery "multi 3a" repo1 schema_index_file_1 (Just "v0") "x.P _" (Just 1)
    testQuery "multi 3b" repo1 schema_index_file_1 (Just "v0") "y.Q _" (Just 1)
    testQuery "multi 3c" repo1 schema_index_file_1 (Just "v0")
      "y.Q { p = { b = 0 }}" Nothing
    testQuery "multi 3d" repo1 schema_index_file_1 (Just "v0")
      "derived.D _" (Just 1)
      -- this should be a type error, P doesn't have the b field
    -- check that the results gets transformed according to the schema_id.
    testQueryFacts "multi 3d" repo1 schema_index_file_1 (Just "v0") "x.P _"
      (RecordTy [FieldDef "a" StringTy])
      [RTS.Tuple [RTS.String "xyz"]]

    -- query repo1 with index 1, don't ask for v0
    testQuery "multi 4a" repo1 schema_index_file_1 Nothing "x.P _" (Just 1)
    testQuery "multi 4b" repo1 schema_index_file_1 Nothing "y.Q _" (Just 1)
    testQuery "multi 4c" repo1 schema_index_file_1 Nothing
      "y.Q { p = { b = 3 }}" (Just 1)
    testQuery "multi 4d" repo1 schema_index_file_1 Nothing
      "derived.D _" (Just 1)
    testQueryFacts "multi 4e" repo1 schema_index_file_1 Nothing "x.P _"
      (RecordTy [FieldDef "a" StringTy, FieldDef "b" NatTy ])
      [RTS.Tuple [RTS.String "xyz", RTS.Nat 3]]

    -- query repo1 with index 1, explicitly ask for schema v1
    testQuery "multi 5a" repo1 schema_index_file_1 (Just "v1") "x.P _" (Just 1)
    testQuery "multi 5b" repo1 schema_index_file_1 (Just "v1") "y.Q _" (Just 1)
    testQuery "multi 5c" repo1 schema_index_file_1 (Just "v1")
      "y.Q { p = { b = 3 }}" (Just 1)
    testQuery "multi 5d" repo1 schema_index_file_1 (Just "v1")
      "derived.D _" (Just 1)
    testQueryFacts "multi 4e" repo1 schema_index_file_1 (Just "v1") "x.P _"
      (RecordTy [FieldDef "a" StringTy, FieldDef "b" NatTy ])
      [RTS.Tuple [RTS.String "xyz", RTS.Nat 3]]

    -- if we downgraded the schema, so the repo has a later version
    -- than the global schema, these queries should still work
    testQuery "multi 6a" repo1 schema_index_file_0 Nothing "x.P _" (Just 1)
    testQuery "multi 6b" repo1 schema_index_file_0 Nothing "y.Q _" (Just 1)
    testQuery "multi 6c" repo1 schema_index_file_0 Nothing
      "y.Q { p = { b = 3 }}" Nothing
    testQuery "multi 6d" repo1 schema_index_file_0 Nothing
      "derived.D _" (Just 1)

    -- asking for a schema that doesn't exist, we will default to the
    -- latest global schema
    testQuery "multi 7a" repo1 schema_index_file_0 (Just "na") "x.P _" (Just 1)
    testQuery "multi 7b" repo1 schema_index_file_0 (Just "na") "y.Q _" (Just 1)
    testQuery "multi 7c" repo1 schema_index_file_0 (Just "na")
      "y.Q { p = { b = 3 }}" Nothing
    testQuery "multi 7d" repo1 schema_index_file_0 (Just "na")
      "derived.D _" (Just 1)

    -- Test that glean.schema_id works. repo2 should be using schema v0
    testQuery "multi 8a" repo2 schema_index_file_1 (Just "v0")
      "z.R _" (Just 1)
    testQuery "multi 8b" repo2 schema_index_file_1 (Just "v0")
      "x.P _" (Just 1)
    testQuery "multi 8c" repo2 schema_index_file_1 (Just "v0")
      "derived.D _" (Just 1)

    -- create a stacked DB on top of v0
    let
      stacked (Thrift.Repo name hash) =
        Thrift.Dependencies_stacked $ Thrift.Stacked name hash Nothing
      set kickOff = kickOff {
          kickOff_dependencies = Just (stacked repo0) }
    repo3 <- mkRepo schema_index_file_1 "3" set $ \env repo -> do
      void $ syncWriteJsonBatch env repo v0_facts Nothing

    testQuery "multi 9a" repo3 schema_index_file_1 Nothing "x.P _" (Just 1)

validateSchemaChanges :: Test
validateSchemaChanges =
    let
      schema_v0 =
        [s|
          schema x.1 {
            predicate P : { a: string }
            predicate Q : { a: string | b: nat }
          }

          schema all.1 : x.1 {}
        |]

      schema_v1 =
        [s|
          schema x.1 {
            predicate P : { a: string, b : { c: nat, d: bool } }
          }

          schema all.1 : x.1 {}
        |]

      schema_v2 =
        [s|
          schema x.1 {
            predicate R : string
            predicate P : { a: string, b : R }
          }

          schema all.1 : x.1 {}
        |]

      schema_v3 =
        [s|
          schema x.1 {
            predicate Q : { a: string | b: nat | c: [nat] }
          }

          schema all.1 : x.1 {}
        |]

      schema_v4 =
        [s|
          schema x.1 {
            predicate P : { a: string }
          }

          schema all.1 : x.1 {}
        |]

      schema_v5 =
        [s|
          schema x.1 {
            type T = string
            predicate P : { a: T }
          }

          schema all.1 : x.1 {}
        |]

      schema_v6 =
        [s|
          schema x.1 {
            predicate P : { a: bool }
            predicate Q : { a: string | b: nat }
          }

          schema all.1 : x.1 {}
        |]

      schema_v7 =
        [s|
          schema x.1 {
            predicate P : { a: bool }
          }

          schema y.1 {
            import x.1
            predicate Q : { a: x.P }
          }

          schema all.1 : x.1, y.1 {}
        |]

      schema_v8 =
        [s|
          schema x.1 {
            predicate P : { a: bool }
          }

          schema x.2 {
            predicate P : { a: bool, b: bool }
          }

          schema x.2 evolves x.1

          schema y.1 {
            import x.2
            predicate Q : { a: x.P }
          }

          schema all.1 : x.1, x.2, y.1 {}
        |]

      withIndex root a b f = do
        saveJSON schema_index_file schema_index
        f schema_index_file
        where
        schema_index_file = root </> "schema_index_" <> a <> b
        schema_index = Internal.SchemaIndex
          { schemaIndex_current = Internal.SchemaInstance
            { schemaInstance_versions = Map.fromList [ ("v1", 1) ]
            , schemaInstance_file = Text.pack a
            }
          , schemaIndex_older = [
              Internal.SchemaInstance
                { schemaInstance_versions = Map.fromList [ ("v0", 1) ]
                , schemaInstance_file = Text.pack b
                }
          ]
          }

      validate
        :: String
        -> String
        -> String
        -> (Either SomeException () -> Bool)
        -> Test
      validate lbl a b p = TestLabel lbl $ TestCase $ do
        withSystemTempDirectory "glean-dbtest" $ \root -> do
          let fileA = "schemaA"
              fileB = "schemaB"
          writeFile (root </> fileA) a
          writeFile (root </> fileB) b
          withIndex root fileA fileB $ \index_file -> do
            schema <- parseSchemaIndex index_file
            r <- try $ validateNewSchemaInstance schema
            print r
            assertBool "validate" $ p (r :: Either SomeException ())

      validateOK lbl a b = TestList
        [ validate (lbl <> " (forwards)") a b isRight
        , validate (lbl <> " (backwards)") b a isRight
        ]
      validateFAIL lbl a b = TestList
        [ validate (lbl <> " (forwards)") a b isLeft
        , validate (lbl <> " (backwards)") b a isLeft
        ]
    in
    TestList
      [ validateOK "adding a defaultable field" schema_v0 schema_v1
      , validateFAIL "adding a non-defaultable field" schema_v0 schema_v2
      , validateOK "adding an alternative" schema_v0 schema_v3
      , validateOK "adding a predicate" schema_v0 schema_v4
      , validateOK "adding a type synonym" schema_v0 schema_v5
      , validateFAIL "changing the type of a field" schema_v0 schema_v6
      -- this tests only going from schema_v7 to schema_v8 because the
      -- evolves relationship doesn't exist the other way around.
      , validate "change evolved field" schema_v8 schema_v7 isRight
      ]


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [
    TestLabel "multiSchemaTest" multiSchemaTest,
    TestLabel "validateSchemaChanges" validateSchemaChanges
  ]

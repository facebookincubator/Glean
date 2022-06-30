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
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.HUnit

import TestRunner
import Thrift.Util
import Util.String.Quasi

import Glean.Database.Test
import Glean.Init
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

      testQuery name repo schema schema_id query result =
        withTestEnv [
            setRoot dbRoot,
            setSchemaIndex schema,
            enableSchemaId,
            maybe id (setSchemaId . SchemaId) schema_id ] $ \env -> do
          r <- try $ angleQuery env repo query
          case result of
            Just n -> case r :: Either BadQuery UserQueryResults of
              Right UserQueryResults{..} ->
                assertEqual name n (length userQueryResults_facts)
              _ -> assertFailure (name <> ": " <> show r)
            Nothing -> assertBool name $ case r of
              Left{} -> True
              _ -> False

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

    -- query repo1 with index 1, don't ask for v0
    testQuery "multi 4a" repo1 schema_index_file_1 Nothing "x.P _" (Just 1)
    testQuery "multi 4b" repo1 schema_index_file_1 Nothing "y.Q _" (Just 1)
    testQuery "multi 4c" repo1 schema_index_file_1 Nothing
      "y.Q { p = { b = 3 }}" (Just 1)
    testQuery "multi 4d" repo1 schema_index_file_1 Nothing
      "derived.D _" (Just 1)

    -- query repo1 with index 1, explicitly ask for schema v1
    testQuery "multi 5a" repo1 schema_index_file_1 (Just "v1") "x.P _" (Just 1)
    testQuery "multi 5b" repo1 schema_index_file_1 (Just "v1") "y.Q _" (Just 1)
    testQuery "multi 5c" repo1 schema_index_file_1 (Just "v1")
      "y.Q { p = { b = 3 }}" (Just 1)
    testQuery "multi 5d" repo1 schema_index_file_1 (Just "v1")
      "derived.D _" (Just 1)

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

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [
    TestLabel "multiSchemaTest" multiSchemaTest
  ]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module SchemaTest (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bifunctor (first)
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import System.FilePath
import System.IO.Temp
import Data.Text.Prettyprint.Doc hiding ((<>))
import Test.HUnit

import System.Timeout
import TestRunner
import Util.Control.Exception
import Util.EventBase
import Util.String.Quasi

import Glean.Angle.Types (latestAngleVersion, Type_(..))
import Glean.Backend
import Glean.Database.Open
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Test
import Glean.Database.Types
import Glean.Database.Schema.Types
import Glean.Database.Schema
import Glean.Impl.TestConfigProvider
import Glean.Init
import qualified Glean.RTS as RTS
import qualified Glean.RTS.Term as RTS
import qualified Glean.RTS.Types as RTS
import Glean.Schema.Resolve
import Glean.Schema.Util
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Write.JSON

import TestDB

angleQuery :: Env -> Repo -> ByteString -> IO UserQueryResults
angleQuery env repo q = userQuery env repo $ mkAngleQuery q

mkBatch :: PredicateRef -> [ByteString] -> JsonFactBatch
mkBatch ref facts =
  JsonFactBatch
    { jsonFactBatch_predicate = ref
    , jsonFactBatch_facts = facts
    , jsonFactBatch_unit = Nothing
    }

mkAngleQuery :: ByteString -> UserQuery
mkAngleQuery q = def
  { userQuery_query = q
  , userQuery_options = Just def
    { userQueryOptions_syntax = QuerySyntax_ANGLE
    }
  }

-- Test that we can extend the schema with a derived predicate after
-- the DB has been created, and make a query using the new predicate.

mergeSchemaTest :: Test
mergeSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    -- create the DB with the default schema
    repo <- withTestDB [setRoot root] $ \_ -> return

    -- create an extended schema
    let newSchemaFile = root </> "schema"
    (source,_) <- parseSchemaDir schemaSourceDir
    writeFile newSchemaFile (show (pretty source))
    appendFile newSchemaFile
      [s|
        schema mergetest.1 {
          import glean.test.5
          # pick out the nat field from a glean.test.Predicate
          predicate JustNat : { predicate : glean.test.Predicate, nat : nat }
            {P,N} where P = glean.test.Predicate { nat = N }
        }
      |]

    -- query the existing DB using the extended schema
    withTestEnv [setRoot root, setSchemaPath newSchemaFile] $ \env -> do
       r <- try $ angleQuery env repo "mergetest.JustNat.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "merge" $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts == 4
         _ -> False


withSchemaFile :: Int -> String -> (FilePath -> FilePath -> IO a) -> IO a
withSchemaFile version str action = do
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let newSchemaFile = root </> "schema"
    appendFile newSchemaFile $ "version: " <> show version
    appendFile newSchemaFile str
    action root newSchemaFile

withSchema :: Int -> String -> (Either SomeException () -> IO a) -> IO a
withSchema version str action =
  withSchemaFile version str $ \root file -> do
    let settings =
          [ setRoot root
          , setSchemaPath file
          ]
    r <- tryAll $
      withEmptyTestDB settings $ \env repo ->
      withOpenDatabase env repo $ \_ ->
        return ()

    print (r :: Either SomeException ())
    action r

schemaUnversioned :: Test
schemaUnversioned = TestCase $ do
  let
    schema =
      [s|
          schema test.1 {
            predicate P : { a : nat }
          }

          schema test.2 {
            predicate P : { a : nat, b : nat }
          }

          schema all.1 : test.1 {}
          schema all.2 : test.2 {}
      |]

  let fill env repo =
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "test.P" 1)
              [ [s|{ "key" : { "a": 1 } }|] ]
          , mkBatch (PredicateRef "test.P" 2)
              [ [s|{ "key" : { "a": 1, "b": 1 } }|]
              , [s|{ "key" : { "a": 2, "b": 2 } }|]
              ]
          ]
          Nothing

  withSchemaFile latestAngleVersion schema $ \root file -> do
    let
      create repo propList =
        withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
          kickOffTestDB env repo id
          let props = HashMap.fromList propList
          void $ updateProperties env repo props []
          fill env repo
          completeTestDB env repo
          return repo

    repo1 <- create (Thrift.Repo "schematest-repo" "1") []
    repo2 <- create (Thrift.Repo "schematest-repo" "2")
      [("glean.schema_version", "1")]
    repo3 <- create (Thrift.Repo "schematest-repo" "3")
      [("glean.schema_version", "3")]

    withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.2,
      -- because the all.2 schema inherits from test.2 and we pick
      -- all.2 by default
      r <- try $ angleQuery env repo1 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 1" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 2
        _ -> False

    withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.1,
      -- if we set the schema_version field in the query to 1.
      r <- try $ userQuery env repo1 $
        (mkAngleQuery "test.P _") { userQuery_schema_version = Just 1 }
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 2" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 1
        _ -> False

    withTestEnv [setRoot root, setSchemaPath file, setSchemaVersion 1] $
      \env -> do
      -- Test that an unversioned query "test.P _" now resolves to
      -- test.P.1, because we're now asking for all.1 explicitly, and
      -- all.1 inherits from test.1
      r <- try $ angleQuery env repo1 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 3" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 1
        _ -> False

    withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.1
      -- when using repo2, which has the glean.schema_version:1 property
      -- set in its metadata.
      r <- try $ angleQuery env repo2 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 3" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 1
        _ -> False

    withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.2,
      -- because the all.3 schema doesn't exist so we pick all.2 by default
      r <- try $ angleQuery env repo3 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 4" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 2
        _ -> False


schemaTypeError :: Test
schemaTypeError = TestCase $ do
  let
    schema =
      [s|
        schema error.1 {
          predicate Test : { a : string, b : nat }
            {A,B} where {A,B} = {42, "xyz"}
        }

        schema all.1 : error.1 {}
      |]

  withSchema latestAngleVersion schema $ \r ->
    assertBool "schemaTypeError" $
      case r of
        Left e | Just (ErrorCall err) <- fromException e ->
          "type mismatch for variable A" `isInfixOf` err
        _ -> False


schemaStoredError :: Test
schemaStoredError = TestCase $ do
  let
    schema =
      [s|
        schema error.1 {
          predicate P : { a : string, b : nat }
            {"xyz", 42}

          predicate Q : P # should be an error to refer to P here
            stored P _
        }

        schema all.1 : error.1 {}
      |]

  withSchema latestAngleVersion schema $ \r -> do
    print r
    assertBool "schemaStoredError" $
      case r of
        Left e | Just (ErrorCall err) <- fromException e ->
          "stored predicate" `isInfixOf` err
        _ -> False


schemaValidation :: Test
schemaValidation = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let schema1 = root </> "schema1"
    appendFile schema1
      [s|
        schema test.1 {
          predicate P : { a : string, b : nat }
          predicate Q : { p : P }
        }

        schema test.2 : test.1 {
          predicate Q : { p : P }
        }

        schema all.1 : test.1 {}
      |]

    let schema2 = root </> "schema2"
    appendFile schema2
      [s|
        schema test.1 {
          predicate P : { b : nat, a : string  }  # field order matters
          predicate Q : { p : string }            # field types matter
        }

        schema test.2 : test.1 {
          predicate Q : { p : P }
        }

        schema all.1 : test.2 {}
      |]

    let schema3 = root </> "schema3"
    appendFile schema3
      [s|
        schema test.1 {
          predicate P : { a : string, b : nat }
          predicate Q : { p : P }
        }

        schema test.2 : test.1 {
          predicate P : { a : string, b : nat, c : {} }
          predicate Q : { p : P }  # now points to a different P
        }

        schema all.1 : test.2 {}
      |]

    withTestEnv [setRoot root, setSchemaPath schema1] $ \env -> do
      s <- B.readFile schema1
      validateSchema env (ValidateSchema s)

      s <- B.readFile schema2
      r <- try $ validateSchema env (ValidateSchema s)
      print r
      assertBool "validate2 - first failure" $ case r of
        Left err@Exception{} -> "test.P.1 has changed" `isInfixOf` show err
        _ -> False

      assertBool "validate2 - second failure failure" $ case r of
        Left err@Exception{} -> "test.Q.1 has changed" `isInfixOf` show err
        _ -> False

      s <- B.readFile schema3
      r <- try $ validateSchema env (ValidateSchema s)
      print r
      assertBool "validate3" $ case r of
        Left err@Exception{} -> "test.Q.2 has changed" `isInfixOf` show err
        _ -> False

schemaReservedWord :: Test
schemaReservedWord = TestCase $ do
  let
    schema =
      [s|
        schema error.1 {
          type R = { class : string }
        }

        schema all.1 : error.1 {}
      |]

  withSchema latestAngleVersion schema $ \r ->
    assertBool "schemaReservedWord" $
      case r of
        Left e -> "class is a reserved word" `isInfixOf` show e
        _ -> False

schemaUpperCaseField :: Test
schemaUpperCaseField = TestCase $ do
  let
    schema =
      [s|
        schema error.1 {
          type T = { Field : nat }
        }

        schema all.1 : error.1 {}
      |]

  withSchema latestAngleVersion schema $ \r ->
    assertBool "schemaUpperCaseField" $
      case r of
        Left e -> "field names must begin with a lowercase" `isInfixOf` show e
        _ -> False

schemaTypeShadowing :: Test
schemaTypeShadowing = TestCase $ do
  withSchemaAndFacts []
    [s|
      schema x.1 {
        type Q = string
        predicate P : Q
      }
      schema x.2 {
        type Q = nat
        predicate P: Q
      }
      schema all.1 : x.1, x.2 {}
    |]
    [ mkBatch (PredicateRef "x.P" 2)
        [ [s|{ "key": 1 }|]
        , [s|{ "key": 2 }|]
        ]
    ]
    [s| x.P (x.Q _) |]
    $ \_ response _ ->
      -- This used to silently default to the latest version, but now
      -- we give an ambiguity error.  It's your responsibility to make
      -- sure there are no overlapping names exposed by the "all"
      -- schema.
      case response of
        Left err | "ambiguous" `isInfixOf` show err ->
          assertBool "ambiguous identifier" True
        _ -> assertFailure (show response)

fakeSchemaKey :: Text
fakeSchemaKey = "glean/schema"

changeSchemaTest :: Test
changeSchemaTest = TestCase $ do
    let
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema all.1 : test.1 {}
        |]

      schema_v1 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema test.2 : test.1 {
            predicate P : { a : string, b : nat, c : {} }
          }
          schema all.1 : test.1, test.2 {}
        |]

    let repo = Thrift.Repo "schematest" "123"

    withEventBaseDataplane $ \evb -> do
      withConfigProvider defaultConfigOptions $ \confApi -> do
        setTestConfig confApi fakeSchemaKey schema_v0

        let
          dbConfig = def
            { cfgRoot = Nothing
            , cfgSchemaSource = ThriftSource.configWithDeserializer
                fakeSchemaKey parseAndResolveSchema
            , cfgServerConfig = ThriftSource.value def
                { ServerConfig.config_db_rocksdb_cache_mb = 0 }
            }

        withDatabases evb dbConfig (realConfigAPI confApi) $ \env -> do
          kickOffTestDB env repo id
          completeTestDB env repo

          info <- getSchemaInfo env repo
          assertBool "changeSchemaTest 1" $
            PredicateRef "test.P" 1 `elem`
              Map.elems (schemaInfo_predicateIds info)
          assertBool "changeSchemaTest 1" $
            PredicateRef "test.P" 2 `notElem`
              Map.elems (schemaInfo_predicateIds info)

          -- Now update the schema
          setTestConfig confApi fakeSchemaKey schema_v1
          -- wait for the schema to be updated for our repo
          let
            loop = do
              info <- getSchemaInfo env repo
              when (PredicateRef "test.P" 2 `notElem`
                  Map.elems (schemaInfo_predicateIds info)) $
                do putStrLn "waiting..."; threadDelay 1000000; loop
          r <- timeout 20000000 loop
          assertBool "changeSchemaTest 2" $ isJust r


-- | Trying to write to a predicate that was not in the schema when
-- the DB was created should fail.
writeEphemeralPredicate :: Test
writeEphemeralPredicate = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root <> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema all.1 : test.1 {}
        |]

      schema_v1_file = root <> "schema1"
      schema_v1 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema test.2 : test.1 {
            predicate P : { a : string, b : nat, c : {} }
          }

          schema all.1 : test.1, test.2 {}
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create the DB with the default schema
    repo <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file] $
      \_env repo -> return repo

    -- try to write a fact from the new schema
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
      -- this should work
      void $ syncWriteJsonBatch env repo
        [ mkBatch (PredicateRef "test.P" 1)
            [ "{ \"key\" : {} }" ]
        ]
        Nothing

      -- this should fail
      r <- try $ syncWriteJsonBatch env repo
        [ mkBatch (PredicateRef "test.P" 2)
            [ "{ \"key\" : {} }" ]
        ]
        Nothing
      print r
      assertBool "writeEphemeralPredicate" $ case r of
        Left e@Exception{} -> "not in scope" `isInfixOf` show e
        _ -> False


backwardCompatDeriving :: Test
backwardCompatDeriving = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root <> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema all.1 : test.1 {}
        |]

      schema_v1_file = root <> "schema1"
      schema_v1 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema test.2 : test.1 {
            predicate P : { a : string, b : nat, c : {} }

            derive test.P.1 default
              { A, B } where P.2 { A, B, _ }
          }
          schema all.1 : test.1 , test.2 {}
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create a DB with schema v0, create a fact of P.1
    repo0 <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file] $
      \env repo -> do
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "test.P" 1)
              [ "{ \"key\" : {} }" ]
          ]
          Nothing
        completeTestDB env repo
        return repo

    -- open the DB with schema_v1, we should still be able to query
    -- for P.1 and get one fact
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
       r <- try $ angleQuery env repo0 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "backcompat 1" $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts == 1
         _ -> False

       void $ deleteDatabase env repo0

    -- create a new DB with schema_v1, create a fact of P.2
    repo1 <- withEmptyTestDB [setRoot root, setSchemaPath schema_v1_file] $
      \env repo -> do
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "test.P" 2)
              [ "{ \"key\" : {} }" ]
          ]
          Nothing
        completeTestDB env repo
        return repo

    -- open the DB with schema_v1, we should be able to query for P.1 and P.2
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
       r <- try $ angleQuery env repo1 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "backcompat 2" $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts == 1
         _ -> False

       r <- try $ angleQuery env repo1 "test.P.2 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "backcompat 3" $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts == 1
         _ -> False

deriveDefault :: Test
deriveDefault = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root <> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema test.2 : test.1 {
            predicate P : { a : string, b : nat, c : {} }

            derive test.P.1 default
              { A, B } where P.2 { A, B, _ }

            derive test.P.2 default
              { A, B, {} } where test.P.1 { A, B }
          }

          schema all.1 : test.1, test.2 {}
        |]

    writeFile schema_v0_file schema_v0

    let
      -- we should be able to query for both P.1 and P.2
      checkQuery repo ss = do
        let settings = setRoot root : setSchemaPath schema_v0_file : ss
        withTestEnv settings $ \env -> do
           r <- try $ angleQuery env repo "test.P.1 _"
           print (r :: Either BadQuery UserQueryResults)
           assertBool "backcompat 1" $ case r of
             Right UserQueryResults{..} -> length userQueryResults_facts == 1
             _ -> False

           r <- try $ angleQuery env repo "test.P.2 _"
           print (r :: Either BadQuery UserQueryResults)
           assertBool "backcompat 2" $ case r of
             Right UserQueryResults{..} -> length userQueryResults_facts == 1
             _ -> False

      deleteDB repo =
        withTestEnv [setRoot root, setSchemaPath schema_v0_file] $ \env ->
           void $ deleteDatabase env repo

    let
      mkP version env repo =
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "test.P" version)
              [ "{ \"key\" : {} }" ]
          ]
          Nothing

      mkRepo hash facts =
        withTestEnv [setRoot root, setSchemaPath schema_v0_file] $ \env -> do
          let repo = Repo "test" hash
          kickOffTestDB env repo id
          facts env repo
          completeTestDB env repo
          return repo

    -- create a DB with a fact of P.1
    repo0 <- mkRepo "1" (mkP 1)
    checkQuery repo0 []

    -- create a DB with a fact of P.2
    repo1 <- mkRepo "2" (mkP 2)
    checkQuery repo1 []

    -- create a DB with no facts
    repo2 <- mkRepo "3" (\_ _ -> return ())
    withTestEnv [setRoot root, setSchemaPath schema_v0_file] $ \env -> do
       r <- try $ angleQuery env repo2 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "backcompat 3" $ case r of
         Left e@BadQuery{} -> "recursive" `isInfixOf` show e
         _ -> False
    deleteDB repo2

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

  , TestLabel "cannot remove field" $ TestCase $ do
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate P : { a : nat, b: string }
        }
        schema test.2 {
          predicate P : { a : nat }
        }
        schema test.2 evolves test.1
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "throws error stating missing field" $
        case r of
          Left err ->
            "cannot evolve predicate test.P: field missing: b"
            `isInfixOf` show err
          Right _ -> False

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

  , TestLabel "cannot remove predicate" $ TestCase $ do
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
      assertBool "errors stating missing predicate" $
        case r of
          Left err -> "missing evolved predicate" `isInfixOf` show err
          Right _ -> False

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

  , TestLabel "cannot remove option" $ TestCase $ do
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
      assertBool "error states missing option" $
        case r of
          Left err ->
            "option missing: c"
            `isInfixOf` show err
          Right _ -> False


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
  [ TestLabel "backcompat - remove field" $ TestCase $ do
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

withSchemaAndFacts
  :: [Setting]
  -> String                    -- ^ schema
  -> [JsonFactBatch]           -- ^ db contents
  -> Text                    -- ^ initial query
  -> ( DbSchema
    -> Either BadQuery UserQueryResults                  -- query response
    -> (Text -> IO (Either BadQuery UserQueryResults)) -- run more queries
    -> IO a )
  -> IO a
withSchemaAndFacts customSettings schema facts query act =
  withSchemaFile latestAngleVersion schema $ \root file -> do
  let settings =
        [ setRoot root
        , setSchemaPath file
        ] ++ customSettings

  -- create db and write facts
  repo <- withEmptyTestDB settings $ \env repo -> do
      void $ syncWriteJsonBatch env repo facts Nothing
      completeTestDB env repo
      return repo

  -- get PredicateDetails
  dbSchema <- do
    (sourceSchemas, schemas) <- either error return
      $ parseAndResolveSchema $ encodeUtf8 $ pack schema
    newDbSchema sourceSchemas schemas readWriteContent

  let run q = do
        -- open db for querying
        -- We need to open the db again because schema evolutions are
        -- only triggered when the db is read-only
        response <- withTestEnv settings $ \env ->
          try $ runQuery env repo (encodeUtf8 q)
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
        }
      , userQuery_encodings = [ UserQueryEncoding_bin def ]
      }


schemaNegation :: [Test]
schemaNegation =
  [ TestLabel "negation - derived" $ TestCase $ do
    -- a derived predicate can use negation
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate Base : string
          predicate Derived : string
            A where Base A; !(A = "x"..);
        }
        schema all.1 : test.1 {}
      |]
      $ \r ->
      assertBool "schemaNegation - derived" $
        case r of
          Left _ -> False
          Right _ -> True

  , TestLabel "negation - stored" $ TestCase $ do
    -- a stored predicate cannot use negation
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate Base : string
          predicate Stored : string
            stored A where Base A; !(A = "x"..);
        }
        schema all.1 : test.1 {}
      |]
      $ \r ->
      assertBool "schemaNegation - stored" $
        case r of
          Left e -> "use of negation is not allowed in a stored predicate"
            `isInfixOf` show e
          _ -> False

  , TestLabel "negation - stored if statements" $ TestCase $ do
    -- a stored predicate cannot use if statements
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate Base : string
          predicate Stored : string
            stored A where A = if (Base B) then B else "x"
        }
        schema all.1 : test.1 {}
      |]
      $ \r ->
      assertBool "schemaNegation - stored" $
        case r of
          Left e -> "use of if-statements is not allowed in a stored predicate"
            `isInfixOf` show e
          _ -> False

  , TestLabel "negation - stored dependency" $ TestCase $ do
    -- a stored predicate cannot depend on a derived
    -- predicate that uses negation
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate Base : string
          predicate Derived : string
            A where Base A; !(A = "x"..);
          predicate Stored : string
            stored A where Derived A
        }
        schema all.1 : test.1 {}
      |]
      $ \r ->
      assertBool "schemaNegation - stored dependency" $
        case r of
          Left e -> "negation is not allowed in a stored predicate"
            `isInfixOf` show e
          _ -> False

  , TestLabel "negation - stored dependency cross-schema" $ TestCase $ do
    -- use of negation is detected across schemas
    withSchema latestAngleVersion
      [s|
        schema test.1 {
          predicate Base : string
          predicate Derived : string
            A where Base A; !(A = "x"..);
        }
        schema test.2 : test.1 {
          predicate Stored : string
            stored A where Derived A
        }
        schema all.1 : test.1, test.2 {}
      |]
      $ \r ->
      assertBool "schemaNegation - stored dependency cross-schema" $
        case r of
          Left e -> "negation is not allowed in a stored predicate"
            `isInfixOf` show e
          _ -> False
  ]

thinSchemaTest :: Test
thinSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root <> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
          }

          schema x.1 {
            predicate P : nat
          }

          schema all.1 : test.1, x.1 {}
        |]

      schema_v1_file = root <> "schema1"
      schema_v1 =
        [s|
          # delete schema test.1
          # to test that it is still stored in the DB

          schema x.1 {
            predicate P : string  # changed from nat to string
          }

          # This will be a type error if the old x.1 is still present
          schema x.2 : x.1 {
            predicate Q : string
              S where P S
          }

          schema all.1 : x.1, x.2 {}
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create a DB with schema v0, create a fact of test.P.1
    repo0 <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file]
      $ \env repo -> do
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "test.P" 1)
              [ "{ \"key\" : {} }" ]
          ]
          Nothing
        completeTestDB env repo
        return repo

    -- open the DB with schema_v1, we should still be able to query
    -- for test.P.1 and get one fact
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
       r <- try $ angleQuery env repo0 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertEqual "thin 1" 1 $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts
         _ -> error "bad query"

       void $ deleteDatabase env repo0

main :: IO ()
main = withUnitTest $ testRunner $ TestList $
  [ TestLabel "mergeSchemaTest" mergeSchemaTest
  , TestLabel "schemaTypeError" schemaTypeError
  , TestLabel "schemaStoredError" schemaStoredError
  , TestLabel "schemaValidation" schemaValidation
  , TestLabel "schemaReservedWord" schemaReservedWord
  , TestLabel "schemaUpperCaseField" schemaUpperCaseField
  , TestLabel "schemaTypeShadowing" schemaTypeShadowing
  , TestLabel "changeSchema" changeSchemaTest
  , TestLabel "writeEphemeralPredicate" writeEphemeralPredicate
  , TestLabel "backwardCompatDeriving" backwardCompatDeriving
  , TestLabel "deriveDefault" deriveDefault
  , TestLabel "thinSchema" thinSchemaTest
  , TestLabel "schemaUnversioned" schemaUnversioned
  , TestLabel "schemaEvolves"  schemaEvolves
  , TestLabel "schemaEvolvesTransformations" schemaEvolvesTransformations
  ] ++ schemaNegation

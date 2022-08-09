{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module Schema.Basic (main) where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO.Temp
import Data.Text.Prettyprint.Doc hiding ((<>))
import Test.HUnit

import System.Timeout
import TestRunner
import Util.EventBase
import Util.String.Quasi

import Glean.Angle.Types (latestAngleVersion)
import Glean.Backend
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Test
import Glean.Impl.ConfigProvider ()
import Glean.Impl.TestConfigProvider
import Glean.Init
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Write.JSON

import TestDB
import Schema.Lib

-- Test that we can extend the schema with a derived predicate after
-- the DB has been created, and make a query using the new predicate.

mergeSchemaTest :: Test
mergeSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    -- create the DB with the default schema
    repo <- withTestDB [setRoot root] $ \_ -> return

    -- create an extended schema
    let newSchemaFile = root </> "schema"
    SchemaIndex{..} <- parseSchemaDir schemaSourceDir
    writeFile newSchemaFile $ show $ pretty $
      procSchemaSource schemaIndexCurrent
    appendFile newSchemaFile
      [s|
        schema mergetest.1 {
          import glean.test.5
          # pick out the nat field from a glean.test.Predicate
          predicate JustNat : { predicate : glean.test.Predicate, nat : nat }
            {P,N} where P = glean.test.Predicate { nat = N }
        }

        schema all.99999: mergetest.1 {}
      |]

    -- query the existing DB using the extended schema
    withTestEnv [setRoot root, setSchemaPath newSchemaFile] $ \env -> do
      r <- try $ angleQuery env repo "mergetest.JustNat _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "merge" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 4
        _ -> False


-- This test is superseded by the tests in Schema/Multi.hs, we should retire
-- it when support for use_schema_id = false is removed.
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

    withTestEnv [setRoot root, setSchemaPath file] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.2,
      -- because the all.2 schema inherits from test.2 and we pick
      -- all.2 by default
      r <- try $ angleQuery env repo1 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 1" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 2
        _ -> False

    withTestEnv [setRoot root, setSchemaPath file, disableSchemaId] $ \env -> do
      -- Test that an unversioned query "test.P _" resolves to test.P.1,
      -- if we set the schema_version field in the query to 1.
      r <- try $ userQuery env repo1 $
        (mkAngleQuery "test.P _") { userQuery_schema_version = Just 1 }
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 2" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 1
        _ -> False

    withTestEnv [
        setRoot root,
        setSchemaPath file,
        setSchemaVersion 1,
        disableSchemaId ] $ \env -> do
      -- Test that an unversioned query "test.P _" now resolves to
      -- test.P.1, because we're now asking for all.1 explicitly, and
      -- all.1 inherits from test.1
      r <- try $ angleQuery env repo1 "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 3" $ case r of
        Right UserQueryResults{..} -> length userQueryResults_facts == 1
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

    withTestEnv [
        setRoot root,
        setSchemaPath schema1,
        disableSchemaId ] $ \env -> do
          -- disableSchemaId because schema validation isn't performed
          -- when use_schema_id is on. We can retire this test when we
          -- retire the schema validation code.
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
                fakeSchemaKey (processOneSchema Map.empty)
            , cfgServerConfig = ThriftSource.value def
                { ServerConfig.config_db_rocksdb_cache_mb = 0 }
            }

        withDatabases evb dbConfig (realConfigAPI confApi) $ \env -> do
          kickOffTestDB env repo id
          completeTestDB env repo

          info <- getSchemaInfo env repo
            def { getSchemaInfo_omit_source = True }
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
                def { getSchemaInfo_omit_source = True }
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
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema all.1 : test.1 {}
        |]

      schema_v1_file = root </> "schema1"
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
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
          schema all.1 : test.1 {}
        |]

      schema_v1_file = root </> "schema1"
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
      schema_v0_file = root </> "schema0"
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
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }

            # tickle a bug in which we used the wrong type environment
            # when checking predicates in the stored schema
            type T = string
            predicate Q : T stored "abc"
          }

          schema x.1 {
            predicate P : nat
          }

          schema all.1 : test.1, x.1 {}
        |]

      schema_v1_file = root </> "schema1"
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

    -- open the DB with schema_v1, we should not be able to see
    -- test.P.1 any more when we query using the default schema
    -- version.
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
       r <- try $ angleQuery env repo0 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "thin 1" $ case r of
         Left BadQuery{} -> True
         _ -> False

stackedSchemaTest :: Test
stackedSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema x.1 {
            predicate P : string
          }

          schema y.1 {
            predicate Q : string
          }

          schema all.1 : x.1, y.1 {}
        |]
    writeFile schema_v0_file schema_v0

    -- Test a stacked DB:
    --    base: 1 fact of x.P
    --    stacked: 1 fact of y.Q
    -- make sure that we can query x.P on the stacked DB.
    --
    -- This tickled a bug in the schema handling at one point, where
    -- we were over-eagerly removing schemas from the base DB and then
    -- assigning the wrong Pid to the predicates in the stacked DB.

    withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file]
      $ \env repo -> do
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "x.P" 1)
              [ "{ \"key\" : \"a\" }" ]
          ]
          Nothing
        completeTestDB env repo

        let stacked = Repo "stacked" "0"
        kickOffTestDB env stacked $ \x -> x
          { Thrift.kickOff_dependencies =
             Just $ Thrift.Dependencies_stacked repo }
        void $ syncWriteJsonBatch env stacked
          [ mkBatch (PredicateRef "y.Q" 1)
              [ "{ \"key\" : \"b\" }" ]
          ]
          Nothing
        completeTestDB env stacked

        r <- try $ angleQuery env stacked "x.P _"
        print (r :: Either BadQuery UserQueryResults)
        assertEqual "stacked 1" 1 $ case r of
          Right UserQueryResults{..} -> length userQueryResults_facts
          _ -> error "bad query"

        r <- try $ angleQuery env stacked "y.Q _"
        print (r :: Either BadQuery UserQueryResults)
        assertEqual "stacked 2" 1 $ case r of
          Right UserQueryResults{..} -> length userQueryResults_facts
          _ -> error "bad query"


-- | Test that we can extend the schema when creating a stacked DB
stackedSchemaUpdateTest :: Test
stackedSchemaUpdateTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0_file = root </> "schema0"
      schema_v0 =
        [s|
          schema x.1 {
            predicate P : { a : string }
          }

          schema y.1 {
            predicate Q : string
          }

          schema all.1 : x.1, y.1 {}
        |]
    writeFile schema_v0_file schema_v0

    -- v1 adds a derived predicate to v0
    let
      schema_v1_file = root </> "schema1"
      schema_v1 =
        [s|
          schema x.1 {
            predicate P : { a : string }
          }

          schema y.1 {
            predicate Q : { a : string }

            predicate R : string
          }

          schema all.1 : x.1, y.1 {}
        |]
    writeFile schema_v1_file schema_v1

    -- v2 is incompatible with v0 (x.P changed)
    let
      schema_v2_file = root </> "schema2"
      schema_v2 =
        [s|
          schema x.1 {
            predicate P : { a : string, b : nat }
          }

          schema y.1 {
            predicate Q : { a : string }
          }

          schema all.1 : x.1, y.1 {}
        |]
    writeFile schema_v2_file schema_v2

    -- v3 is incompatible with v0 (y.Q changed),
    -- but we should still be able to use it because we only have
    -- facts of x.P and that didn't change.
    let
      schema_v3_file = root </> "schema3"
      schema_v3 =
        [s|
          schema x.1 {
            predicate P : { a : string }
          }

          schema y.1 {
            predicate Q : { a : string, b : nat }
          }

          schema all.1 : x.1, y.1 {}
        |]
    writeFile schema_v3_file schema_v3

    let dbRoot = root </> "db"
    createDirectory dbRoot

    let
      mkRepo schema hash upd facts =
        withTestEnv [
            setRoot dbRoot,
            setSchemaPath schema ] $ \env -> do
          let repo = Repo "test" hash
          kickOffTestDB env repo upd
          facts env repo
          completeTestDB env repo
          return repo

      testQuery name repo schema query result =
        withTestEnv [
            setRoot dbRoot,
            setSchemaPath schema ] $ \env -> do
          r <- try $ angleQuery env repo query
          case result of
            Just n -> case r :: Either BadQuery UserQueryResults of
              Right UserQueryResults{..} ->
                assertEqual name n (length userQueryResults_facts)
              _ -> assertFailure (name <> ": " <> show r)
            Nothing -> assertBool name $ case r of
              Left{} -> True
              _ -> False

    repo0 <- mkRepo schema_v0_file "0" id $ \env repo ->
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "x.P" 1)
              [ [s| { "key" : { "a" : "x" } } |] ]
          ] Nothing

    -- switch to schema v1, make a stacked DB, we shouldn't be able to
    -- make a y.R fact
    let set x = x { Thrift.kickOff_dependencies =
           Just $ Thrift.Dependencies_stacked repo0 }
    r <- try $ mkRepo schema_v1_file "1" set $ \env repo ->
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "y.R" 1)
              [ [s| { "key" : { "a" : "x" } } |] ]
          ] Nothing
    print r
    assertBool "stacked schema 0" $
      case r of
        Left (e :: Thrift.Exception) -> "not in scope: y.R" `isInfixOf` show e
        _ -> False

    -- switch to schema v1, make a stacked DB with update_schema_for_stacked
    let set x = x {
          Thrift.kickOff_dependencies =
            Just $ Thrift.Dependencies_stacked repo0,
          Thrift.kickOff_update_schema_for_stacked = True }
    repo1 <- mkRepo schema_v1_file "2" set $ \env repo ->
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "y.R" 1)
              [ [s| { "key" : "abc" } |] ]
          ] Nothing

    testQuery "stacked schema 1" repo1 schema_v1_file "x.P _" (Just 1)
    testQuery "stacked schema 2" repo1 schema_v1_file "y.Q _" (Just 0)
    testQuery "stacked schema 3" repo1 schema_v1_file "y.R _" (Just 1)

    -- when the current schema is incompatible with the schema in the
    -- base DB, creating the stacked DB with update_schema_for_stacked
    -- should fail.
    let set x = x {
          Thrift.kickOff_dependencies =
            Just $ Thrift.Dependencies_stacked repo0,
          Thrift.kickOff_update_schema_for_stacked = True }
    r <- try $ mkRepo schema_v2_file "3" set $ \env repo ->
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "y.R" 1)
              [ [s| { "key" : "abc" } |] ]
          ] Nothing
    assertBool "stacked schema 4" $
      case r of
        Left (e :: Thrift.Exception) -> "incompatible" `isInfixOf` show e
        _ -> False

    -- switch to schema v3, make a stacked DB with update_schema_for_stacked
    let set x = x {
          Thrift.kickOff_dependencies =
            Just $ Thrift.Dependencies_stacked repo0,
          Thrift.kickOff_update_schema_for_stacked = True }
    repo4 <- mkRepo schema_v3_file "4" set $ \env repo ->
        void $ syncWriteJsonBatch env repo
          [ mkBatch (PredicateRef "y.Q" 1)
              [ [s| { "key" : {} } |] ]
          ] Nothing

    testQuery "stacked schema 5" repo4 schema_v3_file "x.P _" (Just 1)
    testQuery "stacked schema 6" repo4 schema_v3_file "y.Q _" (Just 1)


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
  , TestLabel "stackedSchemaTest" stackedSchemaTest
  , TestLabel "stackedSchemaUpdateTest" stackedSchemaUpdateTest
  ] ++ schemaNegation

{-# LANGUAGE QuasiQuotes #-}
module SchemaTest (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import System.FilePath
import System.IO.Temp
import Data.Text.Prettyprint.Doc hiding ((<>))
import Test.HUnit

import System.Timeout
import TestRunner
import Util.Control.Exception
import Util.EventBase
import Util.String.Quasi

import Glean.Angle.Types (latestAngleVersion)
import Glean.Backend
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Stuff
import Glean.Database.Test
import Glean.Database.Types
import Glean.Impl.TestConfigProvider
import Glean.Init
import Glean.Schema.Resolve
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Write.SendBatch

import TestDB

angleQuery :: Env -> Repo -> ByteString -> IO UserQueryResults
angleQuery env repo q =
  userQuery env repo def
    { userQuery_query = q
    , userQuery_options = Just def
      { userQueryOptions_syntax = QuerySyntax_ANGLE }
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
          import glean.test.4
          # pick out the nat field from a glean.test.Predicate
          predicate JustNat : { predicate : glean.test.Predicate, nat : nat }
            {P,N} where P = glean.test.Predicate { nat = N }
        }

        schema all.3 : all.2, mergetest.1 {}
      |]

    -- query the existing DB using the extended schema
    withTestEnv [setRoot root, setSchemaPath newSchemaFile] $ \env -> do
       r <- try $ angleQuery env repo "mergetest.JustNat _"
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
    r <- tryAll $ withEmptyTestDB [setRoot root, setSchemaPath file] $
      \env repo -> withOpenDatabase env repo $ \_ -> return ()

    print (r :: Either SomeException ())
    action r


schemaUnversioned :: Test
schemaUnversioned = TestCase $ do
  let
    schema =
      [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }

          schema test.2 {
            predicate P : { a : string, b : nat, c : bool }
            predicate Q : { p : P }
          }

          schema all.1 : test.1 {}
      |]

  withSchemaFile latestAngleVersion schema $ \root file -> do
    withEmptyTestDB [setRoot root, setSchemaPath file] $ \env repo -> do
      void $ sendJsonBatch env repo
        [ JsonFactBatch
            { jsonFactBatch_predicate = PredicateRef "test.P" 1
            , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
            }
        ]
        Nothing

      -- Test that an unversioned query "test.P _" resolves to test.P.1,
      -- because the all.1 schema inherits from test.1
      r <- try $ angleQuery env repo "test.P _"
      print (r :: Either BadQuery UserQueryResults)
      assertBool "unversioned 1" $ case r of
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
          predicate Q : { p : P }
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
      assertBool "validate2" $ case r of
        Left err@Exception{} -> "test.P.1 has changed" `isInfixOf` show err
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

  withSchema 2 schema $ \r ->
    assertBool "schemaReservedWord" $ isRight r

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

  withSchema 3 schema $ \r ->
    assertBool "schemaUpperCaseField" $ isRight r

  withSchema latestAngleVersion schema $ \r ->
    assertBool "schemaUpperCaseField" $
      case r of
        Left e -> "field names must begin with a lowercase" `isInfixOf` show e
        _ -> False

fakeSchemaKey :: Text
fakeSchemaKey = "glean/schema"

changeSchemaTest :: Test
changeSchemaTest = TestCase $
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let
      schema_v0 =
        [s|
          schema test.1 {
            predicate P : { a : string, b : nat }
            predicate Q : { p : P }
          }
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
        |]

    let repo = Thrift.Repo "schematest" "123"

    withEventBaseDataplane $ \evb -> do
      withConfigProvider defaultConfigOptions $ \confApi -> do
        setTestConfig confApi fakeSchemaKey schema_v0

        let
          dbConfig = def
            { cfgRoot = root
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
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create the DB with the default schema
    repo <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file] $
      \_env repo -> return repo

    -- try to write a fact from the new schema
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
      -- this should work
      void $ sendJsonBatch env repo
        [ JsonFactBatch
            { jsonFactBatch_predicate = PredicateRef "test.P" 1
            , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
            }
        ]
        Nothing

      -- this should fail
      r <- try $ sendJsonBatch env repo
        [ JsonFactBatch
            { jsonFactBatch_predicate = PredicateRef "test.P" 2
            , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
            }
        ]
        Nothing
      print r
      assertBool "writeEphemeralPredicate" $ case r of
        Left e@Exception{} -> "unknown predicate" `isInfixOf` show e
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

            derive test.P.1
              { A, B } where P.2 { A, B, _ }
          }
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create a DB with schema v0, create a fact of P.1
    repo0 <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file] $
      \env repo -> do
        void $ sendJsonBatch env repo
          [ JsonFactBatch
              { jsonFactBatch_predicate = PredicateRef "test.P" 1
              , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
              }
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
        void $ sendJsonBatch env repo
          [ JsonFactBatch
              { jsonFactBatch_predicate = PredicateRef "test.P" 2
              , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
              }
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
        void $ sendJsonBatch env repo
          [ JsonFactBatch
              { jsonFactBatch_predicate = PredicateRef "test.P" version
              , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
              }
          ]
          Nothing

      mkRepo facts =
        withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file] $
          \env repo -> do
            facts env repo
            completeTestDB env repo
            return repo

    -- create a DB with a fact of P.1
    repo0 <- mkRepo (mkP 1)
    checkQuery repo0 []

    -- check that enabling --db-schema-override doesn't break this
    checkQuery repo0 [setSchemaOverride]
    deleteDB repo0

    -- create a DB with a fact of P.2
    repo1 <- mkRepo (mkP 2)
    checkQuery repo1 []

    -- check that enabling --db-schema-override doesn't break this
    checkQuery repo1 [setSchemaOverride]
    deleteDB repo1

    -- create a DB with no facts
    repo2 <- mkRepo (\_ _ -> return ())
    withTestEnv [setRoot root, setSchemaPath schema_v0_file] $ \env -> do
       r <- try $ angleQuery env repo2 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "backcompat 3" $ case r of
         Left e@BadQuery{} -> "recursive" `isInfixOf` show e
         _ -> False
    deleteDB repo2


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
        |]

    writeFile schema_v0_file schema_v0
    writeFile schema_v1_file schema_v1

    -- create a DB with schema v0, create a fact of test.P.1
    repo0 <- withEmptyTestDB [setRoot root, setSchemaPath schema_v0_file]
      $ \env repo -> do
        void $ sendJsonBatch env repo
          [ JsonFactBatch
              { jsonFactBatch_predicate = PredicateRef "test.P" 1
              , jsonFactBatch_facts = [ "{ \"key\" : {} }" ]
              }
          ]
          Nothing
        completeTestDB env repo
        return repo

    -- open the DB with schema_v1, we should still be able to query
    -- for test.P.1 and get one fact
    withTestEnv [setRoot root, setSchemaPath schema_v1_file] $ \env -> do
       r <- try $ angleQuery env repo0 "test.P.1 _"
       print (r :: Either BadQuery UserQueryResults)
       assertBool "thin 1" $ case r of
         Right UserQueryResults{..} -> length userQueryResults_facts == 1
         _ -> False

       void $ deleteDatabase env repo0


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "mergeSchemaTest" mergeSchemaTest
  , TestLabel "schemaTypeError" schemaTypeError
  , TestLabel "schemaStoredError" schemaStoredError
  , TestLabel "schemaValidation" schemaValidation
  , TestLabel "schemaReservedWord" schemaReservedWord
  , TestLabel "schemaUpperCaseField" schemaUpperCaseField
  , TestLabel "changeSchema" changeSchemaTest
  , TestLabel "writeEphemeralPredicate" writeEphemeralPredicate
  , TestLabel "backwardCompatDeriving" backwardCompatDeriving
  , TestLabel "deriveDefault" deriveDefault
  , TestLabel "thinSchema" thinSchemaTest
  , TestLabel "schemaUnversioned" schemaUnversioned
  ]

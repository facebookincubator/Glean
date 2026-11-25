{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
-- | Resuming a query continuation after the server's schema has changed.
module Schema.ResumeAfterUpdate (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Default
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Text (Text)
import Test.HUnit

import System.Timeout
import TestRunner
import Util.EventBase
import Util.String.Quasi

import Glean.Backend.Types
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Open (readDatabase)
import Glean.Database.Schema.Types (DbSchema(..))
import Glean.Database.Test
import Glean.Database.Types (Env, OpenDB(..))
import Glean.Impl.ConfigProvider ()
import Glean.Impl.TestConfigProvider
import Glean.Init
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Query
import Glean.RTS.Foreign.Stacked (stacked)
import Glean.RTS.Types (Fid(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Write.JSON

import Schema.Lib

schemaKey :: Text
schemaKey = "glean/schema"

storedSchema :: ByteString
storedSchema =
  [s|
    schema test.1 {
      predicate P : { a : string, b : nat }
    }
  |]

-- deriv.D is not in the DB's stored schema, so its Pid is assigned
-- when the server merges its schema with the DB's.
derivSchema :: ByteString
derivSchema =
  [s|
    schema deriv.1 : test.1 {
      predicate D : nat
        B where test.P { b = B }
    }
  |]

-- More predicates that are not in the DB's stored schema. Adding or
-- removing them re-assigns the Pids of the merged schema.
otherSchema :: ByteString
otherSchema =
  [s|
    schema other.1 : test.1 {
      predicate S1 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S2 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S3 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S4 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S5 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S6 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S7 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
      predicate S8 : { s : string, p : test.P } { A, P } where P = test.P { a = A }
    }
  |]

storedOnly, withDeriv, withDerivAndOther :: ByteString
storedOnly = storedSchema <> "schema all.1 : test.1 {}\n"
withDeriv = storedSchema <> derivSchema <> "schema all.1 : test.1, deriv.1 {}\n"
withDerivAndOther = storedSchema <> derivSchema <> otherSchema
  <> "schema all.1 : test.1, deriv.1, other.1 {}\n"

derivD, otherS1 :: PredicateRef
derivD = PredicateRef "deriv.D" 1
otherS1 = PredicateRef "other.S1" 1

predicateIds :: Env -> Repo -> IO (Map.Map Int PredicateRef)
predicateIds env repo = do
  info <- getSchemaInfo env (Just repo) def
    { getSchemaInfo_omit_source = True }
  return $ Map.mapKeys fromIntegral (schemaInfo_predicateIds info)

pidOf :: Env -> Repo -> PredicateRef -> IO Int
pidOf env repo ref = do
  ids <- predicateIds env repo
  case [ pid | (pid, r) <- Map.toList ids, r == ref ] of
    [pid] -> return pid
    other -> assertFailure ("pids for " <> show ref <> ": " <> show other)

hasPredicate :: PredicateRef -> Map.Map Int PredicateRef -> Bool
hasPredicate ref ids = ref `elem` Map.elems ids

-- | Change the server's schema and wait until the open DB's schema has
-- been merged with it.
updateSchema
  :: TestConfigAPI
  -> Env
  -> Repo
  -> ByteString
  -> (Map.Map Int PredicateRef -> Bool)
  -> IO ()
updateSchema confApi env repo schema done = do
  setTestConfig confApi schemaKey schema
  let loop = do
        ids <- predicateIds env repo
        unless (done ids) $ do
          threadDelay 100000
          loop
  r <- timeout 30000000 loop
  assertBool "schema update was not applied" (r == Just ())

-- | A DB created with 'storedOnly', served read-only by a server whose
-- schema the test can change.
withLiveSchemaDB :: (TestConfigAPI -> Env -> Repo -> IO ()) -> IO ()
withLiveSchemaDB act =
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \confApi -> do
    setTestConfig confApi schemaKey storedOnly
    let
      repo = Thrift.Repo "resumetest" "1"
      dbConfig = def
        { cfgDataStore = tmpDataStore
        , cfgSchemaLocation = Just (SchemaLocation_config schemaKey)
        , cfgServerConfig = ThriftSource.value def
            { ServerConfig.config_db_rocksdb_cache_mb = 0 }
        }
    withDatabases evb dbConfig (realConfigAPI confApi) $ \env -> do
      kickOffTestDB env repo id
      void $ syncWriteJsonBatch env repo
        [ mkBatch (PredicateRef "test.P" 1)
            [ [s|{ "key" : { "a" : "x", "b" : 1 } }|]
            , [s|{ "key" : { "a" : "y", "b" : 2 } }|]
            , [s|{ "key" : { "a" : "z", "b" : 3 } }|]
            ]
        ]
        Nothing
      completeTestDB env repo
      act confApi env repo

-- | One result per page, expanding nested facts.
pageOf :: ByteString -> Maybe UserQueryCont -> UserQuery
pageOf q cont = (mkAngleQuery q)
  { userQuery_options = Just def
    { userQueryOptions_syntax = QuerySyntax_ANGLE
    , userQueryOptions_max_results = Just 1
    , userQueryOptions_recursive = True
    , userQueryOptions_continuation = cont
    }
  }

firstPage :: Env -> Repo -> ByteString -> IO UserQueryCont
firstPage env repo q = do
  page <- userQuery env repo (pageOf q Nothing)
  assertEqual "first page" 1 (length (userQueryResults_facts page))
  maybe (assertFailure "no continuation") return
    (userQueryResults_continuation page)

-- | Run the continuation's bytecode against the DB's current inventory,
-- below any checks UserQuery makes before restarting it.
restartBytecode :: Env -> Repo -> UserQueryCont -> IO QueryResults
restartBytecode env repo cont =
  readDatabase env repo $ \odb lookup -> do
    derived <- FactSet.new (Fid (userQueryCont_nextId cont))
    restartCompiled
      (schemaInventory (odbSchema odb))
      Nothing
      (stacked lookup derived)
      Nothing
      QueryRuntimeOptions
        { queryMaxResults = Just 1
        , queryMaxBytes = Nothing
        , queryMaxTimeMs = Nothing
        , queryMaxSetSize = Nothing
        , queryDepth = ExpandRecursive
        , queryWantStats = False
        }
      (userQueryCont_continuation cont)

-- After a schema update removes predicates, the Pid that the restarted
-- bytecode gives the derived deriv.D facts it nests in its results
-- names no predicate.
restartWithUnknownNestedPid :: ByteString -> Test
restartWithUnknownNestedPid q = TestCase $
  withLiveSchemaDB $ \confApi env repo -> do
    updateSchema confApi env repo withDerivAndOther (hasPredicate otherS1)
    pid <- pidOf env repo derivD
    cont <- firstPage env repo q

    updateSchema confApi env repo withDeriv (not . hasPredicate otherS1)
    ids <- predicateIds env repo
    assertBool "precondition: deriv.D's old Pid names no predicate"
      (Map.notMember pid ids)

    -- the restart's first derived fact is the deriv.D one it nests
    let expected = "unknown pid " <> show pid <> " for nested fact "
          <> show (userQueryCont_nextId cont)
    r <- try $ restartBytecode env repo cont
    case r of
      Left (e :: SomeException) ->
        assertBool ("expected " <> show expected <> ", got: " <> show e)
          (expected `isInfixOf` show e)
      Right _ -> assertFailure "restart did not report the unknown Pid"

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "restartWithUnknownNestedPid" $
      restartWithUnknownNestedPid "{ X, B } where X = deriv.D B"
  -- the nested test.P fact is expanded before the deriv.D one
  , TestLabel "restartWithUnknownNestedPidAfterAnother" $
      restartWithUnknownNestedPid
        "{ X, P } where P = test.P { b = B }; X = deriv.D B"
  -- the deriv.D fact is expanded while the nested test.P one is pending
  , TestLabel "restartWithUnknownNestedPidBeforeAnother" $
      restartWithUnknownNestedPid
        "{ P, X } where P = test.P { b = B }; X = deriv.D B"
  ]

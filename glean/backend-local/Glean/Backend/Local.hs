{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--
-- | An abstraction over the Glean API that can be served by
-- either a local DB or a remote server.
--
module Glean.Backend.Local
  (
    Env(..)
  , Config(..)
  , options
  , withDatabases

    -- * Schemas
  , loadDbSchema
  , serializeInventory
  ) where

import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Stream (stream)
import Control.Exception (catches, Handler (Handler), throwIO)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef.Extra
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Typeable
import qualified Haxl.Core as Haxl

import Util.Control.Exception
import qualified Util.Control.Exception.CallStack as CallStack
import Util.STM

import Glean.Backend.Types
import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.CompletePredicates as Database
import qualified Glean.Database.Create as Database
import Glean.Database.Config
import qualified Glean.Database.Delete as Database
import Glean.Database.Env
import Glean.Database.Open as Database
import qualified Glean.Database.List as Database
import qualified Glean.Database.PredicateStats as Database (predicateStats)
import qualified Glean.Database.Restore as Database
import qualified Glean.Database.Schema as Database
import Glean.Database.Schema hiding (getSchemaInfo, schemaId)
import qualified Glean.Database.Types as Database
import Glean.Database.Types (Env(..))
import qualified Glean.Database.Finish as Database
import qualified Glean.Database.Writes as Database
import Glean.Internal.Types
import qualified Glean.Query.UserQuery as UserQuery
import qualified Glean.Query.Derive as Derive
import Glean.RTS (Fid(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Haxl.DataSource.Glean.Common


instance Backend Database.Env where
  queryFact env repo id = readDatabase env repo $ \_ db ->
    Lookup.lookupFact db (Fid id)

  factIdRange env repo = do
    (starting, next) <- readDatabase env repo $ \_ db ->
      (,) <$> Lookup.startingId db <*> Lookup.firstFreeId db
    return $ Thrift.FactIdRange (fromFid starting) (fromFid next)

  getSchemaInfo env (Just repo) req =
    withOpenDatabase env repo $ \odb -> do
      index <- Observed.get (Database.envSchemaSource env)
      Database.getSchemaInfo (Database.odbSchema odb) index req

  getSchemaInfo env Nothing Thrift.GetSchemaInfo{..} = do
      index <- Observed.get (Database.envSchemaSource env)
      sid <- case getSchemaInfo_select of
          Thrift.SelectSchema_schema_id sid -> return sid
          other -> throwIO $ userError $ "unsupported: " <> show other
      Database.getSchemaInfoForSchema index sid (envDebug env)

  validateSchema env (Thrift.ValidateSchema str) = do
    schema <- Observed.get (Database.envSchemaSource env)
    conf <- Observed.get (Database.envServerConfig env)
    validateNewSchema conf str schema (envDebug env)

  predicateStats env repo opts = Database.predicateStats env repo opts

  userQueryFacts env repo q
    | isNothing (Thrift.userQueryFacts_schema_id q) =
      UserQuery.userQueryFacts env repo q {
        Thrift.userQueryFacts_schema_id = schemaId env
      }
    | otherwise = UserQuery.userQueryFacts env repo q

  userQuery env repo q
    | isNothing (Thrift.userQuery_schema_id q) =
      UserQuery.userQuery env repo q {
        Thrift.userQuery_schema_id = schemaId env
      }
    | otherwise = UserQuery.userQuery env repo q

  userQueryBatch env repo Thrift.UserQueryBatch{..} = do
      resultsRef <- newIORef mempty
      numCaps <- getNumCapabilities
      stream numCaps (forM_ queries) $ \q -> do
        res <- runOne q
        atomicModifyIORef'_ resultsRef $ Map.insert q res
      results <- readIORef resultsRef
      return $ map (results Map.!) queries
      where
        queries =
          [ Thrift.UserQuery
            { userQuery_predicate = userQueryBatch_predicate
            , userQuery_predicate_version = userQueryBatch_predicate_version
            , userQuery_encodings  = userQueryBatch_encodings
            , userQuery_client_info = userQueryBatch_client_info
            , userQuery_schema_id = userQueryBatch_schema_id <|> schemaId env
            , userQuery_options = userQueryBatch_options
            , userQuery_query = q
            }
          | q <- userQueryBatch_queries
          ]
        runOne query =
          (Thrift.UserQueryResultsOrException_results
            <$> userQuery env repo query)
          `catches`
          [ Handler $ \(Thrift.Retry r) -> return $
              Thrift.UserQueryResultsOrException_retry $
                Thrift.RetryException r
          , Handler $ \(Thrift.BadQuery b) -> return $
              Thrift.UserQueryResultsOrException_badQuery $
                Thrift.BadQueryException b
          ]


  deriveStored = Derive.deriveStored

  listDatabases = Database.listDatabases
  getDatabase env repo =
    maybe (CallStack.throwIO $ Thrift.UnknownDatabase repo) return
    =<< atomically (Catalog.getLocalDatabase (Database.envCatalog env) repo)

  kickOffDatabase = Database.kickOffDatabase
  finishDatabase = Database.finishDatabase
  finalizeDatabase = Database.finalizeDatabase
  updateProperties env repo set unset = do
    Database.updateProperties env repo set unset
    return def

  completePredicates_ = Database.completePredicates

  restoreDatabase = Database.restoreDatabase

  deleteDatabase env repo = do
    Database.deleteDatabase env repo
    return def

  enqueueBatch env cbatch = Database.enqueueBatch env cbatch Nothing
  enqueueJsonBatch env cbatch = Database.enqueueJsonBatch env cbatch
  enqueueBatchDescriptor env repo batch waitPolicy =
    Database.enqueueBatchDescriptor env repo batch waitPolicy
  pollBatch env handle = Database.pollBatch env handle

  displayBackend _ = "(local backend)"

  hasDatabase env repo = do
    Thrift.GetDatabaseResult { getDatabaseResult_database = Thrift.Database{..}}
      <- getDatabase env repo
    return $ case database_status of
      Thrift.DatabaseStatus_Restorable -> False
      _ -> True

  schemaId = Database.envSchemaId

  usingShards _ = False

  initGlobalState backend = do
    capabilities <- getNumCapabilities
    let streamMapM_ :: (a -> IO ()) -> [a] -> IO ()
        streamMapM_ f xx = stream capabilities (forM_ xx) f
    if capabilities == 1
      then return
        ( GleanGetState $ Haxl.SyncFetch $
            mapM_ (syncGetOne backend) . HashMap.toList . requestByRepo
        , GleanQueryState $ Haxl.SyncFetch $
            mapM_ (syncQueryOne backend)
        )
      else return
        ( GleanGetState $ Haxl.BackgroundFetch $
            streamMapM_ (syncGetOne backend) . HashMap.toList . requestByRepo
        , GleanQueryState $ Haxl.BackgroundFetch $
            streamMapM_ (syncQueryOne backend)
        )


-- -----------------------------------------------------------------------------
-- Haxl

syncGetOne :: Env -> (Thrift.Repo, [Haxl.BlockedFetch GleanGet]) -> IO ()
syncGetOne env (repo, requests) = do
  let schema = schemaId env
  results <- userQueryFacts env repo (mkUserQueryFacts Nothing schema requests)
  putResults results requests

syncQueryOne :: Env -> Haxl.BlockedFetch GleanQuery -> IO ()
syncQueryOne env
    (Haxl.BlockedFetch (QueryReq (Query q :: Query q) repo stream) rvar) =
  runSyncQuery repo env
    (Query (mkUserQuery Nothing (schemaId env) q) :: Query q)
    (if stream then Just mempty else Nothing) rvar

runSyncQuery
  :: forall q r. (Show q, Typeable q, QueryResult q r)
  => Thrift.Repo
  -> Env
  -> Query q
  -> Maybe r -- results so far
  -> Haxl.ResultVar (r, Bool)
  -> IO ()
runSyncQuery repo env q@(Query req) acc rvar = do
  r <- tryAll $ userQuery env repo req
  case r of
    Left e -> Haxl.putFailure rvar e
    Right results ->
      putQueryResults q results acc rvar $
        \(q :: Query q) acc -> runSyncQuery repo env q acc rvar

-- -----------------------------------------------------------------------------
-- DbSchema

loadDbSchema :: Backend a => a -> Thrift.Repo -> IO DbSchema
loadDbSchema backend repo = do
  Thrift.SchemaInfo schema pids _ dbSchemaIds _ _ <-
    getSchemaInfo backend (Just repo) def
      { Thrift.getSchemaInfo_select = Thrift.SelectSchema_stored def }
  fromStoredSchema Nothing (StoredSchema schema pids dbSchemaIds)
    readWriteContent def

-- | Serialize the inventory for the schema used by this repo.
-- The inventory is some information about the predicates in the schema
serializeInventory
  :: Backend backend
  => backend
  -> Thrift.Repo
  -> IO ByteString
serializeInventory backend repo = do
  dbSchema <- loadDbSchema backend repo
  return $ Inventory.serialize $ schemaInventory dbSchema

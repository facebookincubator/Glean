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
module Glean.Backend
  (
    module Glean.Backend.Remote

    -- * Local or remote backends
  , Service(..)
  , withBackendWithDefaultOptions
  , withBackend
  , BackendKind(..), LocalOrRemote(..)
  , Logging(..)

    -- * Parsing command line options
  , options, optionsLong

    -- * Logging
  , LoggingBackend(..)

    -- * Schemas
  , loadDbSchema
  , serializeInventory
) where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Options.Applicative as O

import Logger.GleanServer (GleanServerLogger)
import qualified Logger.GleanServer as Logger
import qualified Util.Control.Exception.CallStack as CallStack
import Util.EventBase (EventBaseDataplane)
import Util.Logger

import Glean.ClientConfig.Types (ClientConfig(..))
import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.CompletePredicates as Database
import qualified Glean.Database.Config as Database
import qualified Glean.Database.Env as Database
import qualified Glean.Database.Create as Database
import qualified Glean.Database.Delete as Database
import Glean.Database.Open as Database
import qualified Glean.Database.List as Database
import qualified Glean.Database.PredicateStats as Database (predicateStats)
import qualified Glean.Database.Restore as Database
import qualified Glean.Database.Schema as Database
import Glean.Database.Schema hiding (getSchemaInfo)
import qualified Glean.Database.Types as Database
import qualified Glean.Database.Work as Database
import qualified Glean.Database.Writes as Database
import Glean.Impl.ConfigProvider
import Glean.Internal.Types (StoredSchema(..))
import Glean.Logger
import qualified Glean.Query.UserQuery as UserQuery
import qualified Glean.Query.Derive as Derive
import Glean.RTS (Fid(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.Some
import Glean.Util.Time

import Glean.Backend.Remote hiding (options, optionsLong)
import qualified Glean.Backend.Remote as Remote


data Logging = EnableLogging | DisableLogging
  deriving (Eq, Show)

-- | Specifies what kind of 'Backend' to construct.
data Service
  = Local Database.Config Logging
  | Remote (ThriftSource ClientConfig)
  deriving Show

-- | Use the provided 'Service' to make a 'Backend'.  (note in fact
-- that it provides a 'LocalOrRemote', which is a 'Backend' that
-- additionally supports 'backendKind').
withBackendWithDefaultOptions
  :: EventBaseDataplane
  -> ConfigAPI
  -> Service
  -> Maybe Thrift.SchemaId
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackendWithDefaultOptions evb cfgapi service schema =
  withBackend evb cfgapi service schema id

-- | Use the provided 'Service' to make a 'Backend', applying some
-- 'Settings' if this is a remote backend. (note in fact that it
-- provides a 'LocalOrRemote', which is a 'Backend' that additionally
-- supports 'backendKind').
withBackend
  :: EventBaseDataplane
  -> ConfigAPI
  -> Service
  -> Maybe Thrift.SchemaId
  -> Settings
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackend evb cfgapi service schema settings inner = case service of
  Local cfg logging ->
    let cfg' = cfg { Database.cfgSchemaId = schema } in
    Database.withDatabases evb cfg' cfgapi $
      case logging of
        EnableLogging -> inner . LoggingBackend
        DisableLogging -> inner
  Remote src -> do
    config <- ThriftSource.loadDefault cfgapi src
    let (config', opts) = settings (config, def)
    client <- clientInfo
    inner $ ThriftBackend
      config
      evb
      (thriftServiceWithTimeout config' opts)
      client
      schema

-- | Command-line options to specify a 'Service' that we can connect to.
-- The 'Service' is either a remote Glean server (e.g. @--service=<host>:port@)
-- or a local database store (e.g. @--db-root=<dir>@).
options :: O.Parser Service
options = optionsLong "service"

optionsLong :: String -> O.Parser Service
optionsLong self =
  Remote <$> Remote.optionsLong self <|>
  Local <$> Database.options <*> logging
  where
    logging = (\b -> if b then EnableLogging else DisableLogging) <$> O.switch
      (  O.long "enable-logging"
      <> O.help "Log requests to Scuba/Hive/..."
      )

data InvalidSchema = InvalidSchema deriving(Show)
instance Exception InvalidSchema

-- | A logging wrapper for Env. We do it this way because some backend
-- calls invoke other backend calls, and we only want to log the
-- outermost one. For example, userQuery will call queryFact a *lot*,
-- and it would be too expensive to log each and every call to
-- queryFact.
newtype LoggingBackend = LoggingBackend Database.Env

instance Backend LoggingBackend where
  queryFact (LoggingBackend env) repo id =
    loggingAction (runLogRepo "queryFact" env repo) (const mempty) $
      queryFact env repo id
  firstFreeId (LoggingBackend env) repo =
    loggingAction (runLogRepo "firstFreeId" env repo) (const mempty) $
      firstFreeId env repo
  factIdRange (LoggingBackend env) repo =
    loggingAction (runLogRepo "factIdRange" env repo) (const mempty) $
      factIdRange env repo
  getSchemaInfo (LoggingBackend env) repo req =
    loggingAction (runLogRepo "getSchemaInfo" env repo) (const mempty) $
      getSchemaInfo env repo req
  validateSchema (LoggingBackend env) req =
    loggingAction (runLogCmd "validateSchema" env) (const mempty) $
      validateSchema env req
  predicateStats (LoggingBackend env) repo opts =
    loggingAction (runLogRepo "predicateStats" env repo) (const mempty) $
      predicateStats env repo opts
  userQueryFacts (LoggingBackend env) repo req =
    loggingAction (runLogQueryFacts "userQueryFacts" env repo req)
      logQueryResults $
        userQueryFacts env repo req
  userQuery (LoggingBackend env) repo req =
    loggingAction (runLogQuery "userQuery" env repo req) logQueryResults $
      userQuery env repo req

  deriveStored (LoggingBackend env) log repo q =
    loggingAction
      (runLogDerivePredicate "deriveStored" env repo q)
      (const mempty)
      (deriveStored env (runLogDerivationResult env log repo q) repo q)

  listDatabases (LoggingBackend env) req =
    loggingAction (runLogCmd "listDatabases" env) (const mempty) $
      Database.listDatabases env req
  getDatabase (LoggingBackend env) repo =
    loggingAction (runLogRepo "getDatabase" env repo) (const mempty) $
      getDatabase env repo

  kickOffDatabase (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "kickOffDatabase" env $ Thrift.kickOff_repo rq)
      (const mempty) $
      kickOffDatabase env rq
  finalizeDatabase (LoggingBackend env) repo =
    loggingAction
      (runLogRepo "finalizeDatabase" env repo)
      (const mempty) $
      finalizeDatabase env repo
  updateProperties (LoggingBackend env) repo set unset =
    loggingAction
      (runLogRepo "updateProperties" env repo)
      (const mempty) $
      updateProperties env repo set unset
  getWork (LoggingBackend env) rq =
    loggingAction (runLogCmd "getWork" env) (const mempty) $
      getWork env rq
  workCancelled (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workCancelled" env
        $ Thrift.work_repo
        $ Thrift.workCancelled_work rq)
      (const mempty) $
      workCancelled env rq
  workHeartbeat (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workHeartbeat" env
        $ Thrift.work_repo
        $ Thrift.workHeartbeat_work rq)
      (const mempty) $
      workHeartbeat env rq
  workFinished (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workFinished" env
        $ Thrift.work_repo
        $ Thrift.workFinished_work rq)
      (const mempty) $
      workFinished env rq

  completePredicates (LoggingBackend env) repo =
    loggingAction
       (runLogRepo "completePredicates" env repo)
       (const mempty) $
       completePredicates env repo

  restoreDatabase (LoggingBackend env) loc =
    loggingAction (runLogCmd "restoreDatabase" env) (const mempty) $
      restoreDatabase env loc
  deleteDatabase (LoggingBackend env) repo =
    loggingAction (runLogRepo "deleteDatabase" env repo) (const mempty) $
      deleteDatabase env repo
  enqueueBatch (LoggingBackend env) cbatch =
    loggingAction
      (runLogRepo "enqueueBatch" env (Thrift.computedBatch_repo cbatch))
      (const mempty) $
        enqueueBatch env cbatch
  enqueueJsonBatch (LoggingBackend env) repo batch =
    loggingAction (runLogRepo "enqueueJsonBatch" env repo) (const mempty) $
      enqueueJsonBatch env repo batch
  pollBatch (LoggingBackend env) handle =
    loggingAction (runLogCmd "pollBatch" env) (const mempty) $
      pollBatch env handle
  displayBackend (LoggingBackend b) = displayBackend b
  hasDatabase (LoggingBackend b) repo = hasDatabase b repo
  maybeRemote (LoggingBackend b) = maybeRemote b
  schemaId (LoggingBackend b) = schemaId b


instance Backend Database.Env where
  queryFact env repo id = readDatabase env repo $ \_ db ->
    Lookup.lookupFact db (Fid id)

  firstFreeId env repo =
    fromFid <$> readDatabase env repo (const Lookup.firstFreeId)

  factIdRange env repo = do
    (starting, next) <- readDatabase env repo $ \_ db ->
      (,) <$> Lookup.startingId db <*> Lookup.firstFreeId db
    return $ Thrift.FactIdRange (fromFid starting) (fromFid next)

  getSchemaInfo env repo req =
    withOpenDatabase env repo $ \odb -> do
      index <- Observed.get (Database.envSchemaSource env)
      Database.getSchemaInfo (Database.odbSchema odb) index req

  validateSchema env (Thrift.ValidateSchema str) = do
    schema  <- get (Database.envSchemaSource env)
    validateNewSchema str schema

  predicateStats env repo opts = Database.predicateStats env repo opts

  userQueryFacts = UserQuery.userQueryFacts
  userQuery = UserQuery.userQuery

  deriveStored = Derive.deriveStored

  listDatabases = Database.listDatabases
  getDatabase env repo =
    maybe (CallStack.throwIO $ Thrift.UnknownDatabase repo) return
    =<< atomically (Catalog.getLocalDatabase (Database.envCatalog env) repo)

  kickOffDatabase = Database.kickOffDatabase
  finalizeDatabase = Database.finalizeDatabase
  updateProperties env repo set unset = do
    Database.updateProperties env repo set unset
    return def
  getWork = Database.getWork
  workCancelled = Database.workCancelled
  workHeartbeat = Database.workHeartbeat
  workFinished = Database.workFinished

  completePredicates = Database.completePredicates

  restoreDatabase = Database.restoreDatabase

  deleteDatabase env repo = do
    Database.deleteDatabase env repo
    return def

  enqueueBatch env cbatch = Database.enqueueBatch env cbatch Nothing
  enqueueJsonBatch env cbatch = Database.enqueueJsonBatch env cbatch
  pollBatch env handle = Database.pollBatch env handle

  displayBackend _ = "(local backend)"

  hasDatabase env repo = do
    Thrift.GetDatabaseResult Thrift.Database{..} _ <- getDatabase env repo
    return $ case database_status of
      Thrift.DatabaseStatus_Restorable -> False
      _ -> True

  maybeRemote _ = Nothing

  schemaId = Database.envSchemaId

-- -----------------------------------------------------------------------------
-- DbSchema

loadDbSchema :: Backend a => a -> Thrift.Repo -> IO DbSchema
loadDbSchema backend repo = do
  Thrift.SchemaInfo schema pids versions <- getSchemaInfo backend repo def
    { Thrift.getSchemaInfo_select = Thrift.SelectSchema_stored def }
  fromStoredSchema (StoredSchema schema pids versions) readWriteContent

serializeInventory
  :: Backend backend
  => backend
  -> Thrift.Repo
  -> IO ByteString
serializeInventory backend repo = do
  dbSchema <- loadDbSchema backend repo
  return $ Inventory.serialize $ schemaInventory dbSchema

-- -----------------------------------------------------------------------------
-- Logging

runLogQueryFacts
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQueryFacts
  -> GleanServerLogger -> IO ()
runLogQueryFacts cmd env repo Thrift.UserQueryFacts{..} log =
  runLogRepo cmd env repo $ log
    <> maybe mempty logQueryOptions userQueryFacts_options
    <> maybe mempty logQueryClientInfo userQueryFacts_client_info
    <> maybe mempty (Logger.setSchemaId . Thrift.unSchemaId)
        userQueryFacts_schema_id

runLogQuery
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> GleanServerLogger
  -> IO ()
runLogQuery cmd env repo Thrift.UserQuery{..} log = do
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.setQuery (Text.decodeUtf8 userQuery_query)
    , Logger.setPredicate userQuery_predicate
    , maybe mempty (Logger.setPredicateVersion . fromIntegral)
        userQuery_predicate_version
    , maybe mempty (Logger.setSchemaVersion . fromIntegral)
        userQuery_schema_version
    , maybe mempty (Logger.setSchemaId . Thrift.unSchemaId)
        userQuery_schema_id
    , maybe mempty logQueryOptions userQuery_options
    , maybe mempty logQueryClientInfo userQuery_client_info
    ]

logQueryOptions :: Thrift.UserQueryOptions -> GleanServerLogger
logQueryOptions Thrift.UserQueryOptions{..} = mconcat
  [ Logger.setNoBase64Binary userQueryOptions_no_base64_binary
  , Logger.setExpandResults userQueryOptions_expand_results
  , Logger.setRecursive userQueryOptions_recursive
  , maybe mempty (Logger.setMaxResults . fromIntegral)
      userQueryOptions_max_results
  , Logger.setSyntax $ case userQueryOptions_syntax of
      Thrift.QuerySyntax_JSON -> "JSON"
      Thrift.QuerySyntax_ANGLE -> "Angle"
  , maybe mempty
      ( Logger.setRequestContinuationSize
      . ByteString.length
      . Thrift.userQueryCont_continuation
      )
      userQueryOptions_continuation
  ]

logQueryClientInfo :: Thrift.UserQueryClientInfo -> GleanServerLogger
logQueryClientInfo Thrift.UserQueryClientInfo{..} = mconcat
  [ maybe mempty Logger.setClientUnixname userQueryClientInfo_unixname
  , Logger.setClientApplication userQueryClientInfo_application
  , Logger.setClientName userQueryClientInfo_name
  ]

logQueryResults :: Thrift.UserQueryResults -> GleanServerLogger
logQueryResults Thrift.UserQueryResults{..} = mconcat
  [ Logger.setResults $ case userQueryResults_results of
      Thrift.UserQueryEncodedResults_bin bin ->
        Map.size (Thrift.userQueryResultsBin_facts bin)
      Thrift.UserQueryEncodedResults_json json ->
        length (Thrift.userQueryResultsJSON_facts json)
      Thrift.UserQueryEncodedResults_compact compact ->
        length (Thrift.userQueryResultsCompact_facts compact)
      _ ->
        length userQueryResults_facts
  , Logger.setTruncated (isJust userQueryResults_continuation)
  , maybe mempty logQueryStats userQueryResults_stats
  , maybe mempty Logger.setType userQueryResults_type
  , maybe mempty
      ( Logger.setResponseContinuationSize
      . ByteString.length
      . Thrift.userQueryCont_continuation
      )
      userQueryResults_continuation
  ]

logQueryStats :: Thrift.UserQueryStats -> GleanServerLogger
logQueryStats Thrift.UserQueryStats{..} = mconcat
  [ Logger.setResults (fromIntegral userQueryStats_result_count)
  , Logger.setFacts (fromIntegral userQueryStats_num_facts)
  , maybe mempty (Logger.setBytecodeSize . fromIntegral)
      userQueryStats_bytecode_size
  , maybe mempty (Logger.setCompileTimeUs . fromIntegral . (`quot` 1000))
      userQueryStats_compile_time_ns
  , maybe mempty (Logger.setExecuteTimeUs . fromIntegral . (`quot` 1000))
      userQueryStats_execute_time_ns
  ]

runLogDerivePredicate
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> GleanServerLogger
  -> IO ()
runLogDerivePredicate cmd env repo Thrift.DerivePredicateQuery {..} log =
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.setPredicate derivePredicateQuery_predicate
    , maybe mempty (Logger.setPredicateVersion . fromIntegral)
        derivePredicateQuery_predicate_version
    , maybe mempty logQueryClientInfo derivePredicateQuery_client_info
    ]

runLogDerivationResult
  :: Database.Env
  -> LogDerivationResult
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> Either (DiffTimePoints, SomeException) Thrift.UserQueryStats
  -> IO ()
runLogDerivationResult env log repo Thrift.DerivePredicateQuery{..} res = do
  log res
  runLogRepo "deriveStored(completed)" env repo $ mconcat
    [ Logger.setPredicate derivePredicateQuery_predicate
    , maybe mempty (Logger.setPredicateVersion . fromIntegral)
        derivePredicateQuery_predicate_version
    , maybe mempty logQueryClientInfo derivePredicateQuery_client_info
    , case res of
        Left (_,err) -> failureLog err
        Right stats -> successLog <> logQueryStats stats
    , timeLog $ toDiffSeconds $ case res of
        Left (duration, _) -> duration
        Right Thrift.UserQueryStats{..} ->
          nanoseconds (fromIntegral userQueryStats_elapsed_ns)
    ]

-- -----------------------------------------------------------------------------
-- Backends that might be local or remote


data BackendKind
  = BackendEnv Database.Env
  | BackendThrift ThriftBackend

displayBackendKind :: BackendKind -> String
displayBackendKind BackendEnv{} = "BackendKind BackendEnv {_ :: Database.Env}"
displayBackendKind (BackendThrift tb) = unwords
  [ "BackendKind BackendThrift {", show tb, "}" ]

instance Show BackendKind where
  show = displayBackendKind

-- | Sometimes we need to do something backend-specific, so we need to
-- get back from the abstract 'Backend' to the concrete underlying
-- representation.  A 'LocalOrRemote' is a 'Backend' that additionally
-- supports 'backendKind' to find its 'BackendKind'.
--
-- We don't want this to be part of the 'Backend' class, because there
-- are clients that only want to use a remote 'Backend' but nevertheless
-- want to use the 'Backend' abstraction, because many of the other
-- APIs depend on it.  If 'backendKind' were part of 'Backend', then
-- remote-only clients would depend on support for local DBs too.
class Backend a => LocalOrRemote a where
  backendKind :: a -> BackendKind

instance LocalOrRemote LoggingBackend where
  backendKind (LoggingBackend env) = BackendEnv env

instance LocalOrRemote Database.Env where
  backendKind env = BackendEnv env

instance LocalOrRemote ThriftBackend where
  backendKind t = BackendThrift t

instance Backend (Some LocalOrRemote) where
  queryFact (Some backend) = queryFact backend
  firstFreeId (Some backend) = firstFreeId backend
  factIdRange (Some backend) = factIdRange backend
  getSchemaInfo (Some backend) = getSchemaInfo backend
  validateSchema (Some backend) = validateSchema backend
  predicateStats (Some backend) = predicateStats backend
  listDatabases (Some backend) = listDatabases backend
  getDatabase (Some backend) = getDatabase backend
  userQueryFacts (Some backend) = userQueryFacts backend
  userQuery (Some backend) = userQuery backend
  deriveStored (Some backend) = deriveStored backend

  kickOffDatabase (Some backend) = kickOffDatabase backend
  finalizeDatabase (Some backend) = finalizeDatabase backend
  updateProperties (Some backend) = updateProperties backend
  getWork (Some backend) = getWork backend
  workCancelled (Some backend) = workCancelled backend
  workHeartbeat (Some backend) = workHeartbeat backend
  workFinished (Some backend) = workFinished backend
  completePredicates (Some backend) = completePredicates backend

  restoreDatabase (Some backend) = restoreDatabase backend
  deleteDatabase (Some backend) = deleteDatabase backend

  enqueueBatch (Some backend) = enqueueBatch backend
  enqueueJsonBatch (Some backend) = enqueueJsonBatch backend
  pollBatch (Some backend) = pollBatch backend
  displayBackend (Some backend) = displayBackend backend
  hasDatabase (Some backend) = hasDatabase backend
  maybeRemote (Some backend) = maybeRemote backend
  schemaId (Some backend) = schemaId backend

instance LocalOrRemote (Some LocalOrRemote) where
  backendKind (Some b) = backendKind b

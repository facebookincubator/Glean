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
) where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Control.Exception
import qualified Data.ByteString as ByteString
import Data.Coerce (coerce)
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Options.Applicative as O

import Logger.GleanServer (GleanServerLogger)
import qualified Logger.GleanServer as Logger
import Data.RateLimiterMap
import Util.EventBase (EventBaseDataplane)
import Util.Logger

import Glean.ClientConfig.Types (ClientConfig(..))
import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.Config as Database
import qualified Glean.Database.Env as Database
import qualified Glean.Database.Index as Database
import qualified Glean.Database.Storage as Storage
import Glean.Database.Stuff
import qualified Glean.Database.Types as Database
import qualified Glean.Database.Work as Database
import qualified Glean.Database.Writes as Database
import Glean.Impl.ConfigProvider
import qualified Glean.Query.UserQuery as UserQuery
import qualified Glean.Query.Derive as Derive
import Glean.RTS (Fid(..), Pid(..))
import qualified Glean.RTS.Foreign.Lookup as Lookup
import Glean.Database.Schema
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.ThriftSource as ThriftSource

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
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackendWithDefaultOptions evb cfgapi service =
  withBackend evb cfgapi service id

-- | Use the provided 'Service' to make a 'Backend', applying some
-- 'Settings' if this is a remote backend. (note in fact that it
-- provides a 'LocalOrRemote', which is a 'Backend' that additionally
-- supports 'backendKind').
withBackend
  :: EventBaseDataplane
  -> ConfigAPI
  -> Service
  -> Settings
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackend evb cfgapi service settings inner = case service of
  Local cfg logging ->
    Database.withDatabases evb cfg cfgapi $
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
  getSchemaInfo (LoggingBackend env) repo =
    loggingAction (runLogRepo "getSchemaInfo" env repo) (const mempty) $
      getSchemaInfo env repo
  validateSchema (LoggingBackend env) req =
    loggingAction (runLogCmd "validateSchema" env) (const mempty) $
      validateSchema env req
  predicateStats (LoggingBackend env) repo =
    loggingAction (runLogRepo "predicateStats" env repo) (const mempty) $
      predicateStats env repo
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

  derivePredicate (LoggingBackend env) repo q =
    loggingAction
      (runLogDerivePredicate "derivePredicate" env repo q)
      (const mempty)
      (derivePredicate env repo q)

  pollDerivation (LoggingBackend env) handle =
    loggingAction (runLogCmd "pollDerivation" env) logDerivationProgress $
      pollDerivation env handle

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


instance Backend Database.Env where
  queryFact env repo id = readDatabase env repo $ \_ db ->
    Lookup.lookupFact db (Fid id)

  firstFreeId env repo =
    fromFid <$> readDatabase env repo (const Lookup.firstFreeId)

  factIdRange env repo = do
    (starting, next) <- readDatabase env repo $ \_ db ->
      (,) <$> Lookup.startingId db <*> Lookup.firstFreeId db
    return $ Thrift.FactIdRange (fromFid starting) (fromFid next)

  getSchemaInfo env repo = withOpenDatabase env repo $
    return . toSchemaInfo . Database.odbSchema

  validateSchema env (Thrift.ValidateSchema str) = do
    (curSrc, curSchemas)  <- get (Database.envSchemaSource env)
    validateNewSchema str curSrc curSchemas

  predicateStats env repo = withOpenDatabase env repo $ \Database.OpenDB{..} ->
    Map.fromList . coerce <$> Storage.predicateStats odbHandle

  userQueryFacts = UserQuery.userQueryFacts
  userQuery = UserQuery.userQuery

  deriveStored = Derive.deriveStored
  derivePredicate = Derive.derivePredicate
  pollDerivation = Derive.pollDerivation

  listDatabases = Database.listDatabases
  getDatabase env repo = maybe (throwIO $ Thrift.UnknownDatabase repo) return
    =<< atomically (Catalog.getLocalDatabase (Database.envCatalog env) repo)

  kickOffDatabase = Database.kickOffDatabase
  updateProperties env repo set unset = do
    Database.updateProperties env repo set unset
    return def
  getWork = Database.getWork
  workCancelled = Database.workCancelled
  workHeartbeat = Database.workHeartbeat
  workFinished = Database.workFinished

  restoreDatabase = Database.restoreDatabase

  deleteDatabase env repo = do
    Database.deleteDatabase env repo
    return def

  enqueueBatch env cbatch = Database.enqueueBatch env cbatch
  enqueueJsonBatch env cbatch = Database.enqueueJsonBatch env cbatch
  pollBatch env handle = Database.pollBatch env handle

  displayBackend _ = "(local backend)"

  hasDatabase env repo = do
    Thrift.GetDatabaseResult Thrift.Database{..} _ <- getDatabase env repo
    return $ case database_status of
      Just Thrift.DatabaseStatus_Restorable -> False
      _ -> True

  maybeRemote _ = Nothing

-- -----------------------------------------------------------------------------
-- DbSchema

loadDbSchema :: Backend a => a -> Thrift.Repo -> IO DbSchema
loadDbSchema backend repo = do
  info <- getSchemaInfo backend repo
  fromSchemaInfo info readWriteContent

-- -----------------------------------------------------------------------------
-- Logging

instance ActionLog GleanServerLogger where
  successLog = Logger.setSuccess True
  failureLog ex = mconcat
    [ Logger.setSuccess False
    , Logger.setError (Text.pack (show ex))
    ]
  timeLog = Logger.setTimeElapsed
  allocLog = Logger.setAllocatedBytes . fromIntegral

runLogCmd :: Text -> Database.Env -> GleanServerLogger -> IO ()
runLogCmd cmd env log =
  whenAllowed (Database.envLoggerRateLimit env) cmd $ \weight ->
    Logger.runLog (Database.envLogger env) $
      log <> Logger.setMethod cmd <> Logger.setWeight weight

runLogRepo :: Text -> Database.Env -> Thrift.Repo -> GleanServerLogger -> IO ()
runLogRepo cmd env Thrift.Repo{..} log =
  runLogCmd cmd env $
    log <> Logger.setRepoName repo_name <> Logger.setRepoHash repo_hash

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

runLogQuery
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> GleanServerLogger
  -> IO ()
runLogQuery cmd env@Database.Env{..} repo Thrift.UserQuery{..} log = do
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.setQuery (Text.decodeUtf8 userQuery_query)
    , Logger.setPredicate userQuery_predicate
    , maybe mempty (Logger.setPredicateVersion . fromIntegral)
        userQuery_predicate_version
    , maybe mempty (Logger.setSchemaVersion . fromIntegral)
        userQuery_schema_version
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
  [ Logger.setFacts (fromIntegral userQueryStats_num_facts)
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
  -> (Either SomeException Thrift.UserQueryStats -> IO ())
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> Either SomeException Thrift.UserQueryStats
  -> IO ()
runLogDerivationResult env log repo Thrift.DerivePredicateQuery{..} res = do
  log res
  runLogRepo "deriveStored(completed)" env repo $ mconcat
    [ Logger.setPredicate derivePredicateQuery_predicate
    , maybe mempty (Logger.setPredicateVersion . fromIntegral)
        derivePredicateQuery_predicate_version
    , maybe mempty logQueryClientInfo derivePredicateQuery_client_info
    , case res of
        Left err -> failureLog err
        Right stats -> successLog <> logQueryStats stats
    ]

logDerivationProgress :: Thrift.DerivationProgress -> GleanServerLogger
logDerivationProgress = \case
      Thrift.DerivationProgress_ongoing _ -> mempty
      Thrift.DerivationProgress_complete stats -> logQueryStats stats

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

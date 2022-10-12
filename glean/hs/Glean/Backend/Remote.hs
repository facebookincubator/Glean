{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Backend.Remote
  ( Backend(..)
  , StackedDbOpts(..)

    -- * Command-line options
  , options
  , optionsLong

    -- * Construction
  , withRemoteBackend
  , withRemoteBackendSettings
  , Settings
  , setService
  , setNoShards
  , setTimeout
  , thriftServiceWithTimeout
  , ThriftBackend(..)
  , defaultClientConfigSource

    -- * Shards
  , DbShard
  , dbShard
  , dbShardWord

    -- * More operations
  , SchemaPredicates
  , loadPredicates
  , databases
  , localDatabases
  , usingShards
  , clientInfo

  , LogDerivationResult
  ) where

import Control.Applicative
import Control.Exception
import Data.Bits
import qualified Data.ByteString.Unsafe as B
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign
import GHC.Fingerprint
import Options.Applicative as Options
import System.IO.Unsafe

import Thrift.Channel
import Thrift.Api
import Util.EventBase (EventBaseDataplane)
import Util.Log

import Glean.BuildInfo (buildRule)
import Glean.ClientConfig.Types (UseShards(..), ClientConfig(..))
import Glean.DefaultConfigs
import Glean.GleanService.Client (GleanService)
import qualified Glean.GleanService.Client as GleanService
import Glean.Typed.Predicate (makePredicates, Predicates, SchemaPredicates)
import qualified Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.Service
import Glean.Util.Some
import Glean.Username (getUsername)
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.ThriftService
import Glean.Util.Time (DiffTimePoints)
import Glean.Impl.ThriftService

data StackedDbOpts
  = IncludeBase
  | ExcludeBase
  deriving (Eq, Show)

-- |
-- An abstraction over Glean's Thrift API. This allows client code
-- to work with either a local or remote backend, chosen at runtime.
--
class Backend a where
  queryFact :: a -> Thrift.Repo -> Thrift.Id -> IO (Maybe Thrift.Fact)
  firstFreeId :: a -> Thrift.Repo -> IO Thrift.Id
  factIdRange :: a -> Thrift.Repo -> IO Thrift.FactIdRange
  getSchemaInfo :: a -> Thrift.Repo -> Thrift.GetSchemaInfo
    -> IO Thrift.SchemaInfo
  validateSchema :: a -> Thrift.ValidateSchema -> IO ()
  predicateStats :: a -> Thrift.Repo -> StackedDbOpts
    -> IO (Map Thrift.Id Thrift.PredicateStats)
  listDatabases :: a -> Thrift.ListDatabases -> IO Thrift.ListDatabasesResult
  getDatabase :: a -> Thrift.Repo -> IO Thrift.GetDatabaseResult

  userQueryFacts :: a -> Thrift.Repo -> Thrift.UserQueryFacts
    -> IO Thrift.UserQueryResults
  userQuery :: a -> Thrift.Repo -> Thrift.UserQuery
    -> IO Thrift.UserQueryResults

  deriveStored :: a -> LogDerivationResult -> Thrift.Repo
    -> Thrift.DerivePredicateQuery -> IO Thrift.DerivationStatus

  kickOffDatabase :: a -> Thrift.KickOff -> IO Thrift.KickOffResponse
  finalizeDatabase :: a -> Thrift.Repo -> IO Thrift.FinalizeResponse

  updateProperties
    :: a
    -> Thrift.Repo
    -> Thrift.DatabaseProperties
    -> [Text]
    -> IO Thrift.UpdatePropertiesResponse

  getWork :: a -> Thrift.GetWork -> IO Thrift.GetWorkResponse
  workCancelled :: a -> Thrift.WorkCancelled -> IO ()
  workHeartbeat :: a -> Thrift.WorkHeartbeat -> IO ()
  workFinished :: a -> Thrift.WorkFinished -> IO ()

  completePredicates
    :: a
    -> Thrift.Repo
    -> IO Thrift.CompletePredicatesResponse

  -- | Request a backed up database (specified via its backup locator) to be
  -- made available. This call doesn't wait until the database actually becomes
  -- available, it only issues the request.
  --
  -- This might (for local databases) or might not (for databases on a Thrift
  -- server) return an STM action that waits for the restore operation.
  restoreDatabase :: a -> Text -> IO ()

  -- For a local database this will delete the specified repo
  deleteDatabase :: a -> Thrift.Repo -> IO Thrift.DeleteDatabaseResult

  -- Enqueue a batch for writing
  enqueueBatch :: a -> Thrift.ComputedBatch -> IO Thrift.SendResponse

  -- Enqueue a JSON batch for writing
  enqueueJsonBatch
    :: a
    -> Thrift.Repo
    -> Thrift.SendJsonBatch
    -> IO Thrift.SendJsonBatchResponse

  -- Poll the status of a write batch
  pollBatch :: a -> Thrift.Handle -> IO Thrift.FinishResponse

  -- | Render for debugging
  displayBackend :: a -> String

  -- | For a given 'Repo', check whether any servers have the DB.  If
  -- the backend is remote and using shards, this should check whether
  -- any servers are advertising the appropriate shard.
  hasDatabase :: a -> Thrift.Repo -> IO Bool

  -- | If this is a remote backend, get its ThriftBackend.
  maybeRemote :: a -> Maybe ThriftBackend

  -- | The schema version the client wants to use. This is sent along
  -- with queries.
  schemaId :: a -> Maybe Thrift.SchemaId

-- | The exception includes the length of time from start to error
type LogDerivationResult =
  Either (DiffTimePoints, SomeException) Thrift.UserQueryStats -> IO ()

-- | A remote Glean service, supports the operations of 'Backend'.
data ThriftBackend = ThriftBackend
  { thriftBackendClientConfig :: ClientConfig
  , thriftBackendEventBase :: EventBaseDataplane
  , thriftBackendService :: ThriftService GleanService
  , thriftBackendClientInfo :: Thrift.UserQueryClientInfo
  , thriftBackendSchemaId :: Maybe Thrift.SchemaId
  }

instance Show ThriftBackend where
  show tb = unwords [ "ThriftBackend {(",
    "thriftBackendClientConfig: (" <> show (thriftBackendClientConfig tb),
    "), thriftBackendService: (" <> show (thriftBackendService tb),
    "), thriftBackendClientInfo: (" <> show (thriftBackendClientInfo tb),
    "), thriftBackendSchemaId: (" <> show (thriftBackendSchemaId tb),
    ")}"]


instance Backend (Some Backend) where
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

type Settings
  = (ClientConfig,ThriftServiceOptions)
  -> (ClientConfig,ThriftServiceOptions)

setService :: Service -> Settings
setService service (conf, opts) = (conf { clientConfig_serv = service }, opts)

setNoShards :: Settings
setNoShards (conf, opts) = (conf { clientConfig_use_shards = NO_SHARDS }, opts)

-- | Set the processing timeout, overriding any timeout specified by the
-- service and the host_timeout_ms set in the ClientConfig.
setTimeout :: Double -> Settings
setTimeout t (conf, opts) = (conf, opts { processingTimeout = Just t })

-- | Construct a 'Backend' for interacting with a Glean server.
withRemoteBackend
  :: ConfigProvider cfg
  => EventBaseDataplane
  -> cfg
  -> ThriftSource ClientConfig
  -> Maybe Thrift.SchemaId
  -> (forall b . Backend b => b -> IO a)
  -> IO a
withRemoteBackend evb cfg configSource schema inner =
  withRemoteBackendSettings evb cfg configSource schema id inner

-- | Construct a 'Backend' for interacting with a Glean server, using
-- the given 'Settings'.
withRemoteBackendSettings
  :: ConfigProvider cfg
  => EventBaseDataplane
  -> cfg
  -> ThriftSource ClientConfig
  -> Maybe Thrift.SchemaId
  -> Settings
  -> (forall b . Backend b => b -> IO a)
  -> IO a
withRemoteBackendSettings evb configAPI configSource schema settings inner = do
  config <- ThriftSource.loadDefault configAPI configSource
  client <- clientInfo
  let (config', opts) = settings (config, def)
  inner $ ThriftBackend
    config
    evb
    (thriftServiceWithTimeout config' opts)
    client
    schema

thriftServiceWithTimeout
  :: IsThriftService t
  => ClientConfig
  -> ThriftServiceOptions
  -> t s
thriftServiceWithTimeout ClientConfig{..} opts =
  mkThriftService clientConfig_serv opts'
  where
    -- add host_timeout_ms if a timeout wasn't already specified and
    -- we're talking to a specific host.
    opts' = case clientConfig_serv of
      HostPort{} ->
        opts {
          processingTimeout = processingTimeout opts <|>
            Just (fromIntegral clientConfig_host_timeout_ms / 1000)
          }
      _otherwise -> opts


instance Backend ThriftBackend where
  queryFact t repo id = do
    fact <- withShard t repo $ GleanService.queryFact repo id
    case fact of
      Thrift.Fact 0 _ _ -> return Nothing
      _ -> return (Just fact)
  firstFreeId t repo = withShard t repo $ GleanService.firstFreeId repo
  factIdRange t repo = withShard t repo $ GleanService.factIdRange repo
  getSchemaInfo t repo req = withShard t repo $
    GleanService.getSchemaInfo repo req
  validateSchema t req = withoutShard t $ GleanService.validateSchema req
  predicateStats t repo opts = withShard t repo $
    GleanService.predicateStats repo $
      Thrift.PredicateStatsOpts{Thrift.predicateStatsOpts_excludeBase=opts==ExcludeBase}
  listDatabases t l = withoutShard t $ GleanService.listDatabases l
    { Thrift.listDatabases_client_info = client }
    where
      client = Thrift.listDatabases_client_info l
        <|> Just (thriftBackendClientInfo t)
  getDatabase t repo = withShard t repo $ GleanService.getDatabase repo
  userQueryFacts t repo q = withShard t repo $
    GleanService.userQueryFacts repo q
      { Thrift.userQueryFacts_client_info = client }
    where
      client = Thrift.userQueryFacts_client_info q
        <|> Just (thriftBackendClientInfo t)

  userQuery t repo q = withShard t repo $
    GleanService.userQuery repo q { Thrift.userQuery_client_info = client }
    where
      client = Thrift.userQuery_client_info q
        <|> Just (thriftBackendClientInfo t)

  deriveStored t _ repo pred = withShard t repo $
    GleanService.deriveStored repo pred
      { Thrift.derivePredicateQuery_client_info = client }
    where
      client = Thrift.derivePredicateQuery_client_info pred
        <|> Just (thriftBackendClientInfo t)

  kickOffDatabase t rq = withoutShard t $ GleanService.kickOff rq
  finalizeDatabase t rq = withoutShard t $ GleanService.finalize rq

  updateProperties t repo set unset =
    withoutShard t $
      GleanService.updateProperties repo set unset
  getWork t rq = withoutShard t $ GleanService.getWork rq

  workCancelled t rq =
    withShard t (Thrift.work_repo $ Thrift.workCancelled_work rq)
      $ GleanService.workCancelled rq
  workHeartbeat t rq =
    withShard t (Thrift.work_repo $ Thrift.workHeartbeat_work rq)
      $ GleanService.workHeartbeat rq
  workFinished t rq =
    withShard t (Thrift.work_repo $ Thrift.workFinished_work rq)
      $ GleanService.workFinished rq

  completePredicates t repo = withShard t repo $
    GleanService.completePredicates repo

  restoreDatabase t loc =
    withoutShard t $ GleanService.restore loc

  deleteDatabase t repo =
    withoutShard t $ GleanService.deleteDatabase repo

  enqueueBatch t cbatch =
    withShard t (Thrift.computedBatch_repo cbatch) $
      GleanService.sendBatch cbatch
  enqueueJsonBatch t repo batch =
    withShard t repo $ GleanService.sendJsonBatch repo batch
  pollBatch t handle = withoutShard t $ GleanService.finishBatch handle

  displayBackend = show

  hasDatabase ThriftBackend{..} repo = do
    let serv = thriftServiceWithDbShard thriftBackendService (Just (dbShard repo))
    hosts <- getSelection thriftBackendEventBase serv 1
    return (not (null hosts))

  maybeRemote = Just

  schemaId ThriftBackend{..} = thriftBackendSchemaId

withShard
  :: ThriftBackend
  -> Thrift.Repo
  -> Thrift GleanService a
  -> IO a
withShard (ThriftBackend ClientConfig{..} evb serv _ _) repo act =
  case clientConfig_use_shards of
    NO_SHARDS -> unsharded
    USE_SHARDS -> sharded
    USE_SHARDS_AND_FALLBACK -> do
      r <- try sharded
      case r of
        Right a -> return a
        Left e@(ChannelException msg) ->
          if "SELECTION_NO_HOST_FOR_SHARD" `Text.isInfixOf` msg then do
            logWarning $ "falling back to unsharded request: " <> show e
            unsharded
          else
            throwIO e
  where
    unsharded = runThrift evb serv act
    shard = dbShard repo
    sharded = runThrift evb (thriftServiceWithDbShard serv (Just shard)) act

withoutShard
  :: ThriftBackend
  -> Thrift GleanService a
  -> IO a
withoutShard (ThriftBackend _ evb serv _ _) req = runThrift evb serv req

dbShard :: Thrift.Repo -> DbShard
dbShard = Text.pack . show . dbShardWord

dbShardWord :: Thrift.Repo -> Word64
dbShardWord Thrift.Repo{..} =
  unsafeDupablePerformIO $ B.unsafeUseAsCStringLen repo $ \(ptr,len) -> do
      -- Use GHC's md5 binding. If this ever changes then the test in
      -- hs/tests/TestShard.hs will detect it.
    Fingerprint w _ <- fingerprintData (castPtr ptr) len
    return (w `shiftR` 1)
       -- SR doesn't like shards >= 0x8000000000000000
  where
  repo = Text.encodeUtf8 repo_name <> "/" <> Text.encodeUtf8 repo_hash


-- -----------------------------------------------------------------------------
-- Functionality built on Backend

loadPredicates
  :: Backend a
  => a
  -> Thrift.Repo
  -> [SchemaPredicates]
  -> IO Predicates
loadPredicates backend repo refs =
  makePredicates refs <$>
    getSchemaInfo backend repo def { Thrift.getSchemaInfo_omit_source = True }

databases :: Backend a => a -> IO [Thrift.Database]
databases be =
  Thrift.listDatabasesResult_databases <$>
    listDatabases be def { Thrift.listDatabases_includeBackups = True }

localDatabases :: Backend a => a -> IO [Thrift.Database]
localDatabases be =
  Thrift.listDatabasesResult_databases <$>
    listDatabases be def { Thrift.listDatabases_includeBackups = False }

usingShards :: Backend b => b -> Bool
usingShards backend =
  case maybeRemote backend of
    Just (ThriftBackend { thriftBackendClientConfig = ClientConfig{..} }) ->
      clientConfig_use_shards /= NO_SHARDS
    _otherwise -> False

clientInfo :: IO Thrift.UserQueryClientInfo
clientInfo = do
  unixname <- getUsername
  return def
    { Thrift.userQueryClientInfo_name = "api-haskell"
    , Thrift.userQueryClientInfo_unixname = Text.pack <$> unixname
    , Thrift.userQueryClientInfo_application = buildRule
    }


options :: Parser (ThriftSource ClientConfig)
options = optionsLong "service"

optionsLong :: String -> Parser (ThriftSource ClientConfig)
optionsLong self = do
  config <- option (eitherReader ThriftSource.parse)
    (  long "client-config"
    <> metavar "(file:PATH | config:PATH)"
    <> Options.value defaultClientConfigSource)
  let updateService svc config = config { clientConfig_serv = svc }
  service <- fmap updateService <$> optional (strOption
    (  long self
    <> metavar "TIER or HOST:PORT"
    <> help "Glean server to connect to"))
  let updateSharding sh config = config { clientConfig_use_shards = sh }
  sharding <- fmap updateSharding <$> optional (option readShard
    (  long "use-shards"
    <> metavar "yes|no|fallback"
    <> help ("Whether to specify a shard when connecting" <>
         " (default: fallback)")))
  return
    $ maybe id fmap service
    $ maybe id fmap sharding config
  where
    readShard = maybeReader $ \str -> case str of
      "yes" -> Just USE_SHARDS
      "no" -> Just NO_SHARDS
      "fallback" -> Just USE_SHARDS_AND_FALLBACK
      _ -> Nothing

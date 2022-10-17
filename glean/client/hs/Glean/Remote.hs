{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Remote
  ( -- * Command-line options
    options
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
  , clientInfo
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Typeable
import qualified Haxl.Core as Haxl
import Options.Applicative as Options

import Thrift.Channel
import Thrift.Api
import Util.EventBase (EventBaseDataplane)
import Util.Log

import Glean.Backend.Types
import Glean.BuildInfo (buildRule)
import Glean.ClientConfig.Types (UseShards(..), ClientConfig(..))
import Glean.DefaultConfigs
import Glean.GleanService.Client (GleanService)
import qualified Glean.GleanService.Client as GleanService
import qualified Glean.Types as Thrift
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Util.ConfigProvider
import Glean.Util.Service
import Glean.Username (getUsername)
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.ThriftService
import Glean.Impl.ThriftService
import Glean.Types

import Haxl.DataSource.Thrift
import Haxl.DataSource.Glean.Common

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
  -> (ThriftBackend -> IO a)
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
  -> (ThriftBackend -> IO a)
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

  completePredicates_ t repo = withShard t repo $
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
    let serv = thriftServiceWithDbShard thriftBackendService
          (Just (dbShard repo))
    hosts <- getSelection thriftBackendEventBase serv 1
    return (not (null hosts))

  usingShards (ThriftBackend ClientConfig{..} _ _ _ _) =
    clientConfig_use_shards /= NO_SHARDS

  schemaId ThriftBackend{..} = thriftBackendSchemaId

  initGlobalState = initRemoteGlobalState


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

-- -----------------------------------------------------------------------------
-- Haxl

initRemoteGlobalState
  :: ThriftBackend
  -> IO (Haxl.State GleanGet, Haxl.State GleanQuery)
initRemoteGlobalState backend =  do
  return
    ( GleanGetState (remoteFetch backend)
    , GleanQueryState (remoteQuery backend)
    )

remoteFetch :: ThriftBackend -> Haxl.PerformFetch GleanGet
remoteFetch (ThriftBackend config evb ts clientInfo schema) =
  Haxl.BackgroundFetch $ \requests -> do
  let
    ts' repo = case clientConfig_use_shards config of
      NO_SHARDS -> ts
      USE_SHARDS -> thriftServiceWithDbShard ts (Just (dbShard repo))
      USE_SHARDS_AND_FALLBACK ->
        thriftServiceWithDbShard ts (Just (dbShard repo)) -- TODO

  forM_ (HashMap.toList $ requestByRepo requests) $ \(repo, requests) -> do
    runThrift evb (ts' repo) $ do
      let
        sendCob :: Maybe ChannelException -> IO ()
        sendCob Nothing = return ()
        sendCob (Just ex) = putException (toException ex) requests

        recvCob
          :: (Response -> Either SomeException UserQueryResults)
          -> RecvCallback
        recvCob _ (Left ex) = putException (toException ex) requests
        recvCob deserialize (Right response) =
          case deserialize response of
            Left err -> putException err requests
            Right res -> putResults res requests

      dispatchCommon
        (\p c ct s r o x ->
          GleanService.send_userQueryFacts p c ct s r o repo x)
        sendCob
        (recvCob . GleanService.recv_userQueryFacts)
        (mkRequest (Just clientInfo) schema requests)

putException :: SomeException -> [Haxl.BlockedFetch a] -> IO ()
putException ex requests =
  forM_ requests $ \(Haxl.BlockedFetch _ rvar) -> Haxl.putFailure rvar ex


remoteQuery :: ThriftBackend -> Haxl.PerformFetch GleanQuery
remoteQuery (ThriftBackend config evb ts clientInfo schema) =
  Haxl.BackgroundFetch $ mapM_ fetch
  where
  ts' repo = case clientConfig_use_shards config of
    NO_SHARDS -> ts
    USE_SHARDS -> thriftServiceWithDbShard ts (Just (dbShard repo))
    USE_SHARDS_AND_FALLBACK ->
      thriftServiceWithDbShard ts (Just (dbShard repo)) -- TODO

  fetch :: Haxl.BlockedFetch GleanQuery -> IO ()
  fetch (Haxl.BlockedFetch (QueryReq q repo stream) rvar) =
    runRemoteQuery evb repo q' (ts' repo) acc rvar
    where
      q' = withClientInfo clientInfo q
      acc = if stream then Just id else Nothing

  withClientInfo :: UserQueryClientInfo -> Query q -> Query q
  withClientInfo info (Query q) = Query q'
    where
      q' = q {
        userQuery_client_info = Just info,
        userQuery_schema_id = schema
      }


runRemoteQuery
  :: forall q. (Show q, Typeable q)
  => EventBaseDataplane
  -> Repo
  -> Query q
  -> ThriftService GleanService
  -> Maybe ([q] -> [q]) -- results so far
  -> Haxl.ResultVar ([q], Bool)
  -> IO ()
runRemoteQuery evb repo q@(Query req) ts acc rvar =
  runThrift evb ts $ do
    let
      recvCob
        :: (Response -> Either SomeException UserQueryResults)
        -> RecvCallback
      recvCob _ (Left ex) = Haxl.putFailure rvar (toException ex)
      recvCob deserialize (Right response) =
        case deserialize response of
          Left err -> Haxl.putFailure rvar err
          Right res -> putQueryResults q res acc rvar $
            \(q :: Query q) acc -> runRemoteQuery evb repo q ts acc rvar

    dispatchCommon
      (\p c ct s r o x -> GleanService.send_userQuery p c ct s r o repo x)
      (sendCobSingle rvar)
      (recvCob . GleanService.recv_userQuery)
      req

module Glean.Backend.Remote
  ( Backend(..)

    -- * Construction
  , withRemoteBackend
  , withRemoteBackendConfig
  , Settings
  , setService
  , setNoShards
  , setTimeout
  , thriftServiceWithTimeout
  , ThriftBackend(..)
  , defaultClientConfigSource

    -- * Shards
  , Shard
  , dbShard

    -- * More operations
  , SchemaPredicates
  , loadPredicates
  , databases
  , localDatabases
  , fillDatabase
  , usingShards
  , clientInfo

  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Unsafe as B
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign
import GHC.Fingerprint
import System.IO.Unsafe
import TextShow (showt)

import Thrift.Channel
import Thrift.Api
import Util.Control.Exception
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
import Glean.Impl.ThriftService


-- |
-- An abstraction over Glean's Thrift API. This allows client code
-- to work with either a local or remote backend, chosen at runtime.
--
class Backend a where
  queryFact :: a -> Thrift.Repo -> Thrift.Id -> IO (Maybe Thrift.Fact)
  firstFreeId :: a -> Thrift.Repo -> IO Thrift.Id
  factIdRange :: a -> Thrift.Repo -> IO Thrift.FactIdRange
  getSchemaInfo :: a -> Thrift.Repo -> IO Thrift.SchemaInfo
  validateSchema :: a -> Thrift.ValidateSchema -> IO ()
  predicateStats :: a -> Thrift.Repo -> IO (Map Thrift.Id Thrift.PredicateStats)
  listDatabases :: a -> Thrift.ListDatabases -> IO Thrift.ListDatabasesResult
  getDatabase :: a -> Thrift.Repo -> IO Thrift.GetDatabaseResult

  userQueryFacts :: a -> Thrift.Repo -> Thrift.UserQueryFacts
    -> IO Thrift.UserQueryResults
  userQuery :: a -> Thrift.Repo -> Thrift.UserQuery
    -> IO Thrift.UserQueryResults

  derivePredicate :: a -> Thrift.Repo -> Thrift.DerivePredicateQuery
    -> IO Thrift.DerivePredicateResponse
  pollDerivation :: a -> Thrift.Handle -> IO Thrift.DerivationProgress

  kickOffDatabase :: a -> Thrift.KickOff -> IO Thrift.KickOffResponse

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


-- | A remote Glean service, supports the operations of 'Backend'.
data ThriftBackend = ThriftBackend
  { thriftBackendClientConfig :: ClientConfig
  , thriftBackendEventBase :: EventBaseDataplane
  , thriftBackendService :: ThriftService GleanService
  , thriftBackendClientInfo :: Thrift.UserQueryClientInfo
  }

instance Show ThriftBackend where
  show tb = unwords [ "ThriftBackend {(",
    "thriftBackendClientConfig: (" <> show (thriftBackendClientConfig tb),
    "), thriftBackendService: (" <> show (thriftBackendService tb ),
    "), thriftBackendClientInfo: (" <> show (thriftBackendClientInfo tb ), ")}"]


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
  derivePredicate (Some backend) = derivePredicate backend
  pollDerivation (Some backend) = pollDerivation backend

  kickOffDatabase (Some backend) = kickOffDatabase backend
  updateProperties (Some backend) = updateProperties backend
  getWork (Some backend) = getWork backend
  workCancelled (Some backend) = workCancelled backend
  workHeartbeat (Some backend) = workHeartbeat backend
  workFinished (Some backend) = workFinished backend

  restoreDatabase (Some backend) = restoreDatabase backend
  deleteDatabase (Some backend) = deleteDatabase backend

  enqueueBatch (Some backend) = enqueueBatch backend
  enqueueJsonBatch (Some backend) = enqueueJsonBatch backend
  pollBatch (Some backend) = pollBatch backend
  displayBackend (Some backend) = displayBackend backend
  hasDatabase (Some backend) = hasDatabase backend
  maybeRemote (Some backend) = maybeRemote backend

withRemoteBackendConfig
  :: EventBaseDataplane
  -> ClientConfig
  -> (forall b . Backend b => b -> IO a)
  -> IO a
withRemoteBackendConfig evb config inner = do
  client <- clientInfo
  inner $ ThriftBackend config evb (thriftServiceWithTimeout config def) client

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
  -> Settings
  -> (forall b . Backend b => b -> IO a)
  -> IO a
withRemoteBackend evb configAPI settings inner = do
  config <- ThriftSource.loadDefault configAPI defaultClientConfigSource
  client <- clientInfo
  let (config', opts) = settings (config, def)
  inner $ ThriftBackend
    config
    evb
    (thriftServiceWithTimeout config' opts)
    client


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
  getSchemaInfo t repo = withShard t repo $ GleanService.getSchemaInfo repo
  validateSchema t req = withoutShard t $ GleanService.validateSchema req
  predicateStats t repo = withShard t repo $ GleanService.predicateStats repo
  listDatabases t l = withoutShard t $ GleanService.listDatabases l
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

  derivePredicate t repo pred = withShard t repo $
    GleanService.derivePredicate repo pred
      { Thrift.derivePredicateQuery_client_info = client }
    where
      client = Thrift.derivePredicateQuery_client_info pred
        <|> Just (thriftBackendClientInfo t)

  pollDerivation t handle = withoutShard t $
    GleanService.pollDerivation handle

  kickOffDatabase t rq = withoutShard t $ GleanService.kickOff rq
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
    let serv = thriftServiceWithShard thriftBackendService (Just (dbShard repo))
    hosts <- getSelection thriftBackendEventBase serv 1
    return (not (null hosts))

  maybeRemote = Just

withShard
  :: ThriftBackend
  -> Thrift.Repo
  -> Thrift GleanService a
  -> IO a
withShard (ThriftBackend ClientConfig{..} evb serv _) repo act =
  case clientConfig_use_shards of
    NO_SHARDS -> unsharded
    USE_SHARDS -> sharded
    USE_SHARDS_AND_FALLBACK -> do
      r <- try sharded
      case r of
        Right a -> return a
        Left e@(ChannelException msg) ->
          if "no hosts were found" `Text.isSuffixOf` msg then do
            logWarning $ "falling back to unsharded request: " <> show e
            unsharded
          else
            throwIO e
  where
    unsharded = runThrift evb serv act
    shard = dbShard repo
    sharded = runThrift evb (thriftServiceWithShard serv (Just shard)) act

withoutShard
  :: ThriftBackend
  -> Thrift GleanService a
  -> IO a
withoutShard (ThriftBackend _ evb serv _) req = runThrift evb serv req

dbShard :: Thrift.Repo -> Shard
dbShard Thrift.Repo{..} =
  unsafeDupablePerformIO $ B.unsafeUseAsCStringLen repo $ \(ptr,len) -> do
      -- Use GHC's md5 binding. If this ever changes then the test in
      -- hs/tests/TestShard.hs will detect it.
    Fingerprint w _ <- fingerprintData (castPtr ptr) len
    return $ Text.pack (show (w `shiftR` 1))
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
  makePredicates refs <$> getSchemaInfo backend repo

databases :: Backend a => a -> IO [Thrift.Database]
databases be =
  Thrift.listDatabasesResult_databases <$>
    listDatabases be def { Thrift.listDatabases_includeBackups = True }

localDatabases :: Backend a => a -> IO [Thrift.Database]
localDatabases be =
  Thrift.listDatabasesResult_databases <$>
    listDatabases be def { Thrift.listDatabases_includeBackups = False }

fillDatabase
  :: Backend a
  => a
  -> Thrift.Repo
  -> Text
  -> IO () -- ^ @return ()@ to allow existing, or @throwIO@ to forbid
  -> IO b
  -> IO b
fillDatabase env repo handle ifexists action = tryBracket
  (do
    r <- kickOffDatabase env def
      { Thrift.kickOff_repo = repo
      , Thrift.kickOff_fill = Just $ Thrift.KickOffFill_writeHandle handle
      }
    when (Thrift.kickOffResponse_alreadyExists r) ifexists)
  (\_ e -> workFinished env Thrift.WorkFinished
    { workFinished_work = def
        { Thrift.work_repo = repo
        , Thrift.work_handle = handle
        }
    , workFinished_outcome = case e of
        Left ex -> Thrift.Outcome_failure (Thrift.Failure (showt ex))
        Right _ -> Thrift.Outcome_success def
    })
  $ const action


usingShards :: Backend b => b -> Bool
usingShards backend =
  case maybeRemote backend of
    Just (ThriftBackend ClientConfig{..} _ _ _) ->
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

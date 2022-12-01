{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- | Connecting to a Glean server or a local database store.
--
-- Client code that wants to be able to use either a remote
-- Glean server or a local database store should look like:
--
-- > import Glean
-- > import Glean.LocalOrRemote
-- > import Glean.Util.ConfigProvider
-- > import Glean.Impl.ConfigProvider
-- > import Glean.Schema.Builtin.Types (schema_id)
-- > import Util.EventBase
-- >
-- > main :: IO ()
-- > main =
-- >   withConfigOptions options $ \(service, cfgOpts) ->
-- >   withEventBaseDataplane $ \evb ->
-- >   withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
-- >   withBackendWithDefaultOptions evb cfgAPI service (Just schema_id)
-- >       $ \backend -> do
-- >     ...
--
module Glean.LocalOrRemote
  (
    -- * Connecting to a remote server or a local database store
    options
  , optionsLong
  , withBackendWithDefaultOptions
  , withBackend
  , defaultClientConfigSource
  , Service(..)
  , Logging(..)
  , LocalOrRemote(..)
  , LoggingBackend(..)
  , BackendKind(..)

  -- * Misc
  , validate
  , Validate(..)
  , dumpJsonToFile
  , finalize
  , sendJsonBatch

    -- * Schemas
  , loadDbSchema
  , serializeInventory
  ) where

import Control.Monad.Extra
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.IORef
import Options.Applicative as O
import System.IO
import Text.Printf

import Util.EventBase

import Glean hiding (finalize, sendJsonBatch)
  -- we will provide versions of finalize and sendJsonBatch that are
  -- more efficient with local DBs.
import qualified Glean
import Glean.Backend.Local hiding (options)
import qualified Glean.Backend.Local as Local
import Glean.Backend.Logging
import Glean.DefaultConfigs
import Glean.Dump
import Glean.Database.Validate
import Glean.Database.Work (finalizeWait)
import qualified Glean.Remote as Remote
import Glean.Util.ConfigProvider
import Glean.Util.Some
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Write.JSON (syncWriteJsonBatch)


data Logging = EnableLogging | DisableLogging
  deriving (Eq, Show)

-- | Specifies what kind of 'Backend' to construct.
data Service
  = Local Local.Config Logging
  | Remote (ThriftSource ClientConfig)
  deriving Show

-- | Use the provided 'Service' to make a 'Backend'.  (note in fact
-- that it provides a 'LocalOrRemote', which is a 'Backend' that
-- additionally supports 'backendKind').
withBackendWithDefaultOptions
  :: ConfigProvider conf
  => EventBaseDataplane
  -> conf
  -> Service
  -> Maybe SchemaId
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackendWithDefaultOptions evb cfgapi service schema =
  withBackend evb cfgapi service schema id

-- | Use the provided 'Service' to make a 'Backend', applying some
-- 'Settings' if this is a remote backend. (note in fact that it
-- provides a 'LocalOrRemote', which is a 'Backend' that additionally
-- supports 'backendKind').
withBackend
  :: ConfigProvider conf
  => EventBaseDataplane
  -> conf
  -> Service
  -> Maybe SchemaId
  -> Remote.Settings
  -> (forall b. LocalOrRemote b => b -> IO a)
  -> IO a
withBackend evb cfgapi service schema settings inner = case service of
  Local cfg logging ->
    let cfg' = cfg { cfgSchemaId = schema } in
    withDatabases evb cfg' cfgapi $
      case logging of
        EnableLogging -> inner . LoggingBackend
        DisableLogging -> inner
  Remote src -> do
    config <- ThriftSource.loadDefault cfgapi src
    let (config', opts) = settings (config, def)
    client <- Remote.clientInfo
    inner $ Remote.ThriftBackend
      config'
      evb
      (Remote.thriftServiceWithTimeout config' opts)
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
  Local <$> Local.options <*> logging
  where
    logging = (\b -> if b then EnableLogging else DisableLogging) <$> O.switch
      (  O.long "enable-logging"
      <> O.help "Log requests to Scuba/Hive/..."
      )

-- -----------------------------------------------------------------------------
-- Backends that might be local or remote


data BackendKind
  = BackendEnv Local.Env
  | BackendThrift Remote.ThriftBackend

displayBackendKind :: BackendKind -> String
displayBackendKind BackendEnv{} = "BackendKind BackendEnv {_ :: Local.Env}"
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

instance LocalOrRemote Local.Env where
  backendKind env = BackendEnv env

instance LocalOrRemote Remote.ThriftBackend where
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
  userQueryBatch (Some backend) = userQueryBatch backend
  deriveStored (Some backend) = deriveStored backend

  kickOffDatabase (Some backend) = kickOffDatabase backend
  finalizeDatabase (Some backend) = finalizeDatabase backend
  updateProperties (Some backend) = updateProperties backend
  getWork (Some backend) = getWork backend
  workCancelled (Some backend) = workCancelled backend
  workHeartbeat (Some backend) = workHeartbeat backend
  workFinished (Some backend) = workFinished backend
  completePredicates_ (Some backend) = completePredicates_ backend

  restoreDatabase (Some backend) = restoreDatabase backend
  deleteDatabase (Some backend) = deleteDatabase backend

  enqueueBatch (Some backend) = enqueueBatch backend
  enqueueJsonBatch (Some backend) = enqueueJsonBatch backend
  pollBatch (Some backend) = pollBatch backend
  displayBackend (Some backend) = displayBackend backend
  hasDatabase (Some backend) = hasDatabase backend
  schemaId (Some backend) = schemaId backend
  usingShards (Some backend) = usingShards backend
  initGlobalState (Some backend) = initGlobalState backend

instance LocalOrRemote (Some LocalOrRemote) where
  backendKind (Some b) = backendKind b


-- | Write facts to a file in JSON format suitable for parsing using
-- 'parseJsonFactBatches'.
--
dumpJsonToFile
  :: Backend b
  => b
  -> Glean.Repo
  -> FilePath
  -> IO ()
dumpJsonToFile backend repo file =
  withFile file WriteMode $ \hdl -> do
    notFirst <- newIORef False
    hPutStrLn hdl "["
    dump backend repo (withBatch hdl notFirst)
    hPutStrLn hdl "]"
  where
    withBatch hdl notFirst Glean.JsonFactBatch{..} = do
      whenM (readIORef notFirst) $ hPutStr hdl ","
      writeIORef notFirst True
      let Glean.PredicateRef{..} = jsonFactBatch_predicate
      hPrintf hdl "{ \"predicate\": \"%s.%d\", \"facts\": [\n"
        predicateRef_name predicateRef_version
      BC.hPutStrLn hdl (BC.intercalate ",\n" jsonFactBatch_facts)
      hPutStrLn hdl "]}"

finalize :: LocalOrRemote backend => backend -> Glean.Repo -> IO ()
finalize backend repo =
  case backendKind backend of
    -- finalizeWait is faster than polling if we have local DBs.
    BackendEnv env -> finalizeWait env repo
    _ -> Glean.finalize backend repo

-- | Version of sendJsonBatch that uses a synchronous write rather
-- than polling when we're using a local backend.
sendJsonBatch
  :: LocalOrRemote be
  => be
  -> Glean.Repo
  -> [Glean.JsonFactBatch]
  -> Maybe Glean.SendJsonBatchOptions
  -> IO ()
sendJsonBatch backend repo batches opts = do
  case backendKind backend of
    -- syncWriteJsonBatch is faster than polling if we have local DBs.
    BackendEnv env -> syncWriteJsonBatch env repo batches opts
    _ -> void $ Glean.sendJsonBatch backend repo batches opts

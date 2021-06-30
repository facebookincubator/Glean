-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Server (main) where

import Control.Concurrent
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List
import Data.Maybe
import qualified Options.Applicative as O
import System.Exit (die)

import Facebook.Fb303
import Facebook.Service
import Fb303Core.Types
import Thrift.Processor
import qualified Thrift.Server.CppServer as CppServer
import Thrift.Server.Types
import Util.Control.Exception
import Util.EventBase
import Util.Log

import Glean.Backend.Remote hiding (options)
import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.Catalog.Filter as Catalog
import Glean.Database.Env
import Glean.Database.Types
#if FACEBOOK
import qualified Glean.Search.Handler as SearchHandler
#endif
import qualified Glean.Handler as GleanHandler
import Glean.Server.Config as Config
import Glean.Server.Shard
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.Periodic
import Glean.Util.Time

databasesUpdatedCallback
  :: EventBaseDataplane
  -> ShardKey
  -> MVar (Maybe [Shard])
  -> HashSet Repo
  -> IO ()
databasesUpdatedCallback evb shardKey currentShards dbs = swallow $ do
  modifyMVar_ currentShards $ \prevShards -> do
    let newShards = sort $ map dbShard $ HashSet.toList dbs

    if Just newShards == prevShards then do
      logInfo $ "no change in shards: " <> show newShards
      return prevShards
    else do
      updateShards evb shardKey newShards
      return (Just newShards)

withHandler
  :: Config
  -> GleanHandler.State
  -> (forall s. Processor s => (forall r. s r -> IO r) -> IO a)
  -> IO a
withHandler cfg state cont =
  case cfgHandler cfg of
    "glean" -> cont $ GleanHandler.handler state
#if FACEBOOK
    "search" -> cont $ SearchHandler.searchHandler state
#endif
    _ -> throwIO $ ErrorCall "--handler: invalid value"

-- The 'dbUpdateNotifierThread' will sit in a loop waiting for changes
-- to the local databases (using STM retry to detect changes).  When
-- changes are detected, it waits 10s so that multiple changes are
-- processed in a single batch, and then invokes the callback.
--
-- The doPeriodically on the outside is just a fallback in case something
-- goes wrong; it ensures that the exception is caught and logged, and we
-- don't immediately retry in a loop.
--
dbUpdateNotifierThread :: Env -> (HashSet Repo -> IO ()) -> IO ()
dbUpdateNotifierThread Env{..} callback = doPeriodically (seconds 30) $ do
  initial <- updated
  go initial
  where
  go prev = do
    atomically $ do
      dbs <- list
      when (dbs == prev) retry
    vlog 1 "DB update detected, waiting 10s"
    threadDelay 10000000  -- wait 10s so we can batch updates
    current <- updated
    go current

  list = HashSet.fromList . map Catalog.itemRepo
    <$> Catalog.list envCatalog [Catalog.Local] Catalog.everythingF

  updated = do
    dbs <- atomically list
    vlog 1 "DB update notification"
    callback dbs
    return dbs

withShardsUpdater :: EventBaseDataplane -> Config -> Env -> IO a -> IO a
withShardsUpdater evb cfg env action
  | cfgSetShards cfg = do
    port <- case cfgPort cfg of
      Just port -> return port
      Nothing -> die "--set-shards requires --port"
    currentShards <- newMVar Nothing
    key <- getShardKey evb port
    withAsync
      (dbUpdateNotifierThread env
        $ databasesUpdatedCallback evb key currentShards)
      $ const action
  | otherwise = action

main :: IO ()
main =
  withConfigOptions (O.info options O.fullDesc) $ \(cfg, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \configAPI ->
  withDatabases evb (cfgDBConfig cfg) configAPI $ \databases ->
  withShardsUpdater evb cfg databases $ do

  fb303 <- newFb303 "gleandriver"
  let state = GleanHandler.State fb303 databases

  logInfo "Starting server"

  let
    setAlive server = do
      let port = show $ CppServer.serverPort server
      forM_ (cfgWritePort cfg) $ \path -> writeFile path port
      logInfo $ "server alive on port " ++ port
      writeIORef (fb303_status fb303) Fb303_status_ALIVE

    -- If the janitor is enabled, wait until it has run once to
    -- completion before we advertise the server as alive.  Otherwise
    -- clients may contact this server and see no available DBs.
    waitForAlive server = do
      atomically $ do
        l <- readTVar $ envDatabaseJanitor databases
        when (isNothing l) retry
      setAlive server

  withHandler cfg state $ \handler ->
    withBackgroundFacebookServiceDeferredAlive
      (GleanHandler.fb303State state)
      handler
      defaultOptions{ desiredPort = cfgPort cfg }
      (\server -> waitForAlive server >> waitForTerminateSignals)

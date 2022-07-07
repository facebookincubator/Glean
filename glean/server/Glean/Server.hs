{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Server (main) where

import Control.Concurrent
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
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
import qualified Thrift.Server.CppServer as CppServer
import Thrift.Server.Types
import Util.Control.Exception
import Util.EventBase
import Util.Log

import Glean.Backend.Remote hiding (options)
import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.Catalog.Filter as Catalog
import Glean.Database.Config (cfgShardManager)
import Glean.Database.Env
import Glean.Database.Types
import qualified Glean.Handler as GleanHandler
import qualified Glean.Index as Index
import Glean.Index.GleanIndexingService.Service
import Glean.Server.Config as Config
import Glean.Server.PublishShards
import Glean.Server.Sharding (shardManagerConfig)
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.Periodic
import Glean.Util.Time

databasesUpdatedCallback
  :: EventBaseDataplane
  -> ShardKey
  -> MVar (Maybe [DbShard])
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
    <$> Catalog.list envCatalog [Catalog.Local] Catalog.queryableF

  updated = do
    dbs <- atomically list
    vlog 1 "DB update notification"
    callback dbs
    return dbs

withShardsUpdater :: EventBaseDataplane -> Config -> Env -> IO a -> IO a
withShardsUpdater evb cfg env action
  | cfgPublishShards cfg = do
    port <- case cfgPort cfg of
      Just port -> return port
      Nothing -> die "--publish-shards requires --port"
    currentShards <- newMVar Nothing
    key <- getShardKey evb port
    withAsync
      (dbUpdateNotifierThread env
        $ databasesUpdatedCallback evb key currentShards)
      $ const action
  | otherwise = action

main :: IO ()
main =
  withConfigOptions (O.info options O.fullDesc) $ \(cfg0, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \configAPI ->
  let dbCfg = (cfgDBConfig cfg0){
        cfgShardManager = shardManagerConfig (cfgPort cfg)
      }
      cfg = cfg0{cfgDBConfig = dbCfg}
  in
  withDatabases evb (cfgDBConfig cfg) configAPI $ \databases ->
  withShardsUpdater evb cfg databases $ do

  fb303 <- newFb303 "gleandriver"

  logInfo "Starting server"
  portVar <- newTVarIO Nothing

  let
    setAlive server = do
      let port = CppServer.serverPort server
      atomically $ writeTVar portVar (Just port)
      forM_ (cfgWritePort cfg) $ \path -> writeFile path (show port)
      logInfo $ "server alive on port " ++ show port
      writeIORef (fb303_status fb303) Fb303_status_ALIVE

    -- If the janitor is enabled, wait until it has run once to
    -- completion before we advertise the server as alive.  Otherwise
    -- clients may contact this server and see no available DBs.
    waitForAlive server = do
      atomically $ do
        l <- readTVar $ envDatabaseJanitor databases
        when (isNothing l) retry
      setAlive server

    waitToStart server = waitForAlive server >> waitForTerminateSignals
    opts = defaultOptions{ desiredPort = cfgPort cfg }

    getPort =
      fromMaybe (error "server hasn't started yet") <$> readTVarIO portVar

  let state = GleanHandler.State fb303 databases

  if cfgEnableIndexing cfg
    then
      withBackgroundFacebookServiceDeferredAlive
        (GleanHandler.fb303State state)
        (handlerIndexing state getPort)
        opts
        waitToStart
    else
      withBackgroundFacebookServiceDeferredAlive
        (GleanHandler.fb303State state)
        (GleanHandler.handler state)
        opts
        waitToStart

handlerIndexing
  :: GleanHandler.State
  -> IO Int -- ^ get the port the server is running on
  -> GleanIndexingServiceCommand a
  -> IO a
handlerIndexing state getPort req = case req of
  Index r -> Index.index getPort (GleanHandler.stEnv state) r
  SuperGleanService r -> GleanHandler.handler state r

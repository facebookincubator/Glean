{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Server (main) where

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Options.Applicative as O
import System.Time.Extra (Seconds)

import Facebook.Fb303
import Facebook.Service
import Fb303Core.Types
#if FBTHRIFT
import qualified Thrift.Server.CppServer as ThriftServer
#else
import qualified Thrift.Server.HTTP as ThriftServer
#endif
import Util.EventBase
import Util.Log
import Util.STM

#if GLEAN_FACEBOOK
import Logger.IO
import Glean.Facebook.Logger.Server
import Glean.Facebook.Logger.Database
#endif

#ifdef GLEAN_FACEBOOK
import qualified Glean.Database.Backup.Manifold as Manifold
#endif

import Glean.Database.Config (Config(..))
import Glean.Database.Env
import Glean.Database.Types
import qualified Glean.Handler as GleanHandler
import Glean.Impl.ConfigProvider (ConfigAPI)
import qualified Glean.Index as Index
import Glean.Index.GleanIndexingService.Service
import Glean.Server.Config as Config
import Glean.Server.Sharding (
  shardManagerConfig,
  withShardsUpdater,
  waitForTerminateSignalsAndGracefulShutdown)
import Glean.Util.ConfigProvider
import Glean.Util.Some

main :: IO ()
main =
  withConfigOptions (O.info options O.fullDesc) $ \(cfg0, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(configAPI :: ConfigAPI) ->
#if GLEAN_FACEBOOK
  withLogger configAPI $ \logger ->
#endif
  let dbCfg = (cfgDBConfig cfg0){
        cfgShardManager = shardManagerConfig (cfgPort cfg)
#if GLEAN_FACEBOOK
        , cfgServerLogger = Some (GleanServerFacebookLogger logger)
        , cfgDatabaseLogger = Some (GleanDatabaseFacebookLogger logger)
#endif
      }

#if GLEAN_FACEBOOK
      cfg = cfg0{cfgDBConfig = Manifold.withManifoldBackups evb dbCfg}
#else
      cfg = cfg0{cfgDBConfig = dbCfg}
#endif
  in
  withDatabases evb (cfgDBConfig cfg) configAPI $ \databases -> do
  terminating <- newTVarIO False
  withShardsUpdater evb cfg databases (1 :: Seconds) (readTVar terminating) $ do

  fb303 <- newFb303 "gleandriver"

  logInfo "Starting server"
  portVar <- newTVarIO Nothing

  let
    setAlive server = do
      let port = ThriftServer.serverPort server
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

    waitForTerminate = waitForTerminateSignalsAndGracefulShutdown
                        databases
                        terminating
                        (cfgGracefulShutdownTimeout cfg)

    waitToStart server = waitForAlive server >> waitForTerminate
    opts = ThriftServer.defaultOptions {
      ThriftServer.desiredPort = cfgPort cfg }

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

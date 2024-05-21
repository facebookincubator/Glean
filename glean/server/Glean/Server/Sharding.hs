{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Server.Sharding (
  shardManagerConfig,
  withShardsUpdater,
  waitForTerminateSignalsAndGracefulShutdown,
  -- for testing
  dbUpdateNotifierThread) where

import Control.Concurrent (
  modifyMVar_,
  newMVar,
  MVar,
  newEmptyMVar,
  tryPutMVar,
  takeMVar)
import Control.Concurrent.Async ( withAsync )
import Control.Exception
import Control.Monad ( when, void, unless )
import Data.HashSet (HashSet, toList)
import qualified Data.HashSet as HashSet
import Data.List (sort)
#if GLEAN_FACEBOOK
import Data.Maybe
#endif
import qualified Data.Set as Set
import GHC.Conc (unsafeIOToSTM)
import Glean.Backend.Types (dbShard)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter as Catalog
import Glean.Database.Types ( Env(..) )
#if GLEAN_FACEBOOK
import Glean.Impl.ShardManager
#endif
import Glean.Server.Config (Config, cfgPort, cfgPublishShards)
import Glean.Server.PublishShards ( ShardKey, getShardKey, updateShards )
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types ( Repo )
import Glean.Util.Observed (Observed)
import qualified Glean.Util.Observed as Observed
import Glean.Util.Periodic ( doPeriodically )
import Glean.Util.ShardManager
import Glean.Util.ThriftService (DbShard)
import Util.Time ( seconds )
import System.Exit (die)
import System.Posix.Signals (installHandler, sigTERM, Handler (Catch), sigINT)
import System.Time.Extra ( showDuration, sleep, Seconds )
import Util.Control.Exception ( swallow )
import Util.EventBase ( EventBaseDataplane )
import Util.Log ( vlog, logInfo )
import Util.STM
  (retry, atomically, writeTVar, STM, TVar, readTVar, registerDelay)


type PortNumber = Int

shardManagerConfig ::
  -- | Application port
  Maybe PortNumber ->
  Catalog.Catalog ->
  Observed ServerConfig.Config ->
  (SomeShardManager -> IO b) ->
  IO b
shardManagerConfig _mbPort catalog smCfgServerConfig callback = do
  config <- Observed.get smCfgServerConfig
  case ServerConfig.config_sharding config of
    ServerConfig.ShardingPolicy_no_shards {} ->
      callback $ SomeShardManager noSharding
    ServerConfig.ShardingPolicy_static_assignment {} ->
      callback $
        SomeShardManager $
          shardByRepo $ do
            config <- Observed.get smCfgServerConfig
            case ServerConfig.config_sharding config of
              ServerConfig.ShardingPolicy_static_assignment assignment ->
                return $
                  Just $
                    Set.toList $
                      ServerConfig.staticShardsPolicy_shards assignment
              _ ->
                return Nothing
#if GLEAN_FACEBOOK
    ServerConfig.ShardingPolicy_shard_manager policy -> do
      let smCliArgs = ShardManagerClientArgs
            { serviceName = ServerConfig.shardManagerPolicy_service_name policy
            , applicationPortNumber = fromMaybe 0 _mbPort
            , numberOfShards = fromIntegral $
                ServerConfig.shardManagerPolicy_nshards policy
            , defaultDomainName =
              ServerConfig.shardManagerPolicy_default_domain_id policy
            }
      withShardManager smCliArgs $ \sm -> callback $ SomeShardManager sm
    ServerConfig.ShardingPolicy_shard_manager_most_recent policy -> do
      let ServerConfig.ShardManagerMostRecentPolicy{
            ServerConfig.shardManagerMostRecentPolicy_shard_manager_policy =
              ServerConfig.ShardManagerPolicy{..},
            .. } = policy
          smCliArgs = ShardManagerClientArgs
            { serviceName = shardManagerPolicy_service_name
            , applicationPortNumber = fromMaybe 0 _mbPort
            , numberOfShards = fromIntegral shardManagerPolicy_nshards
            , defaultDomainName = shardManagerPolicy_default_domain_id
            }
          getMostRecent =
            Set.fromList . map itemRepo <$> Catalog.listMostRecent catalog

      withShardManagerForMostRecent
        smCliArgs
        shardManagerMostRecentPolicy_most_recent_domain_id
        getMostRecent $ \sm -> callback $ SomeShardManager sm
#endif
    other ->
      error $ "Unsupported sharding policy: " <> show other

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
-- changes are detected, it waits 1s so that multiple changes are
-- processed in a single batch, and then invokes the callback.
--
-- The doPeriodically on the outside is just a fallback in case something
-- goes wrong; it ensures that the exception is caught and logged, and we
-- don't immediately retry in a loop.
--
dbUpdateNotifierThread
  :: Env
  -> Seconds
  -> STM Bool  -- ^ is the server gracefullly shutting down?
  -> (HashSet Repo -> IO ())
  -> IO ()
dbUpdateNotifierThread env delay terminating callback =
  doPeriodically (seconds 30) $ do
    initial <- updated
    go initial
    where
    go prev = do
      atomically $ do
        termSignal <- terminating
        dbs <- fetchCurrentShardList env termSignal
        when (dbs == prev) retry

      vlog 1 $ "DB update detected, waiting " <> showDuration delay
      sleep delay -- wait 1s so we can batch updates
      current <- updated
      go current

    updated = do
      termSignal <- atomically terminating
      dbs <- atomically $ fetchCurrentShardList env termSignal
      vlog 1 "DB update notification"
      callback dbs
      return dbs

withShardsUpdater
  :: EventBaseDataplane
  -> Config
  -> Env
  -> Seconds
  -> STM Bool  -- ^ is the server gracefullly shutting down?
  -> IO a
  -> IO a
withShardsUpdater evb cfg env delay terminating action
  | cfgPublishShards cfg = do
    port <- case cfgPort cfg of
      Just port -> return port
      Nothing -> die "--publish-shards requires --port"
    currentShards <- newMVar Nothing
    key <- getShardKey evb port
    withAsync
      (dbUpdateNotifierThread env delay terminating
        $ databasesUpdatedCallback evb key currentShards)
      $ const action
  | otherwise = action


waitForTerminateSignalsAndGracefulShutdown
  :: Env
  -> TVar Bool -- ^ broadcast channel for initiating the timeout
  -> Seconds -- ^ amount of time to wait before forcing a shutdown
  -> IO ()
waitForTerminateSignalsAndGracefulShutdown env terminating timeout = do
  -- To wait in Haskell-land while the server is taking requests,
  -- use an mvar that gets filled when the right signals are read
  mvar <- newEmptyMVar
  let sigHandler = void $ tryPutMVar mvar ()
  withSignalHandler sigTERM sigHandler $ \_ ->
    withSignalHandler sigINT sigHandler $ \_ -> do
      -- Haskell will wait here until being instructed to stop
      takeMVar mvar
      logInfo "SIGTERM/SIGINT received"

      -- stop publishing complete shards
      atomically $ writeTVar terminating True

      -- start the timeout (if any)
      timeoutElapsedSTM <- if timeout > 0
        then readTVar <$> registerDelay (floor $ timeout * 1000000)
        else return $ pure True

      -- block until we do not advertise any shards anymore or run out of time
      atomically $ do
        dbs <- list
        timeoutElapsed <- timeoutElapsedSTM

        -- terminate when either the list is empty or we exceed the timeout
        unless (null dbs || timeoutElapsed) $ do
          unsafeIOToSTM $ logInfo $
            "Waiting for incomplete dbs: " <> show (toList dbs)
          retry

      logInfo "Shutting down"

  where
    withSignalHandler sig h = bracket
      (installHandler sig (Catch h) Nothing)
      (\old -> installHandler sig old Nothing)

    list = fetchCurrentShardList env True


fetchCurrentShardList :: Env -> Bool -> STM (HashSet Repo)
fetchCurrentShardList Env{..} terminating =
    let filter = if terminating
                  then incompleteQueryableF
                  else queryableF
    in
    HashSet.fromList . map Catalog.itemRepo
      <$> Catalog.list envCatalog [Catalog.Local] filter

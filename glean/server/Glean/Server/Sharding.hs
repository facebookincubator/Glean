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
  -- for testing
  dbUpdateNotifierThread,
  ) where

import Control.Concurrent ( modifyMVar_, newMVar, MVar )
import Control.Concurrent.Async ( withAsync )
import Control.Concurrent.STM ( retry, atomically )
import Control.Monad ( when )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sort)
#if FACEBOOK
import Data.Maybe
#endif
import qualified Data.Set as Set
import Glean.Backend.Remote (DbShard, dbShard)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter as Catalog
import Glean.Database.Types ( Env(..) )
#if FACEBOOK
import Glean.Impl.ShardManager
#endif
import Glean.Server.Config (Config, cfgPort, cfgPublishShards)
import Glean.Server.PublishShards ( ShardKey, getShardKey, updateShards )
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types
import Glean.Util.Observed (Observed)
import qualified Glean.Util.Observed as Observed
import Glean.Util.Periodic ( doPeriodically )
import Glean.Util.ShardManager
import Glean.Util.Time ( seconds )
import System.Exit (die)
import System.Time.Extra ( showDuration, sleep, Seconds )
import Util.Control.Exception ( swallow )
import Util.EventBase ( EventBaseDataplane )
import Util.Log

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
#if FACEBOOK
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
          getMostRecent = fmap (Set.fromList . map itemRepo) $ atomically $
            Catalog.list catalog [Local] $ groupF repoNameV $ do
              sortF createdV Descending
              limitF 1

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
dbUpdateNotifierThread :: Env -> Seconds -> (HashSet Repo -> IO ()) -> IO ()
dbUpdateNotifierThread Env{..} delay callback = doPeriodically (seconds 30) $ do
  initial <- updated
  go initial
  where
  go prev = do
    atomically $ do
      dbs <- list
      when (dbs == prev) retry
    vlog 1 $ "DB update detected, waiting " <> showDuration delay
    sleep delay -- wait 1s so we can batch updates
    current <- updated
    go current

  list = HashSet.fromList . map Catalog.itemRepo
    <$> Catalog.list envCatalog [Catalog.Local] Catalog.queryableF

  updated = do
    dbs <- atomically list
    vlog 1 "DB update notification"
    callback dbs
    return dbs

withShardsUpdater
  :: EventBaseDataplane -> Config -> Env -> Seconds -> IO a -> IO a
withShardsUpdater evb cfg env delay action
  | cfgPublishShards cfg = do
    port <- case cfgPort cfg of
      Just port -> return port
      Nothing -> die "--publish-shards requires --port"
    currentShards <- newMVar Nothing
    key <- getShardKey evb port
    withAsync
      (dbUpdateNotifierThread env delay
        $ databasesUpdatedCallback evb key currentShards)
      $ const action
  | otherwise = action

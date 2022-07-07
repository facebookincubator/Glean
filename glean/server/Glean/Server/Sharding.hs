{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Server.Sharding (
  shardManagerConfig,
) where

import Data.Maybe
import qualified Data.Set as Set
#if FACEBOOK
import Glean.Impl.ShardManager
#endif
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Observed (Observed)
import qualified Glean.Util.Observed as Observed
import Glean.Util.ShardManager

type PortNumber = Int

shardManagerConfig ::
  -- | Application port
  Maybe PortNumber ->
  Observed ServerConfig.Config ->
  (SomeShardManager -> IO b) ->
  IO b
shardManagerConfig mbPort smCfgServerConfig callback = do
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
            , applicationPortNumber = fromMaybe 0 mbPort
            , numberOfShards = fromIntegral $
                ServerConfig.shardManagerPolicy_nshards policy
            }
      withShardManagerClient smCliArgs $ \sm -> callback $ SomeShardManager sm
#endif
    other ->
      error $ "Unsupported sharding policy: " <> show other

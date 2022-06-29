{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Sharding (
  ShardManagerConfigParams (..),
  defaultShardManagerConfig,
) where

import qualified Data.Set as Set
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Observed (Observed)
import qualified Glean.Util.Observed as Observed
import Glean.Util.ShardManager

newtype ShardManagerConfigParams = ShardManagerConfigParams
  { smCfgServerConfig :: Observed ServerConfig.Config
  }

defaultShardManagerConfig ::
  ShardManagerConfigParams ->
  (SomeShardManager -> IO b) ->
  IO b
defaultShardManagerConfig ShardManagerConfigParams {..} callback = do
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
    other ->
      error $ "Unsupported sharding policy: " <> show other

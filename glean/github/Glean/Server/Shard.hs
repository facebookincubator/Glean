-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Server.Shard
  ( ShardKey
  , getShardKey
  , updateShards
  ) where

import Util.EventBase

data ShardKey = ShardKey

getShardKey :: eventBaseDataplane -> port -> IO ShardKey
getShardKey _ _ = return ShardKey

updateShards :: eventBaseDataplane -> ShardKey -> [shard] -> IO ()
updateShards _ _ _ = return ()

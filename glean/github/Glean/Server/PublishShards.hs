{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Server.PublishShards
  ( ShardKey
  , getShardKey
  , updateShards
  ) where

data ShardKey = ShardKey

getShardKey :: eventBaseDataplane -> port -> IO ShardKey
getShardKey _ _ = return ShardKey

updateShards :: eventBaseDataplane -> ShardKey -> [shard] -> IO ()
updateShards _ _ _ = return ()

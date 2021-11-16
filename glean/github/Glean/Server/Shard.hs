{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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

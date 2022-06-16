{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ShardManager
  ( ShardManager(..)
  , SomeShardManager(..)
  , noSharding
  ) where

import qualified Glean.Types as Glean

data ShardManager shard = ShardManager
  { getAssignedShards :: IO [shard]
  , dbToShard :: Glean.Repo -> Maybe Glean.Dependencies -> shard
  }

-- | A sharding strategy with a single shard and trivial shard assignment
noSharding :: ShardManager ()
noSharding = ShardManager (pure [()]) (\_ _ -> ())

-- | An existential wrapper around a 'ShardManager'
data SomeShardManager where
  SomeShardManager :: Ord shard => ShardManager shard -> SomeShardManager

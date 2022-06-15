{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ShardManager
  ( ShardManager(..)
  , SomeShardManager(..)
  ) where

import qualified Glean.Types as Glean

data ShardManager shard = ShardManager
  { getAssignedShards :: IO [shard]
  , dbToShard :: Glean.Repo -> Maybe Glean.Dependencies -> shard
  }

-- | An existential wrapper around a 'ShardManager'
data SomeShardManager where
  SomeShardManager :: Ord shard => ShardManager shard -> SomeShardManager

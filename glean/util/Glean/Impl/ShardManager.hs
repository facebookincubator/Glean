{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Impl.ShardManager
  ( noSharding
  , module Glean.Util.ShardManager
  ) where
import Glean.Util.ShardManager

-- | A sharding strategy with a single shard and trivial shard assignment
noSharding :: ShardManager ()
noSharding = ShardManager (pure [()]) (pure $ const ())

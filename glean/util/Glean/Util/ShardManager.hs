{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ShardManager
  ( ShardManager(..)
  , SomeShardManager(..)
  , BaseOfStack(..)
  , noSharding
  ) where

import qualified Glean.Types as Glean

-- | The base db of a stack (or a single db)
newtype BaseOfStack = BaseOfStack Glean.Repo

-- | An abstraction for sharding Glean DBs across multiple servers
data ShardManager shard = ShardManager
  { getAssignedShards :: IO [shard]
    -- | Mapping from DB stacks to shard identifiers.
    --   Takes as input the base of the stack.
    --   For non stacked dbs, this is just the db itself.
  , dbToShard :: BaseOfStack -> shard   -- See note [DB to Shard]
  }

-- | A sharding strategy with a single shard and trivial shard assignment
noSharding :: ShardManager ()
noSharding = ShardManager (pure [()]) (const ())

-- | An existential wrapper around a 'ShardManager'
data SomeShardManager where
  SomeShardManager :: Ord shard => ShardManager shard -> SomeShardManager

{- Note: [DB to Shard]

  'dbToShard' only wants the base of the stack. Why?

   We want the following invariant:

     > Every db in a stack maps to the same shard.

  This is for efficiency reasons, as the Glean server needs to download all
  the dependencies of a DB in order to be able to serve it.

  A simple way to enforce this invariant is to use the base of the stack for
  computing the shard id for all the dbs in the stack.
-}

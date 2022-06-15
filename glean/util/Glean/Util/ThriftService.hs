{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ThriftService
  ( ThriftServiceOptions(..)
  , IsThriftService(..)
  , thriftService
  , DbShard
  , runThriftInefficiently
  ) where

import Data.Default
import Data.Text (Text)

import Thrift.Api
import Util.EventBase (EventBaseDataplane, withEventBaseDataplane)

import Glean.Util.Service


-- | Options for creating a Thrift service
newtype ThriftServiceOptions = ThriftServiceOptions
  { processingTimeout :: Maybe Double   -- in seconds
  } deriving (Default, Show)

-- | Used by the Glean client to request a node containing a specific db
type DbShard = Text -- should be a number, I think

-- | A Thrift service that can be called
class IsThriftService t where
  mkThriftService :: Service -> ThriftServiceOptions -> t s
  -- | Request a node with a specific Db
  thriftServiceWithDbShard :: t s -> Maybe DbShard -> t s
  runThrift :: EventBaseDataplane -> t s -> Thrift s a -> IO a
  getSelection :: EventBaseDataplane -> t s -> Int -> IO [(HostName,PortNumber)]

thriftService :: IsThriftService t => Service -> t s
thriftService svc = mkThriftService svc def

runThriftInefficiently :: IsThriftService t => t s -> Thrift s a -> IO a
runThriftInefficiently ts action =
  withEventBaseDataplane $ \evb -> runThrift evb ts action

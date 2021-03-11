module Glean.Util.ThriftService
  ( ThriftServiceOptions(..)
  , IsThriftService(..)
  , thriftService
  , Shard
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

type Shard = Text -- should be a number, I think

-- | A Thrift service that can be called
class IsThriftService t where
  mkThriftService :: Service -> ThriftServiceOptions -> t s
  thriftServiceWithShard :: t s -> Maybe Shard -> t s
  runThrift :: EventBaseDataplane -> t s -> Thrift s a -> IO a
  getSelection :: EventBaseDataplane -> t s -> Int -> IO [(HostName,PortNumber)]

thriftService :: IsThriftService t => Service -> t s
thriftService svc = mkThriftService svc def

runThriftInefficiently :: IsThriftService t => t s -> Thrift s a -> IO a
runThriftInefficiently ts action =
  withEventBaseDataplane $ \evb -> runThrift evb ts action

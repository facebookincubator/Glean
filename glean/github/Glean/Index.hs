module Glean.Index
  ( index
  )
  where

import qualified Glean.Types as Thrift
import qualified Glean.Index.Types as Thrift
import Control.Exception (throwIO)

type Port = Int

index
  :: IO Port
  -> a
  -> Thrift.IndexRequest
  -> IO Thrift.IndexResponse
index _ _ _ = throwIO $ Thrift.Exception "not implemented"

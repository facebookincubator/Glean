-- |
-- This layer provides synchronous writing on top of the lower-level
-- asynchronous write API, by doing polling and waiting as necessary.
--
module Glean.Write.SendBatch
  ( sendBatch
  , sendBatchAsync
  , sendJsonBatch
  , waitBatch
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Default
import qualified Data.Text as Text

import Glean.Backend.Remote
import qualified Glean.Types as Thrift


sendBatch
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.Batch
  -> IO Thrift.Subst
sendBatch backend repo batch = do
  handle <- sendBatchAsync backend repo batch
  waitBatch backend handle

sendBatchAsync
  :: Backend be => be -> Thrift.Repo -> Thrift.Batch -> IO Thrift.Handle
sendBatchAsync backend repo batch = do
  r <- enqueueBatch backend $ Thrift.ComputedBatch repo True batch
  case r of
    Thrift.SendResponse_handle h -> return h
    Thrift.SendResponse_retry (Thrift.BatchRetry r) ->
      retry r $ sendBatchAsync backend repo batch

sendJsonBatch
  :: Backend be
  => be
  -> Thrift.Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> IO Thrift.Subst
sendJsonBatch backend repo batches opts = do
  handle <- send
  -- Cope with talking to an old server where this API was synchronous
  -- and didn't return a handle.
  if Text.null handle
    then return def
    else waitBatch backend handle
  where
    send = do
      r <- try $ enqueueJsonBatch backend repo
        Thrift.SendJsonBatch
          { Thrift.sendJsonBatch_batches = batches
          , Thrift.sendJsonBatch_options = opts
          , Thrift.sendJsonBatch_remember = True }
      case r of
        Right Thrift.SendJsonBatchResponse{..} ->
          return sendJsonBatchResponse_handle
        Left Thrift.Retry{..} ->
          retry retry_seconds send

retry :: Double -> IO a -> IO a
retry secs action = do
  threadDelay $ round $ max secs 1 * 1000000
  action

waitBatch :: Backend be => be -> Thrift.Handle -> IO Thrift.Subst
waitBatch backend handle = do
  e <- try $ pollBatch backend handle
  case e of
    Left Thrift.Retry{..} -> retry retry_seconds $ waitBatch backend handle
    Right r -> case r of
      Thrift.FinishResponse_subst subst -> return subst
      Thrift.FinishResponse_retry (Thrift.BatchRetry r) ->
        retry r $ waitBatch backend handle

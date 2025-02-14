{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- |
-- This layer provides synchronous writing on top of the lower-level
-- asynchronous write API, by doing polling and waiting as necessary.
--
module Glean.Write.SendBatch
  ( sendBatch
  , sendBatchAsync
  , sendJsonBatch
  , sendJsonBatchAsync
  , sendBatchDescriptor
  , sendBatchDescriptorAsync
  , waitBatch
  ) where

import Control.Exception
import Data.Default
import qualified Data.Text as Text
import System.Time.Extra

import Glean.Backend.Types
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
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.Batch
  -> IO Thrift.Handle
sendBatchAsync backend repo batch = do
  r <- enqueueBatch backend $ Thrift.ComputedBatch repo True batch
  case r of
    Thrift.SendResponse_handle h -> return h
    Thrift.SendResponse_retry (Thrift.BatchRetry r) ->
      retry r $ sendBatchAsync backend repo batch

sendJsonBatchAsync
  :: Backend be
  => be
  -> Thrift.Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> IO Thrift.Handle
sendJsonBatchAsync backend repo batches opts = do
  r <- try $ enqueueJsonBatch backend repo
    Thrift.SendJsonBatch
      { Thrift.sendJsonBatch_batches = batches
      , Thrift.sendJsonBatch_options = opts
      , Thrift.sendJsonBatch_remember = True }
  case r of
    Right Thrift.SendJsonBatchResponse{..} ->
      return sendJsonBatchResponse_handle
    Left Thrift.Retry{..} ->
      retry retry_seconds $
        sendJsonBatchAsync backend repo batches opts

sendJsonBatch
  :: Backend be
  => be
  -> Thrift.Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> IO Thrift.Subst
sendJsonBatch backend repo batches opts = do
  handle <- sendJsonBatchAsync backend repo batches opts
  -- Cope with talking to an old server where this API was synchronous
  -- and didn't return a handle.
  if Text.null handle
    then return def
    else waitBatch backend handle

sendBatchDescriptor
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.BatchDescriptor
  -> IO Thrift.Subst
sendBatchDescriptor backend repo descriptor = do
  handle <- sendBatchDescriptorAsync backend repo descriptor
  if Text.null handle
    then return def
    else waitBatch backend handle

sendBatchDescriptorAsync
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.BatchDescriptor
  -> IO Thrift.Handle
sendBatchDescriptorAsync backend repo descriptor = do
  let batch = Thrift.EnqueueBatch_descriptor descriptor
  r <- try $ enqueueBatchDescriptor backend repo batch
    Thrift.EnqueueBatchWaitPolicy_Remember
  case r of
    Right Thrift.EnqueueBatchResponse{..} ->
      return enqueueBatchResponse_handle
    Left Thrift.Retry{..} ->
      retry retry_seconds $
        sendBatchDescriptorAsync backend repo descriptor

retry :: Double -> IO a -> IO a
retry secs action = do
  sleep $ max secs 1
  action

waitBatch
  :: Backend be
  => be
  -> Thrift.Handle
  -> IO Thrift.Subst
waitBatch backend handle = do
  e <- try $ pollBatch backend handle
  case e of
    Left Thrift.Retry{..} ->
      retry retry_seconds $ waitBatch backend handle
    Right r -> case r of
      Thrift.FinishResponse_subst subst -> return subst
      Thrift.FinishResponse_retry (Thrift.BatchRetry r) ->
        retry r $ waitBatch backend handle

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
  ( sendBatchAndWait
  , sendBatch
  , sendJsonBatchAndWait
  , sendJsonBatch
  , sendBatchDescriptorAndWait
  , sendBatchDescriptor
  , waitBatch
  ) where

import Control.Exception
import Data.Default
import qualified Data.Text as Text
import System.Time.Extra

import Glean.Backend.Types
import qualified Glean.Types as Thrift

sendBatchAndWait
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.Batch
  -> IO Thrift.Subst
sendBatchAndWait backend repo batch = do
  handle <- sendBatch backend repo batch True
  waitBatch backend handle

sendBatch
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.Batch
  -> Bool
  -> IO Thrift.Handle
sendBatch backend repo batch remember = do
  r <- enqueueBatch backend $ Thrift.ComputedBatch repo remember batch
  case r of
    Thrift.SendResponse_handle h -> return h
    Thrift.SendResponse_retry (Thrift.BatchRetry r) ->
      retry r $ sendBatch backend repo batch remember

sendJsonBatchAndWait
  :: Backend be
  => be
  -> Thrift.Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> IO Thrift.Subst
sendJsonBatchAndWait backend repo batches opts = do
  handle <- sendJsonBatch backend repo batches opts True
  -- Cope with talking to an old server where this API was synchronous
  -- and didn't return a handle.
  if Text.null handle
    then return def
    else waitBatch backend handle

sendJsonBatch
  :: Backend be
  => be
  -> Thrift.Repo
  -> [Thrift.JsonFactBatch]
  -> Maybe Thrift.SendJsonBatchOptions
  -> Bool
  -> IO Thrift.Handle
sendJsonBatch backend repo batches opts remember = do
  r <- try $ enqueueJsonBatch backend repo
    Thrift.SendJsonBatch
      { Thrift.sendJsonBatch_batches = batches
      , Thrift.sendJsonBatch_options = opts
      , Thrift.sendJsonBatch_remember = remember }
  case r of
    Right Thrift.SendJsonBatchResponse{..} ->
      return sendJsonBatchResponse_handle
    Left Thrift.Retry{..} ->
      retry retry_seconds $
        sendJsonBatch backend repo batches opts remember

sendBatchDescriptorAndWait
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.BatchDescriptor
  -> IO Thrift.Subst
sendBatchDescriptorAndWait backend repo descriptor = do
  handle <- sendBatchDescriptor backend repo descriptor True
  if Text.null handle
    then return def
    else waitBatch backend handle

sendBatchDescriptor
  :: Backend be
  => be
  -> Thrift.Repo
  -> Thrift.BatchDescriptor
  -> Bool
  -> IO Thrift.Handle
sendBatchDescriptor backend repo descriptor remember = do
  let batch = Thrift.EnqueueBatch_descriptor descriptor
      policy = if remember
        then Thrift.EnqueueBatchWaitPolicy_Remember
        else Thrift.EnqueueBatchWaitPolicy_None
  r <- try $ enqueueBatchDescriptor backend repo batch policy
  case r of
    Right Thrift.EnqueueBatchResponse{..} ->
      return enqueueBatchResponse_handle
    Left Thrift.Retry{..} ->
      retry retry_seconds $
        sendBatchDescriptor backend repo descriptor remember

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

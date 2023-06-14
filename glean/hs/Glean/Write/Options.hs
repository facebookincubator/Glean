{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, NamedFieldPuns #-}

module Glean.Write.Options
  ( sendQueueOptions
  , sendAndRebaseQueueOptions
  , writerOptions
  ) where

import Control.Exception
import qualified Data.ByteString as B
import Data.Default (def)
import qualified Options.Applicative as O
import System.IO
import Text.Printf

import Thrift.Protocol.Compact
import Util.Log

import Glean.Write.Async (WriterSettings(..), WriterEvent(..))
import Glean.Write.SendQueue (SendQueueSettings(..))
import Glean.Write.SendAndRebaseQueue (SendAndRebaseQueueSettings(..))
import Glean.Util.RetryChannelException


sendQueueOptions :: O.Parser SendQueueSettings
sendQueueOptions = do
  sendQueueThreads <- O.option O.auto $
    O.long "sender-threads"
    <> O.metavar "N"
    <> O.value 1
    <> O.showDefault
    <> O.help "number of concurrent sender threads, default 1"
  sendQueueMaxMemory <- O.option O.auto $
    O.long "max-send-queue-size"
    <> O.metavar "N"
    <> O.value 2000000000
    <> O.showDefault
    <> O.help "maximum size of send queue (in bytes)"
  sendQueueMaxBatches <- O.option O.auto $
    O.long "max-send-queue-batches"
    <> O.metavar "N"
    <> O.value 1024
    <> O.showDefault
    <> O.help "maximum number of batches in send queue"
  sendQueueRetry <- fmap toRetryPolicy $ O.option O.auto $
    O.long "send-retries"
    <> O.metavar "N"
    <> O.value 20
    <> O.showDefault
    <> O.help "number of retries, default 20"
  return def
    { sendQueueThreads
    , sendQueueMaxMemory
    , sendQueueMaxBatches
    , sendQueueRetry
    }
  where
  toRetryPolicy n = defaultRetryPolicy { maxRetries = n }


sendAndRebaseQueueOptions :: O.Parser SendAndRebaseQueueSettings
sendAndRebaseQueueOptions = do
  sendAndRebaseQueueSendQueueSettings <- sendQueueOptions
  sendAndRebaseQueueFactCacheSize <- O.option O.auto $
    O.long "fact-cache-size"
    <> O.metavar "N"
    <> O.value 2000000000
    <> O.showDefault
    <> O.help "size of the cache for sent facts"
  sendAndRebaseQueueFactBufferSize <- O.option O.auto $
    O.long "fact-buffer-size"
    <> O.metavar "N"
    <> O.value (200*1000*1000)
    <> O.showDefault
    <> O.help "size of the buffer for unsent facts per sender"
  return SendAndRebaseQueueSettings
    { sendAndRebaseQueueSendQueueSettings
    , sendAndRebaseQueueFactCacheSize
    , sendAndRebaseQueueFactBufferSize
    , sendAndRebaseQueueSenders =
        sendQueueThreads sendAndRebaseQueueSendQueueSettings
    , sendAndRebaseQueueAllowRemoteReferences = False
    , sendAndRebaseQueueStats = Nothing
    }


writerOptions :: O.Parser WriterSettings
writerOptions = do
  writerMaxSize <- O.option O.auto $
    O.long "max-batch-size"
    <> O.metavar "N"
    <> O.value 30000000
    <> O.showDefault
    <> O.help "maximum size of Thrift batch (in bytes)"
  writerDump <- O.optional $ O.strOption $
    O.long "dump-batches"
    <> O.metavar "DIR"
    <> O.help ("also dump all batches in binary format to DIR. " <>
      "The files can be used with 'glean write --file-format=binary'")
  return def
    { writerMaxSize = writerMaxSize
    , writerLog = logBatch writerDump
    }
  where
    logBatch dump (WriterPushing batch)
      | Just dir <- dump = do
        let serialized = serializeCompact batch
        bracket
          (openTempFile dir "batch.bin")
          (hClose . snd)
          $ \(f,h) -> do
            logInfo $ printf "dumping batch (%d bytes) to %s"
              (B.length serialized) f
            B.hPut h serialized
    logBatch _ _ = return ()

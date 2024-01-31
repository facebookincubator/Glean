{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | This is in a separate module from "Glean.Write.Async" in order
-- so that depending on "//glean/schema/hs:hs" does not create a cycle.
--
-- This creates simpler uses of 'withSender' and 'withWriter'
module Glean.Write.SimpleAsync
  ( Sender, SendQueueSettings(..)
  , Writer, WriterSettings(..)
  , withSimpleSender
  , withSimpleWriter, withSimpleWriters
  , withTestWriter
  ) where

import qualified Data.ByteString as BS
import Data.Default
import Util.Log (logError, logInfo)

import Glean
import Glean.Util.Time ( toDiffMillis )
import Glean.Write.Async
import Glean.Types

-- | 'withSimpleSender' has 'senderLog' write to "Util.Log"
--
-- One sender is used for all writers.
withSimpleSender
  :: (Backend be)
  => be
  -> Repo
  -> [SchemaPredicates]
  -> SendQueueSettings
  -> (Sender -> IO a)
  -> IO a
withSimpleSender be repo predicates settings =
    withSender
      be
      repo
      predicates
      settings
        { sendQueueLog = \event -> do
            case event of
              SendQueueSending{} -> return ()
              SendQueueSendingJson{} -> return ()
              SendQueueSent size time -> logInfo $ concat
                [ "sent batch ", show size, " in "
                , show (toDiffMillis time), "ms" ]
              SendQueueFinished -> logInfo "all batches sent"
              SendQueueFailed exc -> logError $ "sending failed: " ++ show exc
            sendQueueLog settings event
        }

-- | 'withSimpleWriter' has 'writerLog' output to "Util.Log"
--
-- NOTE: Writers serialise all writes - if you want to produce facts in
-- parallel, use multiple writers (see 'withSimpleWriters' below)
withSimpleWriter
  :: Sender
  -> WriterSettings
  -> (Writer -> IO a)
  -> IO a
withSimpleWriter sender settings =
  withWriter sender (withLogging settings)

-- | Multiple Writer version of 'withSimpleWriter'
withSimpleWriters
  :: Sender
  -> WriterSettings
  -> Int
  -> ([Writer] -> IO a)
  -> IO a
withSimpleWriters sender settings n =
  withWriters sender (withLogging settings) n

withLogging :: WriterSettings -> WriterSettings
withLogging settings =
  settings
    { writerLog = \event -> do
        case event of
          WriterPushing batch -> logInfo $
            "pushing batch " ++ show (BS.length $ batch_facts batch)
          WriterStalling -> logInfo "writing stalled"
          WriterResuming time -> logInfo $
            "writing caught up in " ++ show (toDiffMillis time) ++ "ms"
        writerLog settings event
    }

withTestWriter
  :: (Backend be)
  => be
  -> Repo
  -> [SchemaPredicates]
  -> (Writer -> IO a)
  -> IO a
withTestWriter be repo predicates act =
  withSimpleSender be repo predicates def $ \sender ->
  withSimpleWriter sender def act

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Write.SendQueue
  ( SendQueueSettings(..)
  , SendQueueEvent(..)
  , SendQueue
  , Callback
  , withSendQueue
  , writeSendQueue
  , writeSendQueueJson
  , writeSendQueueDescriptor
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Default
import System.Time.Extra (sleep)
import Text.Printf (printf)

import Util.Control.Exception (tryBracket)
import Util.Log.String
import Util.STM
import Util.Time

import Glean.Backend.Types (Backend)
import Glean.Backend.Retry
import qualified Glean.Backend.Types as Backend
import qualified Glean.Types as Thrift
import Glean.Write.SendBatch
import Glean.Util.RetryChannelException


-- | An event that happens in a 'SendQueue' and can be logged
data SendQueueEvent
  = -- | Started sending a batch
    SendQueueSending Thrift.Batch

    -- | Started sending a JSON batch
  | SendQueueSendingJson [Thrift.JsonFactBatch]

    -- | Started sending a batch descriptor
  | SendQueueSendingDescriptor Thrift.BatchDescriptor

    -- | Finished sending a batch
  | SendQueueSent
      Int -- batch size
      DiffTimePoints -- time taken

    -- | Finished sending all batches and shutting down
  | SendQueueFinished

    -- | Sending failed
  | SendQueueFailed SomeException


data QueueStatus
  = Open
  | Closed
  | Failed SomeException

-- | When the send operation fails or is completed this is called once.
type Callback = Either SomeException Thrift.Subst -> STM ()

-- | The GleanService can return a Handle for an async operation, and the
-- client can later poll to determine the actual result/retry/error.
data Wait = Wait
  { waitHandle :: !Thrift.Handle  -- ^ handle from GleanService
  , waitStart :: !TimePoint  -- ^ When this 'Wait' was constructed
  , waitCallback :: !Callback  -- ^ use-once callback with result.
  , waitOriginalBatch :: !Batch  -- ^ the batch, so we can resend it
  }

data Batch
  = BinaryBatch Thrift.Batch
  | JsonBatch !Int {- size -} [Thrift.JsonFactBatch]
  | BatchDescriptor Thrift.BatchDescriptor

batchSize :: Batch -> Int
batchSize (BinaryBatch bin) = BS.length $ Thrift.batch_facts bin
batchSize (JsonBatch sz _) = sz
batchSize (BatchDescriptor _) = 0

data SendQueue = SendQueue
  { -- | Batches we haven't sent yet
    sqOutQueue :: TQueue (Batch, Callback)

    -- | Batches we've sent and are now waiting for the server to commit
  , sqWaitQueue :: TQueue Wait

    -- | Memory used by batches in sqOutQueue and sqWaitQueue
  , sqMemory :: TVar Int

    -- | Number of batches in flight (in sqOutQueue and sqWaitQueue)
  , sqCount :: TVar Int

    -- | Limit on sqMemory
  , sqMaxMemory :: Int

    -- | Limit on sqCount
  , sqMaxCount :: Int

    -- | Queue status
  , sqStatus :: TVar QueueStatus
  }

data WriteQueueClosed = WriteQueueClosed deriving(Show)
instance Exception WriteQueueClosed

newSendQueue :: Int -> Int -> IO SendQueue
newSendQueue max_mem max_count = SendQueue
  <$> newTQueueIO
  <*> newTQueueIO
  <*> newTVarIO 0
  <*> newTVarIO 0
  <*> pure max_mem
  <*> pure max_count
  <*> newTVarIO Open

releaseBatch :: SendQueue -> Int -> STM ()
releaseBatch sq size = do
  modifyTVar' (sqCount sq) $ \n -> n - 1
  modifyTVar' (sqMemory sq) $ \n -> n - size

acquireBatch :: SendQueue -> Int -> STM ()
acquireBatch sq size = do
  mem <- readTVar $ sqMemory sq
  count <- readTVar $ sqCount sq
  when (mem > sqMaxMemory sq || count > sqMaxCount sq) retry
  writeTVar (sqMemory sq) $! mem + size
  writeTVar (sqCount sq) $! count + 1

writeSendQueue :: SendQueue -> Thrift.Batch -> Callback -> STM ()
writeSendQueue sq batch callback =
  writeSendQueue_ sq (BinaryBatch batch) callback

writeSendQueueJson :: SendQueue -> [Thrift.JsonFactBatch] -> Callback -> STM ()
writeSendQueueJson sq json callback =
  writeSendQueue_ sq (JsonBatch size json) callback
  where
  -- size is approximate for JSON
  size = sum (map jsonFactBatchSize json)
  jsonFactBatchSize Thrift.JsonFactBatch{..} =
    sum (map BS.length jsonFactBatch_facts) +
    maybe 0 BS.length jsonFactBatch_unit

writeSendQueueDescriptor
  :: SendQueue
  -> Thrift.BatchDescriptor
  -> Callback
  -> STM ()
writeSendQueueDescriptor sq descriptor callback =
  writeSendQueue_ sq (BatchDescriptor descriptor) callback

writeSendQueue_ :: SendQueue -> Batch -> Callback -> STM ()
writeSendQueue_ sq batch callback = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> return ()
    Closed -> throwSTM WriteQueueClosed
    Failed exc -> throwSTM exc
  let !size = batchSize batch
  acquireBatch sq size
  writeTQueue (sqOutQueue sq) (batch, callback)

closeSendQueue :: SendQueue -> STM ()
closeSendQueue sq = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> writeTVar (sqStatus sq) Closed
    Closed -> return ()
    Failed exc -> throwSTM exc

failSendQueue :: SendQueue -> SomeException -> STM ()
failSendQueue sq exc = do
  writeTVar (sqStatus sq) $ Failed exc
  xs <- map waitCallback <$> flushTQueue (sqWaitQueue sq)
  ys <- map snd <$> flushTQueue (sqOutQueue sq)
  mapM_ ($ Left exc) (xs ++ ys)

readSendQueue :: SendQueue -> STM (Maybe (Batch, Callback))
readSendQueue sq = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> get
    Closed -> do
      -- Don't exit the sendFromQueue until all batches are safely
      -- written. We might have to resend batches if the server
      -- forgets them.
      count <- readTVar $ sqCount sq
      if count /= 0 then get else return Nothing
    Failed exc -> throwSTM exc
  where
    get = do
      (batch, callback) <- readTQueue $ sqOutQueue sq
      return $ Just (batch, callback)

readWaitQueue :: SendQueue -> STM (Maybe Wait)
readWaitQueue sq = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> get
    Closed -> do
      -- NOTE: There can still be outstanding batches even if both queues are
      -- empty - sendFromQueue might be writing one.
      count <- readTVar $ sqCount sq
      if count /= 0 then get else return Nothing
    Failed exc -> throwSTM exc
  where
    get = Just <$> readTQueue (sqWaitQueue sq)

pollFromWaitQueue
  :: Backend.Backend e => e -> SendQueueSettings -> SendQueue -> IO ()
pollFromWaitQueue backend settings sq = do
  cont <- tryBracket
    (atomically $ readWaitQueue sq)
    (\r ex -> case (ex,r) of
      (Left exc, Just wait) -> do
        logWarning $ "Thread killed: " <> show exc
        atomically $ do
          waitCallback wait $ Left exc
          failSendQueue sq exc
        sendQueueLog settings $ SendQueueFailed exc
      _ -> return ())
    $ \r ->
    case r of
      Just Wait{..} -> do
        result <- try $ waitBatch backend waitHandle
        let !size = batchSize waitOriginalBatch
        case result of
          Left e@Thrift.UnknownBatchHandle{} -> do
            logWarning $ "Server forgot batch; resending (" <> show e <> ")"
            atomically $ do
              -- don't writeSendQueue; retries should work even if the
              -- queue is in the Closed state and shutting down
              writeTQueue (sqOutQueue sq) (waitOriginalBatch, waitCallback)
              return True
          Right subst -> do
            atomically $ do
              waitCallback $ Right subst
              releaseBatch sq size
            elapsed <- getElapsedTime waitStart
            sendQueueLog settings $ SendQueueSent size elapsed
            return True

      Nothing -> return False

  when cont $ pollFromWaitQueue backend settings sq

sendFromQueue
  :: Backend.Backend e
  => e
  -> Thrift.Repo
  -> SendQueueSettings
  -> SendQueue
  -> IO ()
sendFromQueue backend repo settings sq = do
  cont <- tryBracket
    (atomically $ readSendQueue sq)
    (\r ex -> case (ex,r) of
      (Left exc, Just (_, callback)) -> do
        logWarning $ "Thread killed: " <> show exc
        atomically $ do
          callback $ Left exc
          failSendQueue sq exc
        sendQueueLog settings $ SendQueueFailed exc
      _ -> return ())
    $ \r ->
    case r of
      Just (batch, callback) -> do
        sendQueueLog settings $ case batch of
          BinaryBatch bin -> SendQueueSending bin
          JsonBatch _ json -> SendQueueSendingJson json
          BatchDescriptor descriptor -> SendQueueSendingDescriptor descriptor
        start <- getTimePoint
        handle <- case batch of
          BinaryBatch bin -> sendBatchAsync backend repo bin
          JsonBatch _ json -> sendJsonBatchAsync backend repo json Nothing
          BatchDescriptor descriptor
            -> sendBatchDescriptorAsync backend repo descriptor
        atomically $ writeTQueue (sqWaitQueue sq) Wait
          { waitHandle = handle
          , waitStart = start
          , waitCallback = callback
          , waitOriginalBatch = batch
          }
        return True

      Nothing -> return False
  when cont $ sendFromQueue backend repo settings sq

-- | Settings for a 'SendQueue'
data SendQueueSettings = SendQueueSettings
  { -- | Max memory that the send queue should occupy (soft limit)
    sendQueueMaxMemory :: !Int

    -- | Max number of batches that the queue should hold
  , sendQueueMaxBatches :: !Int

    -- | Number of threads to use for sending
  , sendQueueThreads :: !Int

    -- | A function called whenever a 'SendQueueEvent' occurs
  , sendQueueLog :: SendQueueEvent -> IO ()

    -- | How to retry failures
  , sendQueueRetry :: RetryPolicy

    -- | Optionally log stats every 10s
  , sendQueueLogStats :: Bool
  }

instance Show SendQueueSettings where
  show _ = "SendQueueSettings{}"

instance Default SendQueueSettings where
  def = SendQueueSettings
    { sendQueueMaxMemory = 1000000000
    , sendQueueMaxBatches = 10000
    , sendQueueThreads = 1
    , sendQueueLog = const $ return ()
    , sendQueueRetry = defaultRetryPolicy
    , sendQueueLogStats = False
    }

withSendQueue
  :: Backend e
  => e
  -> Thrift.Repo
  -> SendQueueSettings
  -> (SendQueue -> IO a)
  -> IO a
withSendQueue backend repo settings@SendQueueSettings{..} action =
  mask $ \restore -> do
    q <- newSendQueue sendQueueMaxMemory sendQueueMaxBatches
    let retryBackend = backendRetryWrites backend sendQueueRetry
    withStats sendQueueLogStats q $ do
      snd <$> Async.concurrently
        (Async.mapConcurrently_ id
          $ restore (pollFromWaitQueue retryBackend settings q)
          : replicate
              sendQueueThreads
              (restore (sendFromQueue retryBackend repo settings q)))
        (restore (action q)
          `finally` atomically (closeSendQueue q))

withStats :: Bool -> SendQueue -> IO a -> IO a
withStats on SendQueue{..} act = do
  verbose <- vlogIsOn 1
  if on || verbose
    then Async.withAsync (forever $ logQueueStats >> sleep 10) $ \_ -> act
    else act
  where
  logQueueStats = do
    count <- readTVarIO sqCount
    memory <- readTVarIO sqMemory
    logInfo $ printf "count:%d/%d memory:%d/%d"
      count sqMaxCount memory sqMaxMemory

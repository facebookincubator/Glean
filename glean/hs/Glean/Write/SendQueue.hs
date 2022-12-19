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
  ) where

import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Default

import Util.Control.Exception (tryBracket)
import Util.STM

import Glean.Backend.Types (Backend)
import qualified Glean.Backend.Types as Backend
import qualified Glean.Types as Thrift
import Glean.Write.SendBatch
import Glean.Util.Time


-- | An event that happens in a 'SendQueue' and can be logged
data SendQueueEvent
  = -- | Started sending a batch
    SendQueueSending Thrift.Batch

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
  , waitSize :: !Int  -- ^ length of Thrift.batch_facts
  , waitCallback :: !Callback  -- ^ use-once callback with result.
  }

data SendQueue = SendQueue
  { -- | Batches we haven't sent yet
    sqOutQueue :: TQueue (Thrift.Batch, Callback)

    -- | Batches we've sent and are now waiting for the server to commit
  , sqWaitQueue :: TQueue Wait

    -- | Memory used by batches in sqOutQueue
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

writeSendQueue :: SendQueue -> Thrift.Batch -> Callback -> STM ()
writeSendQueue sq batch callback = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> return ()
    Closed -> throwSTM WriteQueueClosed
    Failed exc -> throwSTM exc
  let !size = BS.length $ Thrift.batch_facts batch
  mem <- readTVar $ sqMemory sq
  count <- readTVar $ sqCount sq
  when (mem > sqMaxMemory sq || count > sqMaxCount sq) retry
  writeTQueue (sqOutQueue sq) (batch, callback)
  writeTVar (sqMemory sq) $! mem + size
  writeTVar (sqCount sq) $! count + 1

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

readSendQueue :: SendQueue -> STM (Maybe (Thrift.Batch, Callback))
readSendQueue sq = do
  status <- readTVar $ sqStatus sq
  case status of
    Open -> get
    Closed -> do
      empty <- isEmptyTQueue $ sqOutQueue sq
      if not empty then get else return Nothing
    Failed exc -> throwSTM exc
  where
    get = do
      (batch, callback) <- readTQueue $ sqOutQueue sq
      let size = BS.length $ Thrift.batch_facts batch
      modifyTVar' (sqMemory sq) $ \n -> n - size
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
        atomically $ do
          waitCallback wait $ Left exc
          failSendQueue sq exc
        sendQueueLog settings $ SendQueueFailed exc
      _ -> return ())
    $ \r ->
    case r of
      Just wait -> do
        subst <- waitBatch backend $ waitHandle wait
        atomically $ do
          waitCallback wait $ Right subst
          modifyTVar' (sqCount sq) $ \n -> n - 1
        elapsed <- getElapsedTime $ waitStart wait
        sendQueueLog settings $ SendQueueSent (waitSize wait) elapsed
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
        atomically $ do
          callback $ Left exc
          failSendQueue sq exc
        sendQueueLog settings $ SendQueueFailed exc
      _ -> return ())
    $ \r ->
    case r of
      Just (batch, callback) -> do
        sendQueueLog settings $ SendQueueSending batch
        let !n = BS.length $ Thrift.batch_facts batch
        start <- getTimePoint
        handle <- sendBatchAsync backend repo batch
        atomically $ writeTQueue (sqWaitQueue sq) Wait
          { waitHandle = handle
          , waitStart = start
          , waitSize = n
          , waitCallback = callback
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
  }

instance Show SendQueueSettings where
  show _ = "SendQueueSettings{}"

instance Default SendQueueSettings where
  def = SendQueueSettings
    { sendQueueMaxMemory = 1000000000
    , sendQueueMaxBatches = 10000
    , sendQueueThreads = 1
    , sendQueueLog = const $ return ()
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
    snd <$> Async.concurrently
      (Async.mapConcurrently_ id
        $ restore (pollFromWaitQueue backend settings q)
        : replicate
            sendQueueThreads
            (restore (sendFromQueue backend repo settings q)))
      (restore (action q)
        `finally` atomically (closeSendQueue q))

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Write.Queue
  (
    enqueueWrite,
    addWriteJobToQueue,
    reportQueueSizes,
    downloadBatchFromLocation,
    writeContentFromBatch
  ) where

import Control.Concurrent (MVar, newEmptyMVar)
import Control.Exception (SomeException, throwIO)
import Control.Monad (forM_, when)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import Text.Printf (printf)
import System.Clock (TimeSpec, toNanoSecs)

import ServiceData.GlobalStats (addStatValueType)
import ServiceData.Types (ExportType(..))
import Util.Defer (immediately, now, later)
import Util.STM

import Glean.Database.BatchLocation as BatchLocation
import Glean.Database.Exception (dbError)
import Glean.Database.Types
import Glean.Database.Schema.Types
import Glean.Types as Thrift hiding (Database)
import qualified Glean.RTS.Foreign.Subst as Subst
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Observed as Observed
import Glean.Util.Metric (beginTick)

-- | Add a write job to the queue, or throw 'Retry' if no memory available
enqueueWrite
  :: Env
  -> Repo
  -> WriteQueue
  -> DbSchema
  -> Int
  -> Maybe SchemaId
  -> Bool
  -> Bool
  -> IO WriteContent
  -> IO (MVar (Either SomeException Subst.Subst))
enqueueWrite env@Env{..} repo queue@WriteQueue{..} odbSchema size
    optSchemaId checkQueueSize remember writeContent = do
  start <- beginTick 1
  config <- Observed.get envServerConfig
  mvar <- newEmptyMVar

  -- check the schema ID in the batch matches the DB
  case optSchemaId of
    Just schemaId | ServerConfig.config_check_write_schema_id config ->
      when (not (Map.member schemaId (schemaEnvs odbSchema))) $
        dbError repo $ printf
          "schema ID in batch (%s) does not match schema ID of DB (%s)"
          (unSchemaId schemaId)
          (show (Map.keys (schemaEnvs odbSchema)))
    _ -> return ()

  let WriteQueues{..} = envWriteQueues
      enqueueIt = do
        pending <- now $ readTVar writeQueuesSize
        let !newSize = pending + size
        now $ do
          addWriteJobToQueue
            repo
            queue
            envWriteQueues
            WriteJob
              { writeSize = size
              , writeContentIO = writeContent
              , writeDone = mvar
              , writeStart = start
              , writeFailureIrrecoverable = not remember }
        queueCount <- now $ updateTVar writeQueueCount (+1)
        queueSize <- now $ updateTVar writeQueueSize (+ size)
        later $ do
          addStatValueType "glean.db.write.enqueued" (size `div` k) Sum
          reportQueueSizes repo queueCount queueSize newSize Nothing
  immediately $ do
    check <- if checkQueueSize
      then now $ checkMemoryAvailable env config size
      else return True
    if check then enqueueIt else do
      latency <- now $ readTVar writeQueueLatency
      let elapsed = fromIntegral (toNanoSecs latency) / 1000000000.0
      later $ rejectWrite repo size elapsed
  return mvar

addWriteJobToQueue
  :: Repo
  -> WriteQueue
  -> WriteQueues
  -> WriteJob
  -> STM ()
addWriteJobToQueue repo queue@WriteQueue{..} WriteQueues{..} job = do
  wasEmpty <- isEmptyTQueue writeQueue
  writeTQueue writeQueue job
  -- if this repo previously had no writes, add it to the round-robin
  when wasEmpty $ writeTQueue writeQueues (repo, queue)

-- | Update stats and throw a Retry exception
rejectWrite :: Repo -> Int -> Double -> IO retry
rejectWrite repo size elapsed = do
  let clamped = min 1000.0 $ max 1.0 elapsed
      repoB = Text.encodeUtf8 (repo_name repo)
  addStatValueType "glean.db.write.rejected" (size `div` k) Sum
  addStatValueType ("glean.db.write.retry_ms." <> repoB)
    (round (elapsed*1000)) Avg
  throwIO (Retry clamped)

-- | Check and update the in-memory write queue size
checkMemoryAvailable
  :: Env
  -> ServerConfig.Config
  -> Int -- ^ requested size
  -> STM Bool
checkMemoryAvailable Env{..} ServerConfig.Config{..} size = do
  let WriteQueues{..} = envWriteQueues
  pending <- readTVar writeQueuesSize
  let !newSize = pending + size
  if roundUp mb newSize <= fromIntegral config_db_write_queue_limit_mb
    then do
      writeTVar writeQueuesSize newSize
      return True
    else return False

reportQueueSizes :: Repo -> Int -> Int -> Int -> Maybe TimeSpec -> IO ()
reportQueueSizes repo repoQueueCount repoQueueSize totalQueueSize mLatency = do
  let repoB = Text.encodeUtf8 (repo_name repo)
  addStatValueType ("glean.db.write.queue_count." <> repoB) repoQueueCount Avg
  addStatValueType ("glean.db.write.queue." <> repoB) (repoQueueSize `div` k)
    Avg
  addStatValueType "glean.db.write.queue" (totalQueueSize `div` k) Avg
  forM_ mLatency $ \ latency -> do
    let elapsedMilliSeconds = fromInteger (toNanoSecs latency `div` 1000000)
    when (elapsedMilliSeconds > 0) $
      addStatValueType ("glean.db.write.queue_ms." <> repoB)
        elapsedMilliSeconds Avg

writeContentFromBatch :: Thrift.Batch -> WriteContent
writeContentFromBatch writeBatch = WriteContent
  { writeBatch = writeBatch
  , writeOwnership = Nothing
  }

downloadBatchFromLocation
  :: Env
  -> Thrift.BatchDescriptor
  -> IO WriteContent
downloadBatchFromLocation Env{..} batchDescriptor =
  let
    batchFormat = Thrift.batchDescriptor_format batchDescriptor
    locationString = Thrift.batchDescriptor_location batchDescriptor
  in
    writeContentFromBatch <$> BatchLocation.fromString
      envBatchLocationParser locationString batchFormat

k :: Int
k = 1024

mb :: Int
mb = 1024*1024

roundUp :: Int -> Int -> Int
roundUp unit x = (x + unit-1) `div` unit

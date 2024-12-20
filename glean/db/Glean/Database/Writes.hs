{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Server-side write queue implementation.  Currently this is just a
-- layer that makes every write operation asynchronous with support
-- for polling the status of a previous write.
--
-- Rationale: why are writes async?
--
-- The alternative is to put writes into a separate Thrift queue with
-- a lower priority, to avoid writes from blocking other
-- requests. This would work, but under heavy write load we would
-- still have a lot of concurrent open connections to the server,
-- which is not ideal.
--
-- Longer term we might want to completely separate the write queue,
-- e.g. using Scribe, so making the write API async is a step in the
-- right direction.
--
-- ToDo:
--   - make it a bounded queue
--   - add logging for the size of the queue etc.
--   - decode JSON eagerly but do writes async, so that we can
--     parallelise the JSON writing. Might run into issues with the
--     decoded JSON taking more space and adding GC overhead.

module Glean.Database.Writes
  ( enqueueBatch
  , enqueueJsonBatch
  , enqueueJsonBatchByteString
  , enqueueCheckpoint
  , pollBatch
  , reapWrites
  , writerThread
  , deleteWriteQueues
  , batchOwnedSize
  , batchDependenciesSize
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Trace (traceMsg)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Default
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Monoid as Monoid
import Data.Text as Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import System.Clock
import System.Timeout

import ServiceData.GlobalStats
import ServiceData.Types
import Util.Control.Exception
import Util.Defer
import Util.Log
import Util.STM

import Glean.Database.Open
import Glean.Database.Trace
import Glean.Database.Write.Batch
import Glean.Database.Types
import qualified Glean.RTS.Foreign.Subst as Subst
import Glean.RTS.Foreign.Ownership (DefineOwnership)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift hiding (Database, Exception)
import Glean.Write.JSON
import Glean.Util.Metric
import Glean.Util.Observed as Observed
import Util.Time

{-
Write Queue Monitoring
----------------------

glean.db.write.failed.avg.60
  Total size of write batches that failed (kB)

glean.db.write.succeeded.avg.60
  Total size of write batches that succeeded (kB)

glean.db.write.queue.avg.60
  Total size of write queues (kB)

glean.db.write.queue.<repo>.avg.60
  Size of write queues for <repo> (kB)

glean.db.write.enqueued.avg.60
  Total size of write batches enqueued (kB)

glean.db.write.rejected.avg.60
  Total size of write batches rejected due to queue limit (kB)
-}

-- | Create threads to process the write queues
writerThread :: Env -> WriteQueues -> IO ()
writerThread env WriteQueues{..} = mask $ \restore ->
  forever $ handler restore $
    void $ tryBracket
      dequeue
      done
      execute
 where
  done (Just (WriteJob{..}, repo, WriteQueue{..})) result = do
    latency <- do
      writeEnd <- getTime Monotonic
      return (writeEnd - pointStart writeStart)
    if isLeft result then
      addStatValueType "glean.db.write.failed" (writeSize `div` k) Sum
    else
      addStatValueType "glean.db.write.succeeded" (writeSize `div` k) Sum
    immediately $ do
      now $ writeTVar writeQueueLatency latency
      now $ modifyTVar' writeQueueActive (subtract 1)
      queueCount <- now $ updateTVar writeQueueCount (subtract 1)
      qSize <- now $ updateTVar writeQueueSize (subtract writeSize)
      size <- now $ updateTVar writeQueuesSize (subtract writeSize)
      later $ reportQueueSizes repo queueCount qSize size (Just latency)
    -- don't put the MVar until we have updated writeQueueActive, otherwise
    -- there is a race condition with workFinished which will fail because
    -- active /= 0.
    void $ tryPutMVar writeDone result
  done _ _ = return ()

  handler restore action = do
    r <- tryAll (restore action)
    case r of
      Left e -> do logError (show e); return ()
      Right () -> return ()

  dequeue = atomically $ dequeueLoop (pure ())

  -- Round-robin processing of write queues.
  --  * If we take a job from a queue, and it has other jobs left,
  --    we put it to the back of writeQueues
  --  * If we hit a checkpoint that's not ready, we keep looking
  --    and put the queue with the checkpoint to the back of
  --    writeQueues
  --  * But if there are only queues with checkpoints that aren't
  --    ready, we will block and not busy-wait.
  --
  -- This is an O(n) transaction which could be bad, but in
  -- practice the number of queues with checkpoints will be small.
  dequeueLoop requeueCheckpoints = do
    (repo, queue@WriteQueue{..}) <- readTQueue writeQueues
    maybeJob <- tryReadTQueue writeQueue
    case maybeJob of
      Nothing -> do requeueCheckpoints; return Nothing
      Just job -> do
        let
          requeue = do
            requeueCheckpoints
            whenM (not <$> isEmptyTQueue writeQueue) $
              writeTQueue writeQueues (repo, queue)
        case job of
          WriteJob{} -> do
            requeue
            modifyTVar' writeQueueActive (+1)
            return (Just (job, repo, queue))
          WriteCheckpoint{} -> do
            active <- readTVar writeQueueActive
            if active == 0  -- we get to execute the action
              then do requeue; return (Just (job, repo, queue))
              else do unGetTQueue writeQueue job; dequeueLoop requeue

  execute (Just (WriteJob{..}, repo, _)) = do
    writeContent <- writeContentIO
    writeDatabase env repo writeContent writeStart
  execute (Just (WriteCheckpoint io, _, _)) = do io; return Subst.empty
  execute _ = return Subst.empty

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

-- | Update stats and throw a Retry exception
rejectWrite :: Repo -> Int -> Double -> IO retry
rejectWrite repo size elapsed = do
  let clamped = min 1000.0 $ max 1.0 elapsed
      repoB = Text.encodeUtf8 (repo_name repo)
  addStatValueType "glean.db.write.rejected" (size `div` k) Sum
  addStatValueType ("glean.db.write.retry_ms." <> repoB)
    (round (elapsed*1000)) Avg
  throwIO (Retry clamped)

-- | Add a write job to the queue, or throw 'Retry' if no memory available
enqueueWrite
  :: Env
  -> Repo
  -> Int
  -> IO WriteContent
  -> IO (MVar (Either SomeException Subst.Subst))
enqueueWrite env@Env{..} repo size writeContent = do
  start <- beginTick 1
  config <- Observed.get envServerConfig
  mvar <- newEmptyMVar
  withWritableDatabase env repo $ \queue@WriteQueue{..} -> do
  let WriteQueues{..} = envWriteQueues
      enqueueIt = do
        pending <- now $ readTVar writeQueuesSize
        let !newSize = pending + size
        now $ do
          addToWriteQueue
            repo
            queue
            envWriteQueues
            WriteJob
              { writeSize = size
              , writeContentIO = writeContent
              , writeDone = mvar
              , writeStart = start }
        queueCount <- now $ updateTVar writeQueueCount (+1)
        queueSize <- now $ updateTVar writeQueueSize (+ size)
        later $ do
          addStatValueType "glean.db.write.enqueued" (size `div` k) Sum
          reportQueueSizes repo queueCount queueSize newSize Nothing
  immediately $ do
    check <- now $ checkMemoryAvailable env config size
    if check then enqueueIt else do
      latency <- now $ readTVar writeQueueLatency
      let elapsed = fromIntegral (toNanoSecs latency) / 1000000000.0
      later $ rejectWrite repo size elapsed
  return mvar

-- | Add a checkpoint to the write queue, which will be performed when
-- all previous writes have completed.
enqueueCheckpoint
  :: Env
  -> Repo
  -> IO ()
  -> IO ()
enqueueCheckpoint env repo io = withWritableDatabase env repo $ \queue ->
  atomically $ void $
    addToWriteQueue repo queue (envWriteQueues env) (WriteCheckpoint io)

addToWriteQueue
  :: Repo
  -> WriteQueue
  -> WriteQueues
  -> WriteJob
  -> STM ()
addToWriteQueue repo queue@WriteQueue{..} WriteQueues{..} job = do
  wasEmpty <- isEmptyTQueue writeQueue
  writeTQueue writeQueue job
  -- if this repo previously had no writes, add it to the round-robin
  when wasEmpty $ writeTQueue writeQueues (repo, queue)

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

enqueueBatch :: Env -> ComputedBatch -> Maybe DefineOwnership -> IO SendResponse
enqueueBatch env ComputedBatch{..} ownership = do
  let size = ByteString.length (batch_facts computedBatch_batch)
  traceMsg (envTracer env)
    (GleanTraceEnqueue computedBatch_repo EnqueueBatch size) $ do
  -- NOTE: we use UUIDs here rather than, say, consecutive
  -- numbers because we want to avoid conflicts when the
  -- server restarts/crashes
  handle <- UUID.toText <$> UUID.nextRandom

  let size = batchSize computedBatch_batch
  r <- try $ enqueueWrite env computedBatch_repo size $ pure $
        (writeContentFromBatch computedBatch_batch) {
          writeOwnership= ownership
        }
  case r of
    -- ToDo: make sendBatch use Retry exceptions instead of results too
    Left (Retry n) ->
      return $ Thrift.SendResponse_retry (BatchRetry n)
    Right write -> do
     when computedBatch_remember $ rememberWrite env handle write
     return $ Thrift.SendResponse_handle handle

batchSize :: Thrift.Batch -> Int
batchSize Thrift.Batch{..} =
  ByteString.length batch_facts +
  batchOwnedSize batch_owned +
  batchDependenciesSize batch_dependencies

batchOwnedSize :: HashMap UnitName ListOfIds -> Int
batchOwnedSize = Monoid.getSum . foldMap (Monoid.Sum . storableSize)

batchDependenciesSize :: HashMap Id [FactDependencies] -> Int
batchDependenciesSize deps =
  Monoid.getSum (foldMap (Monoid.Sum . depsSize) deps)
  where
  depsSize deps = sum
    [ storableSize f + storableSize d
    | FactDependencies f d <- deps
    ]

storableSize :: forall a . VS.Storable a => VS.Vector a -> Int
storableSize = (sizeOf (undefined :: a) *) . snd . VS.unsafeToForeignPtr0

enqueueJsonBatch
  :: Env
  -> Repo
  -> Thrift.SendJsonBatch
  -> IO Thrift.SendJsonBatchResponse
enqueueJsonBatch env repo batch = do
  let
    jsonFactBatchSize JsonFactBatch{..} =
      sum (map ByteString.length jsonFactBatch_facts) +
      maybe 0 ByteString.length jsonFactBatch_unit
    size = sum (map jsonFactBatchSize (sendJsonBatch_batches batch))
  traceMsg (envTracer env) (GleanTraceEnqueue repo EnqueueJsonBatch size) $ do
  handle <- UUID.toText <$> UUID.nextRandom
  write <- enqueueWrite env repo size $ writeJsonBatch env repo batch
  when (sendJsonBatch_remember batch) $ rememberWrite env handle write
  return $ def { sendJsonBatchResponse_handle = handle }

-- | Version of enqueueJsonBatch where the facts are ByteStrings. This
-- is to avoid unnecessary ByteString->Text->ByteString conversion
-- when reading from Scribe.
--
-- TODO: this could go away if we could get Thrift to use ByteString
-- as the representation for string, then we could just use
-- enqueueJsonBatch.
--
enqueueJsonBatchByteString
  :: Env
  -> Repo
  -> PredicateRef
  -> [ByteString] -- ^ facts
  -> SendJsonBatchOptions
  -> Bool -- ^ remember?
  -> IO Thrift.SendJsonBatchResponse
enqueueJsonBatchByteString env repo pred facts opts remember = do
  let
    size = sum (map ByteString.length facts)
  traceMsg (envTracer env) (GleanTraceEnqueue repo EnqueueJsonBatchBS size) $ do
  handle <- UUID.toText <$> UUID.nextRandom
  write <- enqueueWrite env repo size $
    writeJsonBatchByteString env repo pred facts opts
  when remember $ rememberWrite env handle write
  return $ def { sendJsonBatchResponse_handle = handle }

pollBatch :: Env -> Handle -> IO FinishResponse
pollBatch env@Env{..} handle = do
  r <- HashMap.lookup handle <$> readTVarIO envWrites
  case r of
    Just write -> do
      -- for tiny writes that will complete in a few ms, we would like
      -- to wait synchronously. Otherwise we'll return a Retry to the
      -- caller which will wait 1s before polling again. In particular
      -- all those one-second delays make tests run slowly.
      s <- timeout 100000 $ readMVar (writeWait write)
      case s of
        Just x -> do
          atomically $ void $ updateTVar envWrites $ HashMap.delete handle
          case x of
            Right subst ->
              return $ Thrift.FinishResponse_subst $ Subst.serialize subst
            Left exc -> case fromException exc of
              Just Retry{..} -> return $ Thrift.FinishResponse_retry $
                Thrift.BatchRetry retry_seconds
              Nothing -> throwIO exc
        Nothing -> do
          timeout <- getWriteTimeout env
          atomically
            $ void
            $ updateTVar envWrites
            $ HashMap.adjust
                (\w -> w { writeTimeout = timeout })
                handle
          return $ Thrift.FinishResponse_retry $ Thrift.BatchRetry 0
    Nothing -> throwIO Thrift.UnknownBatchHandle

rememberWrite
  :: Env
  -> Thrift.Handle
  -> MVar (Either SomeException Subst.Subst)
  -> IO ()
rememberWrite env@Env{..} handle write = do
  timeout <- getWriteTimeout env
  atomically
    $ modifyTVar' envWrites
    $ HashMap.insert handle Write
        { writeWait = write
        , writeTimeout = timeout
        }

getWriteTimeout :: Env -> IO TimePoint
getWriteTimeout Env{..} = do
  ServerConfig.Config{..} <- Observed.get envServerConfig
  now <- getTimePoint
  return $ addToTimePoint now $ seconds $ fromIntegral config_db_writes_keep

-- | Periodically remove write handles that have timed out. NB: The writes
-- themselves will still be executed but the substitutions they produce can
-- no longer be queried.
reapWrites :: Env -> TVar (HashMap Text Write) -> IO ()
reapWrites Env{..} writes = forever $ do
  ServerConfig.Config{..} <- Observed.get envServerConfig
  threadDelay $ fromIntegral config_db_writes_reap * 1000000
  now <- getTimePoint
  atomically $ modifyTVar' writes $ HashMap.filter $ \x -> writeTimeout x > now

deleteWriteQueues :: Env -> OpenDB s -> STM ()
deleteWriteQueues env OpenDB{odbWriting = Just Writing{..}} = do
  let !WriteQueue{..} = wrQueue
  size <- readTVar writeQueueSize
  modifyTVar' (writeQueuesSize (envWriteQueues env)) (subtract size)
  void $ flushTQueue writeQueue
  -- This WriteQueue might still be on the writeQueues, but it will
  -- get removed by the next write thread to encounter it.
deleteWriteQueues _ _ = return ()

k :: Int
k = 1024

mb :: Int
mb = 1024*1024

roundUp :: Int -> Int -> Int
roundUp unit x = (x + unit-1) `div` unit

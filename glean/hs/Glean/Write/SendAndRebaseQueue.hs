{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Write.SendAndRebaseQueue
  ( SendAndRebaseQueueSettings(..)
  , SendAndRebaseQueue
  , withSendAndRebaseQueue
  , writeSendAndRebaseQueue
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Word
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Util.Log
import Util.STM

import Glean.Backend.Types (Backend)
import Glean.FFI (release)
import qualified Glean.Write.SendQueue as SendQueue
import Glean.Write.SendQueue (SendQueue)
import Glean.RTS.Constants (firstAnonId)
import Glean.Write.Stats as Stats
import Glean.RTS.Foreign.Define
import Glean.RTS.Foreign.Stacked
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.LookupCache (LookupCache, countFailuresAsMisses)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Subst
import Glean.RTS.Types (Fid(..))
import Glean.Util.Metric
import qualified Glean.Types as Thrift

{-
SendAndRebaseQueue
------------------

This is a queue for sending facts to a Glean server. It has a couple
of advantages over using a raw SendQueue:

* The queue avoids sending duplicate facts to the server by performing
  some de-duplication before sending the data.

* When the input is JSON, the facts are serialized as binary before
  being sent to the server.

De-duplication
--------------

De-duplication happens in two ways:

1. Each batch is added to the current fact buffer, and de-duplicatd
against the facts already in the buffer.

2. The contents of the buffer are sent to the server asynchronously,
and when the server responds with a substitution (mapping facts in the
buffer to their real fact IDs) the queue places the renamed facts in a
cache, which is used to de-duplicate new batches.

Details
-------

The queue looks like this:

  +-------+     +-------------+
  | cache |     | fact buffer |
  +-------+     +-------------+

when a new batch is added (via writeSendAndRebaseQueue), it is added
to the fact buffer:

                              +---------------+
                              |   new facts   |
                              +---------------+
                              |              /
                              |   rebase    /
                              |            /
  +-------+     +-------------+-----------+
  | cache |     | fact buffer | new facts |
  +-------+     +-------------+-----------+

when this happens, any facts in the new batch that duplicate facts in
the cache or the existing fact buffer are removed.

The fact buffer is sent to the server as a single batch:

  +-------+     +-------------+
  | cache |     | fact buffer |
  +-------+     +------+------+
                       |
                       v
                    server

which will later respond with a substitution. When the server
responds, we will have added more facts to the buffer, so the
situation is now:

  +-------+     +-------------+-----------+
  | cache |     | fact buffer | new facts |
  +-------+     +------+------+-----------+
                       |
                       v
                    server

The server's substitution maps facts in the buffer to their real fact
IDs. At this point we rename the fact buffer using the substitution,
and move those facts into the cache, leaving the fact buffer
containing just the new facts:

  +-------+-------------+    +-----------+
  | cache | fact buffer |    | new facts |
  +-------+-------------+    +-----------+

The new facts are then sent to the server:

  +-------+-------------+    +-----------+
  | cache | fact buffer |    | new facts |
  +-------+-------------+    +-----+-----+
                                   |
                                   v
                                server

so over time we build up a cache of facts from the server:
essentially a cache of part of the DB, and use it to avoid sending
facts that are already in the DB, thereby reducing the amount of data
we send to the server.

All of this is subject to limits:

- sendAndRebaseQueueFactCacheSize: a limit on the size of the cache
  (we drop old entries according to an eviction policy)

- sendAndRebaseQueueFactBufferSize: A limit on the size of the fact
  buffer (writers will wait for the server to respond if the buffer is
  full)

Threads and SendQueue
---------------------

The fact buffer is a bottleneck if multiple threads are writing
simultaneously. To avoid that, we can have multiple fact buffers that
share a cache: this is done by setting sendAndRebaseQueueThreads to a
value higher than 1.

SendAndRebaseQueue is built on top of SendQueue, which manages sending
the individual batches to the server, retrying and resending as
necessary. SendQueue is automatically configured to use the same
number of sender threads as SendAndRebaseQueue, to parallelise the
sending.
-}


-- | When the send operation fails or is completed this is called once.
type Callback = Either SomeException () -> STM ()

newtype Ownership = Ownership
  { ownershipUnits :: HashMap Thrift.UnitName (Vector Thrift.Id)
     -- ^ exactly the same as Batch.owned in glean.thrift
  }

data WaitSubst
  = WaitSubstNone
  | WaitSubstError SomeException
  | WaitSubstSuccess Thrift.Subst

data Sender = Sender
  { sId :: Integer
  , sSubstVar :: TMVar WaitSubst
  , sSent :: TVar Point
    -- ^ Records the size and time the last batch was sent, for stats
  , sFacts :: MVar (FactSet, [Ownership])
    -- ^ [Ownership] is the ownership assignments for facts in the FactSet
  , sCallbacks :: TQueue Callback
  }

data SendAndRebaseQueue = SendAndRebaseQueue
  { srqSendQueue :: !SendQueue
    -- ^ Underlying send queue

  , srqSenders :: TQueue Sender
    -- ^ Senders accumulate facts while waiting for a substitution

  , srqInventory :: !Inventory
    -- ^ Schema inventory (used for rebasing)

  , srqFacts :: !LookupCache
    -- ^ Cache the ids of facts stored on the server

  , srqCacheStats :: !LookupCache.Stats
    -- ^ Stats for the Lookup cache

  , srqStats :: Maybe Stats
    -- ^ How to report stats.

  , srqFactBufferSize :: !Int
    -- ^ Max size of fact buffer
  }

-- | Settings for a 'SendAndRebase'
data SendAndRebaseQueueSettings = SendAndRebaseQueueSettings
  { -- | Settings for the underlying send queue
    sendAndRebaseQueueSendQueueSettings :: !SendQueue.SendQueueSettings

    -- | Max memory that the fact cache should use
  , sendAndRebaseQueueFactCacheSize :: !Int

    -- | Max memory that the fact buffer should use (per sender). The
    -- purpose of this limit is to prevent the buffer from growing
    -- without bound if the server's write queue is full. When the
    -- buffer exceeds this limit, writers will start waiting for the
    -- server. Note that the limit is per sender.
  , sendAndRebaseQueueFactBufferSize :: !Int

    -- | Number of senders
  , sendAndRebaseQueueSenders :: !Int

    -- | Allow facts in the batch to make reference to facts in the remote
    -- server that may not be in the local cache.
  , sendAndRebaseQueueAllowRemoteReferences :: Bool

    -- | How to log stats. Obatin this with 'Glean.Write.Stats.new'.
  , sendAndRebaseQueueStats :: Maybe Stats
  }

-- | Add a new batch to the fact buffer, de-duplicating against the
-- buffer and the cache.
rebase
  :: Inventory
  -> Thrift.Batch
  -> LookupCache
  -> FactSet.FactSet
  -> IO (FactSet.FactSet, Ownership)
rebase inventory batch cache base = do
  LookupCache.withCache Lookup.EmptyLookup cache LookupCache.FIFO $ \cache -> do
    -- when there are multiple senders, the cache may have new facts since
    -- we previously rebased, and it may contain fact IDs that overlap with
    -- the current base. So we have to use a snapshot of the cache, restricted
    -- to fact IDs less than the current base startingId.
    first_id <- Lookup.startingId base
    Lookup.withSnapshot cache first_id $ \snapshot -> do
      factSet <- Lookup.firstFreeId base >>= FactSet.new
      let define = snapshot `stacked` base `stacked` factSet
      subst <- defineBatch define inventory batch DefineFlags
        { trustRefs = True
        , ignoreRedef = True  -- see Note [redefinition]
        }
      let owned = substOwnership subst (Thrift.batch_owned batch)
      return (factSet, Ownership owned)

{- Note [redefinition]

Redefinition is when we have two facts A->B, C->D where A == C but B /= D.

Normally this would be an error, and defineBatch rejects it with an
error. But it can arise naturally here because when
typechecking/rebasing A->B we may have used a different cache from
when we typechecked/rebased C->D. So even though B and D may be
semantically identical, they are literally different.

It doesn't seem possible to ensure that we always use a consistent
cache, so it's inevitable that benign fact redefinitions may
occur. Therefore we ignore redefinitions in `defineBatch`.

Note: this might mean that we are ignoring actual errors and silently
picking one of the two facts if they really did differ. That's bad,
but I don't see an alternative.

-}

toBatchOwnership :: [Ownership] -> HashMap Thrift.UnitName (Vector Thrift.Id)
toBatchOwnership =
  fmap Vector.concat .
  foldr
    (HashMap.unionWith (<>) . fmap (: []) . ownershipUnits)
    HashMap.empty

rebaseOwnershipList :: [Ownership] -> Subst -> [Ownership]
rebaseOwnershipList ownership subst =
  [ Ownership (substOwnership subst own)
  | Ownership own <- ownership
  ]

substOwnership
  :: Subst
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
substOwnership subst = fmap (coerce $ substIntervals subst)

-- | Send the current fact buffer to the server
senderFlush :: SendAndRebaseQueue -> Sender -> IO ()
senderFlush srq sender = modifyMVar_ (sFacts sender) $ \(facts, owned) -> do
  factOnlyBatch <- FactSet.serialize facts
  let batch = factOnlyBatch { Thrift.batch_owned = toBatchOwnership owned }
  !size <- FactSet.factMemory facts
  start <- beginTick (fromIntegral size)
  atomically $ do
    callbacks <- flushTQueue (sCallbacks sender)
    SendQueue.writeSendQueue (srqSendQueue srq) batch $ \result -> do
      case result of
        Right subst -> putTMVar (sSubstVar sender) (WaitSubstSuccess subst)
        Left e -> putTMVar (sSubstVar sender) (WaitSubstError e)
      forM_ callbacks ($ void result)
    writeTVar (sSent sender) start
  return (facts, [])

-- | If the server has returned a substutition, rebase the current
-- fact buffer against it. Send the current fact buffer to the server,
-- unless we're already waiting for a substitution.
senderRebaseAndFlush :: Bool -> SendAndRebaseQueue -> Sender -> IO ()
senderRebaseAndFlush wait srq sender = do
  maybeSubst <- atomically $
    if wait then
      Just <$> takeTMVar (sSubstVar sender)
    else
      tryTakeTMVar (sSubstVar sender)
  case maybeSubst of
    Nothing -> do -- waiting on subst
      log "Waiting on substitution from server"
      return ()
    Just (WaitSubstError e) -> do
      log "Send failure"
      atomically $ putTMVar (sSubstVar sender) (WaitSubstError e)
      throwIO e
    Just WaitSubstNone -> do -- first send
      log "Sending first batch"
      senderFlush srq sender
    Just (WaitSubstSuccess thriftSubst) -> do -- got subst
      log "Sending next batch"
      handle (\(e :: SomeException) -> do
          logError (show e)
          atomically $ putTMVar (sSubstVar sender) (WaitSubstError e)
          throwIO e) $
        modifyMVar_ (sFacts sender) $ \(base,owned) ->
          -- eagerly release the subst when we're done
          bracket (deserialize thriftSubst) release $ \subst -> do
            (newBase, newSubst) <-
              FactSet.rebase (srqInventory srq) subst (srqFacts srq) base
            let newOwned = rebaseOwnershipList owned newSubst
            _ <- evaluate (force (map ownershipUnits newOwned))
            return (newBase, newOwned)
      -- "Commit throughput" will be write throughput to the server
      start <- readTVarIO (sSent sender)
      statBump srq Stats.commitThroughput =<< endTick start
      senderFlush srq sender
    where log msg = vlog 1 $ "Sender " <> show (sId sender) <> ": " <> msg

updateLookupCacheStats :: SendAndRebaseQueue -> IO ()
updateLookupCacheStats SendAndRebaseQueue{..} =
  forM_ srqStats $ \stats -> do
    statValues <- LookupCache.readStatsAndResetCounters srqCacheStats
    Stats.bump stats Stats.lookupCacheStats (countFailuresAsMisses statValues)

statTick :: SendAndRebaseQueue -> Bump Tick -> Word64 -> IO a -> IO a
statTick SendAndRebaseQueue{..} bump val act =
  case srqStats of
    Nothing -> act
    Just stats -> Stats.tick stats bump val act

statBump :: SendAndRebaseQueue -> Bump Tick -> Tick -> IO ()
statBump SendAndRebaseQueue{..} bump val =
  case srqStats of
    Nothing -> return ()
    Just stats -> Stats.bump stats bump val

senderSendOrAppend
  :: SendAndRebaseQueue
  -> Sender
  -> Thrift.Batch
  -> Callback
  -> Point
  -> IO ()
senderSendOrAppend srq sender batch callback latency = do
  -- "Mutator latency" is the latency between calling writeSendAndRebaseQueue
  -- and having a free Sender to write the batch.
  statBump srq Stats.mutatorLatency =<< endTick latency
  let !size = BS.length $ Thrift.batch_facts batch
  newSize <-
    -- "Mutator throughput" is how fast we are appending new facts
    -- to the FactSet.
    statTick srq Stats.mutatorThroughput (fromIntegral size) $ do
      modifyMVar (sFacts sender) $ \(base, ownership) -> do
        (facts, owned) <- rebase (srqInventory srq) batch (srqFacts srq) base
        FactSet.append base facts
        atomically $ writeTQueue (sCallbacks sender) callback
        newSize <- FactSet.factMemory base
        return ((base, owned : ownership), newSize)
  updateLookupCacheStats srq
  let !wait = newSize >= srqFactBufferSize srq
  senderRebaseAndFlush wait srq sender

withSendAndRebaseQueue
  :: Backend e
  => e
  -> Thrift.Repo
  -> Inventory
  -> SendAndRebaseQueueSettings
  -> (SendAndRebaseQueue -> IO a)
  -> IO a
withSendAndRebaseQueue backend repo inventory settings action = do
  vlog 1 $ "Allow remote refs: " <> show sendAndRebaseQueueAllowRemoteReferences
  SendQueue.withSendQueue backend repo sendAndRebaseQueueSendQueueSettings $
    \sendQueue -> do
      cacheStats <- LookupCache.newStats
      let cacheSize = fromIntegral sendAndRebaseQueueFactCacheSize
      srq <- SendAndRebaseQueue
        <$> pure sendQueue
        <*> newTQueueIO
        <*> pure inventory
        <*> LookupCache.new cacheSize 1 cacheStats
        <*> pure cacheStats
        <*> pure sendAndRebaseQueueStats
        <*> pure sendAndRebaseQueueFactBufferSize
      bracket_ (createSenderPool srq) (deleteSenderPool srq) $
        action srq
    where
      SendAndRebaseQueueSettings{..} = settings
      createSenderPool srq =
        forM_ senderIds $ \i -> do
          factset <- FactSet.new baseId
          worker <- Sender i
            <$> newTMVarIO WaitSubstNone
            <*> newTVarIO (error "missing sSent")
            <*> newMVar (factset, [])
            <*> newTQueueIO
          atomically $ writeTQueue (srqSenders srq) worker
      deleteSenderPool srq =
        forM_ senderIds $ \_i -> do
          -- don't deadlock here in case we died leaving the queue empty
          sender <- atomically $ tryReadTQueue (srqSenders srq)
          forM_ sender $ senderRebaseAndFlush True srq
      senderIds = take sendAndRebaseQueueSenders [1..]

      baseId = if sendAndRebaseQueueAllowRemoteReferences
        then firstLocalId
        else Fid Thrift.fIRST_FREE_ID

      -- Higher than Ids in the remote db to avoid remote fact references
      -- from being mistaken for local fact references.
      -- Lower than Ids in JSON batches to avoid references within the
      -- batch from being mistaken for references to the local db.
      firstLocalId = Fid (firstAnonId `div` 2)


writeSendAndRebaseQueue
  :: SendAndRebaseQueue
  -> Thrift.Batch
  -> Callback
  -> IO ()
writeSendAndRebaseQueue srq batch callback = do
  point <- beginTick 1
  bracket
    (atomically $ readTQueue $ srqSenders srq)
    (\sender -> atomically $ writeTQueue (srqSenders srq) sender)
    (\sender -> senderSendOrAppend srq sender batch callback point)

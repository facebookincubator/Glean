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
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Word

import Util.Log
import Util.STM

import Glean.Backend.Types (Backend)
import Glean.FFI (release)
import qualified Glean.Write.SendQueue as SendQueue
import Glean.Write.SendQueue (SendQueue)
import Glean.RTS.Constants (firstAnonId)
import Glean.Write.Stats as Stats
import Glean.RTS.Foreign.Define
import Glean.RTS.Foreign.Ownership
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

(The cache is srqFacts in the code that follows, and the fact buffer is
sFacts, i.e. there's one per Sender)
When a new batch is added (via writeSendAndRebaseQueue), it is added
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

    -- | How to log stats. Obtain this with 'Glean.Write.Stats.new'.
  , sendAndRebaseQueueStats :: Maybe Stats
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

-- | A Sender owns a single fact buffer and drives the flush/rebase cycle
-- for it. Multiple Senders run concurrently (see
-- 'sendAndRebaseQueueSenders'): they share the queue's fact cache
-- ('srqFacts') but each accumulates its own batch independently, so writers
-- don't contend on one buffer.
--
-- A Sender has at most one batch in flight at a time. 'sSubstVar' holds the
-- outcome of that in-flight send (or 'WaitSubstNone' before the first send);
-- the next flush waits on it so the buffer can be rebased onto the real fact
-- IDs the server assigned before more facts are sent.
data Sender = Sender
  { sId :: Integer
  , sSubstVar :: TMVar WaitSubst
    -- ^ Outcome of the in-flight send: the server's substitution, an error,
    -- or 'WaitSubstNone' if nothing has been sent yet
  , sSent :: TVar Point
    -- ^ Records the size and time the last batch was sent, for stats
  , sFacts :: MVar (FactSet, [FactOwnership], ACLConfig)
    -- ^ The fact buffer plus the state that travels with it: the ownership
    -- accumulated for the next flush, and the 'ACLConfig' to propagate with
    -- the batch.
  , sCallbacks :: TQueue Callback
    -- ^ Callbacks to run when the in-flight batch completes
  }

-- | ACL configuration that needs to be propagated with batches
newtype ACLConfig = ACLConfig
  { aclConfig :: Maybe (HashMap.HashMap Text [Text])
  }

-- | An 'ACLConfig' with no configuration.
emptyACLConfig :: ACLConfig
emptyACLConfig = ACLConfig Nothing

-- | Combine ACL configs, preferring the newer config when it is present.
-- A 'Nothing' newer config leaves the existing config untouched.
preferNewerACLConfig :: ACLConfig -> ACLConfig -> ACLConfig
preferNewerACLConfig old new = ACLConfig
  { aclConfig = case aclConfig new of
      Nothing -> aclConfig old
      just -> just
  }

-- | The outcome a 'Sender' is waiting for from its in-flight send: nothing
-- sent yet, a send error, or the server's substitution.
data WaitSubst
  = WaitSubstNone
  | WaitSubstError SomeException
  | WaitSubstSuccess Thrift.Subst

-- | When the send operation fails or is completed this is called once.
type Callback = Either SomeException () -> STM ()

-- | Create a 'SendAndRebaseQueue' for the given repo, run the supplied
-- action with it, and tear it down afterwards. Sets up the underlying
-- 'SendQueue' and a pool of 'sendAndRebaseQueueSenders' senders; on exit any
-- in-flight batches are drained (a final blocking rebase/flush per sender).
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
      srq <- SendAndRebaseQueue sendQueue
        <$> newTQueueIO
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
            <*> newMVar (factset, [], emptyACLConfig)
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


-- | Write a batch to the queue. Takes a free 'Sender' from the pool
-- (blocking until one is available), hands the batch to it, and returns the
-- sender to the pool when done. The 'Callback' fires once the batch's send
-- has completed (or failed).
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

-- | Add an incoming batch to a sender's buffer, then flush if needed.
--
-- The batch is rebased and de-duplicated against the cache and the existing
-- buffer, its ownership is accumulated for the next flush, and its ACL config
-- is merged in (newer config wins). If the buffer has grown past
-- 'srqFactBufferSize' we flush with wait=True, applying back-pressure to the
-- writer until the server responds.
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
  -- Extract ACL config from incoming batch
  let incomingACL = ACLConfig
        { aclConfig = Thrift.batch_acl_config batch
        }
  newSize <-
    -- "Mutator throughput" is how fast we are appending new facts
    -- to the FactSet.
    statTick srq Stats.mutatorThroughput (fromIntegral size) $ do
      modifyMVar (sFacts sender) $ \(base, ownership, existingACL) -> do
        (facts, owned) <-
          rebase (srqInventory srq) batch (srqFacts srq) base
        FactSet.append base facts
        atomically $ writeTQueue (sCallbacks sender) callback
        newSize <- FactSet.factMemory base
        -- Prefer the newer ACL config when present
        let mergedACL = preferNewerACLConfig existingACL incomingACL
        return ((base, owned : ownership, mergedACL), newSize)
  updateLookupCacheStats srq
  let !wait = newSize >= srqFactBufferSize srq
  senderRebaseAndFlush wait srq sender

-- | Add a new batch to the fact buffer, de-duplicating against the
-- buffer and the cache.
rebase
  :: Inventory
  -> Thrift.Batch
  -> LookupCache
  -> FactSet.FactSet
  -> IO (FactSet.FactSet, FactOwnership)
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
      owned <- substOwnership subst $
        FactOwnership $ Thrift.batch_owned batch
      return (factSet, owned)

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

-- | Advance the flush/rebase cycle for a sender, based on the outcome of
-- the in-flight send held in 'sSubstVar':
--
--   * 'WaitSubstNone' (nothing sent yet): flush the first batch.
--   * 'WaitSubstSuccess': rebase the fact buffer onto the real fact IDs the
--     server assigned, rebase the accumulated ownership for the next flush,
--     then flush again.
--   * 'WaitSubstError': re-store the error and rethrow.
--   * No substitution available yet (and not waiting): do nothing.
--
-- When 'wait' is True we block until the substitution is available; this is
-- used to drain in-flight batches (e.g. at shutdown). Otherwise we only
-- proceed if the substitution has already arrived.
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
        modifyMVar_ (sFacts sender) $ \(base, owned, aclCfg) ->
          -- eagerly release the subst when we're done
          bracket (deserialize thriftSubst) release $ \subst -> do
            (newBase, newSubst) <-
              FactSet.rebase (srqInventory srq) subst (srqFacts srq) base
            -- Rebase the accumulated ownership for the next flush.
            newOwned <- mapM (substOwnership newSubst) owned
            return (newBase, newOwned, aclCfg)
      -- "Commit throughput" will be write throughput to the server
      start <- readTVarIO (sSent sender)
      statBump srq Stats.commitThroughput =<< endTick start
      senderFlush srq sender
    where log msg = vlog 1 $ "Sender " <> show (sId sender) <> ": " <> msg

-- | Serialize the sender's current fact buffer and hand it to the
-- underlying 'SendQueue'. The outgoing batch carries the accumulated
-- ownership plus the current ACL config.
--
-- After sending, the ownership is cleared: the facts stay in the buffer and
-- are only renamed and dropped once the substitution comes back in
-- 'senderRebaseAndFlush', where any ownership accumulated in the meantime is
-- rebased. The ACL config is kept as it applies to every batch for this DB.
senderFlush :: SendAndRebaseQueue -> Sender -> IO ()
senderFlush srq sender =
  modifyMVar_ (sFacts sender) $ \(facts, owned, aclCfg) -> do
  factOnlyBatch <- FactSet.serialize facts
  let batch = factOnlyBatch
        { Thrift.batch_owned = ownershipUnits (unionOwnership owned)
        , Thrift.batch_acl_config = aclConfig aclCfg
        }
  -- Log batch details before sending
  let numFacts = Thrift.batch_count batch
      numOwnershipUnits = HashMap.size (Thrift.batch_owned batch)
      hasACL = case aclConfig aclCfg of
        Just cfg -> not (HashMap.null cfg)
        Nothing -> False
      aclEntries = maybe 0 HashMap.size (aclConfig aclCfg)
  log $ "Flushing batch: facts=" ++ show numFacts ++
        ", ownership_units=" ++ show numOwnershipUnits ++
        ", ownership=" ++ show (length owned) ++
        ", has_acl_config=" ++ show hasACL ++
        (if hasACL then " (" ++ show aclEntries ++ " ACL entries)" else "")
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
  -- Clear the ownership now that it has been sent; ownership accumulated
  -- before the substitution arrives is rebased in 'senderRebaseAndFlush'.
  -- Keep ACL config for the next batch (it applies to all batches for this DB)
  return (facts, [], aclCfg)
  where log msg = vlog 1 $ "Sender " <> show (sId sender) <> ": " <> msg

-- | Read and reset the LookupCache counters and report them via 'srqStats'.
-- A no-op if stats reporting is disabled.
updateLookupCacheStats :: SendAndRebaseQueue -> IO ()
updateLookupCacheStats SendAndRebaseQueue{..} =
  forM_ srqStats $ \stats -> do
    statValues <- LookupCache.readStatsAndResetCounters srqCacheStats
    Stats.bump stats Stats.lookupCacheStats (countFailuresAsMisses statValues)

-- | Run an action while recording a timed stat ('Stats.tick'), or just run
-- the action unchanged if stats reporting is disabled.
statTick :: SendAndRebaseQueue -> Bump Tick -> Word64 -> IO a -> IO a
statTick SendAndRebaseQueue{..} bump val act =
  case srqStats of
    Nothing -> act
    Just stats -> Stats.tick stats bump val act

-- | Record a one-off stat value ('Stats.bump'), or do nothing if stats
-- reporting is disabled.
statBump :: SendAndRebaseQueue -> Bump Tick -> Tick -> IO ()
statBump SendAndRebaseQueue{..} bump val =
  case srqStats of
    Nothing -> return ()
    Just stats -> Stats.bump stats bump val

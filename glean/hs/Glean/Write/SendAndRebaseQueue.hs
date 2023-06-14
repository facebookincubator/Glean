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

-- | When the send operation fails or is completed this is called once.
type Callback = Either SomeException () -> STM ()

data Ownership = Ownership
  { ownershipUnits :: HashMap Thrift.UnitName (Vector Thrift.Id)
     -- ^ exactly the same as Batch.owned in glean.thrift
  , ownershipEnd :: !Fid
     -- ^ exclusive upper bound on the facts covered by ownershipUnits
  }

data Sender = Sender
  { sId :: Integer
  , sSubstVar :: TMVar (Maybe Thrift.Subst)
  , sSent :: TVar Point
    -- ^ Records the size and time the last batch was sent, for stats
  , sFacts :: MVar (FactSet, [Ownership])
    -- ^ [Ownership] is the ownership assignments for facts in the
    -- FactSet, in descending order by ownershipEnd (newest at the
    -- front of the list). Each time we add a new batch to the
    -- FactSet, we prepend its ownership to the list. When rebasing,
    -- we will discard ownership for facts already written by taking a
    -- prefix of the list.
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

  , srqAllowRemoteReferences :: Bool
    -- ^ Allow batches to reference remote facts that are not in the cache.

  , srqCacheStats :: !LookupCache.Stats
    -- ^ Stats for the Lookup cache

  , srqStats :: Maybe Stats
    -- ^ How to report stats.
  }

newSendAndRebaseQueue
  :: Bool
  -> SendQueue
  -> Inventory
  -> Maybe Stats
  -> Int
  -> IO SendAndRebaseQueue
newSendAndRebaseQueue allowRemote sendQueue inventory stats cacheMem = do
  cacheStats <- LookupCache.newStats
  SendAndRebaseQueue
    <$> pure sendQueue
    <*> newTQueueIO
    <*> pure inventory
    <*> LookupCache.new (fromIntegral cacheMem) 1 cacheStats
    <*> pure allowRemote
    <*> pure cacheStats
    <*> pure stats

-- | Settings for a 'SendAndRebase'
data SendAndRebaseQueueSettings = SendAndRebaseQueueSettings
  { -- | Settings for the underlying send queue
    sendAndRebaseQueueSendQueueSettings :: !SendQueue.SendQueueSettings

    -- | Max memory that the fact cache should use
  , sendAndRebaseQueueFactCacheSize :: !Int

    -- | Number of senders
  , sendAndRebaseQueueSenders :: !Int

    -- | Allow facts in the batch to make reference to facts in the remote
    -- server that may not be in the local cache.
  , sendAndRebaseQueueAllowRemoteReferences :: Bool

    -- | How to log stats. Obatin this with 'Glean.Write.Stats.new'.
  , sendAndRebaseQueueStats :: Maybe Stats
  }

rebase
  :: Bool
  -> Inventory
  -> Thrift.Batch
  -> LookupCache
  -> FactSet.FactSet
  -> IO (FactSet.FactSet, Ownership)
rebase  allowRemoteRefs inventory batch cache base = do
  LookupCache.withCache Lookup.EmptyLookup cache LookupCache.LRU $ \lookup -> do
    factSet <- Lookup.firstFreeId base >>= FactSet.new
    let define = stacked (stacked lookup base) factSet
    subst <- defineBatch define inventory batch allowRemoteRefs
    let owned = substOwnership subst (Thrift.batch_owned batch)
    nextId <- Lookup.firstFreeId factSet
    return (factSet, Ownership owned nextId)

toBatchOwnership :: [Ownership] -> HashMap Thrift.UnitName (Vector Thrift.Id)
toBatchOwnership =
  fmap Vector.concat .
  foldr
    (HashMap.unionWith (<>) . fmap (: []) . ownershipUnits)
    HashMap.empty

rebaseOwnershipList :: [Ownership] -> Subst -> Thrift.Id -> [Ownership]
rebaseOwnershipList ownership subst boundary =
  [ own {
      ownershipUnits = rebaseOwnership subst (ownershipUnits own),
      ownershipEnd =
        Fid (fromIntegral (substOffset subst) + fromFid (ownershipEnd own))
    }
  | own <- takeWhile ((> boundary) . coerce . ownershipEnd) ownership
  ]

substOwnership, rebaseOwnership
  :: Subst
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
substOwnership subst = fmap (coerce . substIntervals subst . coerce)
rebaseOwnership subst = fmap (coerce . rebaseIntervals subst . coerce)

senderFlush :: SendAndRebaseQueue -> Sender -> IO ()
senderFlush srq sender = withMVar (sFacts sender) $ \(facts, owned) -> do
  factOnlyBatch <- FactSet.serialize facts
  let batch = factOnlyBatch { Thrift.batch_owned = toBatchOwnership owned }
  !size <- FactSet.factMemory facts
  start <- beginTick (fromIntegral size)
  atomically $ do
    callbacks <- flushTQueue (sCallbacks sender)
    SendQueue.writeSendQueue (srqSendQueue srq) batch $ \result -> do
      case result of
        Right subst -> putTMVar (sSubstVar sender) (Just subst)
        Left _ -> return ()
      forM_ callbacks ($ void result)
    writeTVar (sSent sender) start

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
    Just Nothing -> do -- first send
      log "Sending first batch"
      senderFlush srq sender
    Just (Just thriftSubst) -> do -- got subst
      log "Sending next batch"
      modifyMVar_ (sFacts sender) $ \(base,owned) ->
        -- eagerly release the subst when we're done
        bracket (deserialize thriftSubst) release $ \subst -> do
          newBase <- FactSet.rebase (srqInventory srq) subst (srqFacts srq) base
          let
              -- all facts below the boundary have now been written,
              -- we'll use this to determine which ownership data to
              -- retain.
              boundary =
                Thrift.subst_firstId thriftSubst +
                fromIntegral (Vector.length (Thrift.subst_ids thriftSubst))
              newOwned = rebaseOwnershipList owned subst boundary
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
  statTick srq Stats.mutatorThroughput (fromIntegral size) $ do
    -- "Mutator throughput" is how fast we are appending new facts
    -- to the FactSet.
    modifyMVar_ (sFacts sender) $ \(base, ownership) -> do
      (facts, owned) <- rebase
        (srqAllowRemoteReferences srq)
        (srqInventory srq)
        batch
        (srqFacts srq)
        base
      FactSet.append base facts
      atomically $ writeTQueue (sCallbacks sender) callback
      assert (case ownership of
        [] -> True
        (next:_) -> ownershipEnd owned >= ownershipEnd next) $ return ()
      return (base, owned : ownership)
  updateLookupCacheStats srq
  senderRebaseAndFlush False srq sender

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
      srq <-
        newSendAndRebaseQueue
          sendAndRebaseQueueAllowRemoteReferences
          sendQueue
          inventory
          sendAndRebaseQueueStats
          sendAndRebaseQueueFactCacheSize

      bracket_ (createSenderPool srq) (deleteSenderPool srq) $
        action srq
    where
      SendAndRebaseQueueSettings{..} = settings
      createSenderPool srq =
        forM_ senderIds $ \i -> do
          factset <- FactSet.new baseId
          worker <- Sender i
            <$> newTMVarIO Nothing
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

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Write.SendAndRebaseQueue
  ( SendAndRebaseQueueSettings(..)
  , withSendAndRebaseQueue
  , writeSendAndRebaseQueue
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Coerce
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
import Glean.RTS.Foreign.Define
import Glean.RTS.Foreign.Stacked
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.LookupCache (LookupCache)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Subst
import Glean.RTS.Types (Fid(..))
import qualified Glean.Types as Thrift

-- | When the send operation fails or is completed this is called once.
type Callback = Either SomeException () -> STM ()

data Ownership = Ownership
  { ownershipUnits :: HashMap Thrift.UnitName (Vector Thrift.Id)
     -- ^ exactly the same as Batch.owned in glean.thrift
  , ownershipEnd :: Fid
     -- ^ exclusive upper bound on the facts covered by ownershipUnits
  }

data Sender = Sender
  { sId :: Integer
  , sSubstVar :: TMVar (Maybe Thrift.Subst)
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

  , srqRebaseQueue :: TQueue (Thrift.Batch, Callback)
    -- ^ Batches we haven't rebased yet

  , srqSenders :: TQueue Sender
    -- ^ Senders accumulate facts while waiting for a substitution

  , srqInventory :: !Inventory
    -- ^ Schema inventory (used for rebasing)

  , srqFacts :: !LookupCache
    -- ^ Cache the ids of facts stored on the server

  , srqStats :: !LookupCache.Stats
    -- ^ Cache statistics
  }

newSendAndRebaseQueue :: SendQueue -> Inventory -> Int -> IO SendAndRebaseQueue
newSendAndRebaseQueue sendQueue inventory cacheMem = do
  stats <- LookupCache.newStats
  SendAndRebaseQueue
    <$> pure sendQueue
    <*> newTQueueIO
    <*> newTQueueIO
    <*> pure inventory
    <*> LookupCache.new cacheMem 1 stats
    <*> pure stats

-- | Settings for a 'SendAndRebase'
data SendAndRebaseQueueSettings = SendAndRebaseQueueSettings
  { -- | Settings for the underlying send queue
    sendAndRebaseQueueSendQueueSettings :: !SendQueue.SendQueueSettings

    -- | Max memory that the fact cache should use
  , sendAndRebaseQueueFactCacheSize :: !Int

    -- | Number of senders
  , sendAndRebaseQueueSenders :: !Int
  }

rebase
  :: Inventory
  -> Thrift.Batch
  -> LookupCache
  -> FactSet.FactSet
  -> IO (FactSet.FactSet, Ownership)
rebase inventory batch cache base = do
  LookupCache.withCache Lookup.EmptyLookup cache LookupCache.LRU $ \lookup -> do
    factSet <- Lookup.firstFreeId base >>= FactSet.new
    let define = stacked (stacked lookup base) factSet
    subst <- defineBatch define inventory batch False
    let owned = substOwnership subst (Thrift.batch_owned batch)
    nextId <- Lookup.firstFreeId factSet
    return (factSet, Ownership owned nextId)

toBatchOwnership :: [Ownership] -> HashMap Thrift.UnitName (Vector Thrift.Id)
toBatchOwnership =
  fmap Vector.concat .
  foldr
    (HashMap.unionWith (<>) . fmap (: []) . ownershipUnits)
    HashMap.empty

rebaseOwnership :: [Ownership] -> Subst -> Thrift.Id -> [Ownership]
rebaseOwnership ownership subst boundary =
  [ own { ownershipUnits = substOwnership subst (ownershipUnits own) }
  | own <- takeWhile ((> boundary) . coerce . ownershipEnd) ownership
  ]

substOwnership
  :: Subst
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
  -> HashMap Thrift.UnitName (Vector Thrift.Id)
substOwnership subst = fmap (coerce . substIntervals subst . coerce)

senderFlush :: SendAndRebaseQueue -> Sender -> IO ()
senderFlush srq sender = withMVar (sFacts sender) $ \(facts, owned) -> do
  factOnlyBatch <- FactSet.serialize facts
  let batch = factOnlyBatch { Thrift.batch_owned = toBatchOwnership owned }
  atomically $ do
    callbacks <- flushTQueue (sCallbacks sender)
    SendQueue.writeSendQueue (srqSendQueue srq) batch $ \result -> do
      case result of
        Right subst -> putTMVar (sSubstVar sender) (Just subst)
        Left _ -> return ()
      forM_ callbacks ($ void result)

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
              newOwned = rebaseOwnership owned subst boundary
          _ <- evaluate (force (map ownershipUnits newOwned))
          return (newBase, newOwned)
      senderFlush srq sender
    where log msg = vlog 1 $ "Sender " <> show (sId sender) <> ": " <> msg

senderSendOrAppend
  :: SendAndRebaseQueue
  -> Sender
  -> Thrift.Batch
  -> Callback
  -> IO ()
senderSendOrAppend srq sender batch callback = do
  modifyMVar_ (sFacts sender) $ \(base, ownership) -> do
    (facts, owned) <- rebase (srqInventory srq) batch (srqFacts srq) base
    FactSet.append base facts
    atomically $ writeTQueue (sCallbacks sender) callback
    assert (case ownership of
      [] -> True
      (next:_) -> ownershipEnd owned >= ownershipEnd next) $ return ()
    return (base, owned : ownership)
  senderRebaseAndFlush False srq sender

serviceRebaseQueue :: SendAndRebaseQueue -> IO ()
serviceRebaseQueue srq = do
  next <- atomically $ tryReadTQueue $ srqRebaseQueue srq
  case next of
    Nothing -> return ()
    Just (batch, callback) -> do
      sender <- atomically $ readTQueue $ srqSenders srq
      senderSendOrAppend srq sender batch callback
      atomically $ writeTQueue (srqSenders srq) sender
      serviceRebaseQueue srq

withSendAndRebaseQueue
  :: Backend e
  => e
  -> Thrift.Repo
  -> Inventory
  -> SendAndRebaseQueueSettings
  -> (SendAndRebaseQueue -> IO a)
  -> IO a
withSendAndRebaseQueue backend repo inventory settings action =
  SendQueue.withSendQueue backend repo sendAndRebaseQueueSendQueueSettings $
    \sendQueue -> do
      srq <-
        newSendAndRebaseQueue
          sendQueue
          inventory
          sendAndRebaseQueueFactCacheSize

      bracket_ (createSenderPool srq) (deleteSenderPool srq) $
        action srq
    where
      SendAndRebaseQueueSettings{..} = settings
      createSenderPool srq =
        forM_ senderIds $ \i -> do
          factset <- FactSet.new $ Fid Thrift.fIRST_FREE_ID
          worker <- Sender i
            <$> newTMVarIO Nothing
            <*> newMVar (factset, [])
            <*> newTQueueIO
          atomically $ writeTQueue (srqSenders srq) worker
      deleteSenderPool srq =
        forM_ senderIds $ \_i -> do
          sender <- atomically $ readTQueue (srqSenders srq)
          senderRebaseAndFlush True srq sender
      senderIds = take sendAndRebaseQueueSenders [1..]


writeSendAndRebaseQueue
  :: SendAndRebaseQueue
  -> Thrift.Batch
  -> Callback
  -> IO ()
writeSendAndRebaseQueue srq batch callback = do
  atomically $ writeTQueue (srqRebaseQueue srq) (batch, callback)
  serviceRebaseQueue srq
  stats <- LookupCache.readStatsAndResetCounters $ srqStats srq
  vlog 1 $ "Cache stats: " <> show stats

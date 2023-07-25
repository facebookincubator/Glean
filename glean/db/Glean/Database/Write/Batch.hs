{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Write.Batch
  ( WriteContent(..)
  , syncWriteDatabase
  , writeDatabase
  ) where

import Control.Exception
import Control.Monad.Extra
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Default
import Data.IORef
import Data.Maybe
import qualified Data.Text as Text

import Util.Control.Exception
import Util.STM

import Glean.Database.Open
import Glean.Database.Exception
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Schema
import Glean.Database.Types
import Glean.FFI
import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Define (trustRefs)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Inventory (Inventory)
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Foreign.Subst (Subst)
import qualified Glean.RTS.Foreign.Subst as Subst
import Glean.Types (Repo)
import qualified Glean.Types as Thrift
import Glean.Util.Metric
import Glean.Util.Mutex
import qualified Glean.Write.Stats as Stats

-- | What we are going to write into the DB
data WriteContent = WriteContent
  { writeBatch :: Thrift.Batch
  , writeOwnership :: Maybe DefineOwnership
  }

syncWriteDatabase
  :: Env
  -> Repo
  -> Thrift.Batch
  -> IO Subst
syncWriteDatabase env repo batch = do
  point <- beginTick 1
  writeDatabase env repo (WriteContent batch Nothing) point

writeDatabase
  :: Env
  -> Repo
  -> WriteContent
  -> Point
  -> IO Subst
writeDatabase env repo (WriteContent factBatch maybeOwn) latency =
  readDatabase env repo $ \OpenDB{..} lookup ->
  case odbWriting of
    Just writing -> do
      Stats.bump (envStats env) Stats.mutatorLatency =<< endTick latency
      let !size = batch_size factBatch
          do_write deduped batch = do
            let !real_size = batch_size batch
            Stats.tick (envStats env) Stats.mutatorThroughput size
              $ Stats.tick (envStats env)
                  (if deduped
                    then Stats.mutatorDedupedThroughput
                    else Stats.mutatorDupThroughput) real_size
              $ do
              -- TODO: Start substituting the next batch while the current batch
              -- is being committed (T34620702).
              bracket
                (
                  logExceptions (\s -> inRepo repo $ "rename error: " ++ s) $
                  Stats.tick (envStats env) Stats.renameThroughput real_size $
                  withLookupCache repo writing lookup $ \cache ->
                    renameBatch
                      (schemaInventory odbSchema)
                      cache
                      writing
                      batch
                      deduped
                )
                (\(facts, _) -> release facts)
                  -- release the FactSet now that we're done with it,
                  -- don't wait for the GC to free it.
                (\(facts, subst) -> do
                  updateLookupCacheStats env
                  let !is = Subst.substIntervals subst . coerce <$>
                        Thrift.batch_owned batch
                  forM_ maybeOwn $ \ownBatch ->
                    Ownership.substDefineOwnership ownBatch subst
                  -- before commit, because it may modify facts
                  new_next_id <- Lookup.firstFreeId facts
                  logExceptions (\s -> inRepo repo $ "commit error: " ++ s)
                    $ when (not $ envMockWrites env)
                    $ do
                        mem <- fromIntegral <$> FactSet.factMemory facts
                        Stats.tick (envStats env)
                          Stats.commitThroughput mem $ do
                            Storage.commit odbHandle facts is
                            forM_ maybeOwn $ \ownBatch ->
                              Storage.addDefineOwnership odbHandle ownBatch
                  -- update wrNextId only *after* commit, otherwise we
                  -- will use an incomplete snapshot of the DB in
                  -- parallel de-dupliation below which leads to correctness
                  -- bugs.
                  atomicWriteIORef (wrNextId writing) new_next_id
                  return subst
                )

      Stats.tick (envStats env) Stats.mutatorInput size $ do
      -- If nobody is writing to the DB just write the batch directly.
      --
      -- TODO: What if someone is already deduplicating another batch? Should we
      -- not write in that case?
      r <- tryWithMutex (wrLock writing) $ const $ do_write False factBatch
      case r of
        Just subst -> return subst
        Nothing -> do
          -- Somebody is already writing to the DB - deduplicate the batch
          next_id <- readIORef $ wrNextId writing
          bracket
            (
              withLookupCache repo writing lookup $ \cache ->
              -- We need a snapshot here because we don't want lookups to
              -- return fact ids which conflict with ids in the renamed batch.
              Lookup.withSnapshot cache next_id $ \snapshot ->
              logExceptions (\s -> inRepo repo $ "dedup error: " ++ s) $
                FactSet.renameFacts
                  (schemaInventory odbSchema)
                  snapshot
                  next_id
                  factBatch
                  def
            )
            (\(deduped_facts, _) -> release deduped_facts)
              -- release the FactSet now that we're done with it, don't wait for
              -- the GC to free it.
            (\(deduped_facts, dsubst) -> do
              factCount <- FactSet.factCount deduped_facts
              if factCount == 0 && isNothing maybeOwn then
                return dsubst
              else do
                deduped_batch <- FactSet.serialize deduped_facts
                let !is = coerce . Subst.substIntervals dsubst . coerce
                      <$> Thrift.batch_owned factBatch
                forM_ maybeOwn $ \ownBatch ->
                  Ownership.substDefineOwnership ownBatch dsubst
                -- And now write it do the DB, deduplicating again
                wsubst <- withMutex (wrLock writing) $ const $
                  do_write True deduped_batch { Thrift.batch_owned = is }
                return $ dsubst <> wsubst
            )
    Nothing -> dbError repo "can't write to a read only database (1)"
  where
    batch_size = fromIntegral . BS.length . Thrift.batch_facts

withLookupCache
  :: Repo
  -> Writing
  -> Lookup.Lookup
  -> (Lookup.Lookup -> IO a)
  -> IO a
withLookupCache repo Writing{..} lookup f = do
  let baseName = Lookup.lookupName lookup
  join $ atomically $ do
    maybePrevName <- readTVar wrLookupCacheAnchorName
    case maybePrevName of
      Nothing -> do
        writeTVar wrLookupCacheAnchorName (Just baseName)
        return (return ())
      Just prevName
        | prevName == baseName -> return (return ())
        | otherwise -> return $ dbError repo $
          "base lookup mismatch: prev: " <> Text.unpack prevName <>
          ", new: " <> Text.unpack baseName
  LookupCache.withCache lookup wrLookupCache LookupCache.FIFO f

renameBatch
  :: Lookup.CanLookup l
  => Inventory
  -> l
  -> Writing
  -> Thrift.Batch
  -> Bool
  -> IO (FactSet, Subst)
renameBatch inventory lookup Writing{..} batch trusted = do
  next_id <- readIORef wrNextId
  FactSet.renameFacts inventory lookup next_id batch def { trustRefs = trusted }

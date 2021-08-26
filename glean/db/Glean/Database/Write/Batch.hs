-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Write.Batch
  ( syncWriteDatabase
  , writeDatabase
  ) where

import Control.Exception
import Control.Monad.Extra
import qualified Data.ByteString as BS
import Data.Coerce
import Data.IORef

import Util.Control.Exception

import Glean.Database.Open
import Glean.Database.Exception
import Glean.Database.Repo
import qualified Glean.Database.Stats as Stats
import qualified Glean.Database.Storage as Storage
import Glean.Database.Schema
import Glean.Database.Types
import Glean.FFI
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Inventory (Inventory)
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Subst (Subst)
import qualified Glean.RTS.Foreign.Subst as Subst
import Glean.Types (Repo)
import qualified Glean.Types as Thrift
import Glean.Util.Metric
import Glean.Util.Mutex


syncWriteDatabase
  :: Env
  -> Repo
  -> Thrift.Batch
  -> IO Subst
syncWriteDatabase env repo batch = do
  point <- beginTick 1
  writeDatabase env repo batch point

writeDatabase
  :: Env
  -> Repo
  -> Thrift.Batch
  -> Point
  -> IO Subst
writeDatabase env repo factBatch latency =
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
                  LookupCache.withCache lookup (wrLookupCache writing) $ \l ->
                    renameBatch (schemaInventory odbSchema) l writing batch
                )
                (\(facts, _) -> release facts)
                  -- release the FactSet now that we're done with it,
                  -- don't wait for the GC to free it.
                (\(facts, subst) -> do
                  updateLookupCacheStats env
                  let !is = Subst.substIntervals subst . coerce <$>
                        Thrift.batch_owned batch
                  logExceptions (\s -> inRepo repo $ "commit error: " ++ s)
                    $ when (not $ envMockWrites env)
                    $ do
                        mem <- fromIntegral <$> FactSet.factMemory facts
                        Stats.tick (envStats env) Stats.commitThroughput mem $
                          Storage.commit odbHandle facts is
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
              LookupCache.withCache lookup (wrLookupCache writing) $ \cache ->
              -- We need a snapshot here because we don't want lookups to
              -- return fact ids which conflict with ids in the renamed batch.
              Lookup.withSnapshot cache next_id $ \snapshot ->
              logExceptions (\s -> inRepo repo $ "dedup error: " ++ s) $
                FactSet.renameFacts
                  (schemaInventory odbSchema)
                  snapshot
                  next_id
                  factBatch
            )
            (\(deduped_facts, _) -> release deduped_facts)
              -- release the FactSet now that we're done with it, don't wait for
              -- the GC to free it.
            (\(deduped_facts, dsubst) -> do
              deduped_batch <- FactSet.serialize deduped_facts
              let !is = coerce . Subst.substIntervals dsubst . coerce
                    <$> Thrift.batch_owned factBatch
              -- And now write it do the DB, deduplicating again
              wsubst <- withMutex (wrLock writing) $ const $
                do_write True deduped_batch { Thrift.batch_owned = is }
              return $ dsubst <> wsubst
            )
    Nothing -> dbError repo "can't write to a read only database"
  where
    batch_size = fromIntegral . BS.length . Thrift.batch_facts

renameBatch
  :: Lookup.CanLookup l
  => Inventory
  -> l
  -> Writing
  -> Thrift.Batch
  -> IO (FactSet, Subst)
renameBatch inventory lookup Writing{..} batch = do
  next_id <- readIORef wrNextId
  (facts, subst) <- FactSet.renameFacts inventory lookup next_id batch
  new_next_id <- Lookup.firstFreeId facts
  atomicWriteIORef wrNextId new_next_id
  return (facts, subst)

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
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Coerce
import Data.Default
import Data.Int (Int64)
import Data.IORef
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector
import Data.Word

import Util.Control.Exception
import Util.STM

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Open
import Glean.Database.Exception
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Schema
import Glean.Database.Types
import Glean.FFI
import Glean.Internal.Types as Thrift
import Glean.RTS.Foreign.Define (trustRefs)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup (Lookup)
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Foreign.Subst (Subst)
import qualified Glean.RTS.Foreign.Subst as Subst
import Glean.RTS.Types (Pid(..))
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

makeDefineOwnership
  :: Env
  -> Repo
  -> HashMap Int64 [Thrift.FactDependencies]
  -> IO (Maybe DefineOwnership)
makeDefineOwnership env repo deps
  | HashMap.null deps = return Nothing
  | otherwise = do
  readDatabase env repo $ \odb lookup -> do
  maybeOwnership <- readTVarIO (odbOwnership odb)
  forM maybeOwnership $ \ownership -> do
    nextId <- Lookup.firstFreeId lookup
    define <- Ownership.newDefineOwnership ownership nextId
    forM_ (HashMap.toList deps) $ \(pid, ownerMap) ->
      Ownership.addDerivedOwners lookup define (Pid pid) ownerMap
    return define

checkWritable :: Repo -> OpenDB -> IO Writing
checkWritable repo OpenDB{..} =
  case odbWriting of
    Nothing -> dbError repo "can't write to a read only database"
    Just writing -> return writing

-- | We can only add fact ownership *before* @glean complete@, otherwise
-- it won't get propagated. Similarly, we can only add facts with
-- explicit dependencies *after* @glean complete@.
checkComplete :: Env -> Repo -> Thrift.Batch -> IO ()
checkComplete env repo Thrift.Batch{..} = do
  when (not $ HashMap.null batch_owned) $ do
    meta <- atomically $ Catalog.readMeta (envCatalog env) repo
    when (metaAxiomComplete meta) $
      throwIO $ Thrift.Exception
        "Attempting to write facts with ownership after 'glean complete'"
  when (not $ HashMap.null batch_dependencies) $ do
    -- TODO: enforce that not only the axiom predicates be complete
    -- but that every predicate depended upon be complete.
    meta <- atomically $ Catalog.readMeta (envCatalog env) repo
    when (not (metaAxiomComplete meta)) $
      throwIO $ Thrift.Exception
        "Attempting to write facts with dependencies before 'glean complete'"

-- | Write a batch of facts to the database, returning a substitution
writeDatabase
  :: Env
  -> Repo
  -> WriteContent
  -> Point
  -> IO Subst
writeDatabase env repo (WriteContent batch maybeOwn) latency =
  readDatabase env repo $ \odb@OpenDB{..} lookup -> do
    writing <- checkWritable repo odb
    checkComplete env repo batch
    Stats.bump (envStats env) Stats.mutatorLatency =<< endTick latency
    let !size = batchSize batch

    Stats.tick (envStats env) Stats.mutatorInput size $ do
      -- If nobody is writing to the DB just write the batch directly.
      --
      -- TODO: What if someone is already deduplicating another batch? Should we
      -- not write in that case?
      r <- tryWithMutex (wrLock writing) $ const $
        reallyWriteBatch env repo odb lookup writing size False batch maybeOwn
      case r of
        Just subst -> return subst
        Nothing ->
          -- Somebody is already writing to the DB - deduplicate the batch
          deDupBatch env repo odb lookup writing size batch maybeOwn

reallyWriteBatch
  :: Env
  -> Repo
  -> OpenDB
  -> Lookup
  -> Writing
  -> Word64  -- ^ original size of the batch
  -> Bool  -- ^ has the batch already been de-duped?
  -> Thrift.Batch
  -> Maybe DefineOwnership
  -> IO Subst
reallyWriteBatch env repo OpenDB{..} lookup writing original_size deduped
    batch maybeOwn = do
  let !real_size = batchSize batch
  Stats.tick (envStats env) Stats.mutatorThroughput original_size
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
        withLookupCache repo writing lookup $ \cache -> do
          next_id <- readIORef (wrNextId writing)
          FactSet.renameFacts
            (schemaInventory odbSchema)
            cache
            next_id
            batch
            def { trustRefs = deduped }
      )
      (\(facts, _) -> release facts)
        -- release the FactSet now that we're done with it,
        -- don't wait for the GC to free it.
      (\(facts, subst) -> do
        updateLookupCacheStats env
        let !is = Subst.substIntervals subst . coerce <$>
              Thrift.batch_owned batch
            !deps = substDependencies subst
              <$> Thrift.batch_dependencies batch

        derivedOwners <-
          if | Just owners <- maybeOwn -> do
                Ownership.substDefineOwnership owners subst
                return $ Just owners
             | not $ HashMap.null deps ->
                makeDefineOwnership env repo deps
             | otherwise -> return Nothing

        -- before commit, because it may modify facts
        new_next_id <- Lookup.firstFreeId facts
        logExceptions (\s -> inRepo repo $ "commit error: " ++ s)
          $ when (not $ envMockWrites env)
          $ do
              mem <- fromIntegral <$> FactSet.factMemory facts
              Stats.tick (envStats env)
                Stats.commitThroughput mem $ do
                  Storage.commit odbHandle facts is
                  forM_ derivedOwners $ \ownBatch ->
                    Storage.addDefineOwnership odbHandle ownBatch
        -- update wrNextId only *after* commit, otherwise we
        -- will use an incomplete snapshot of the DB in
        -- parallel de-dupliation below which leads to correctness
        -- bugs.
        atomicWriteIORef (wrNextId writing) new_next_id
        return subst
      )

deDupBatch
  :: Env
  -> Repo
  -> OpenDB
  -> Lookup
  -> Writing
  -> Word64
  -> Thrift.Batch
  -> Maybe DefineOwnership
  -> IO Subst
deDupBatch env repo odb lookup writing original_size batch maybeOwn = do
  next_id <- readIORef $ wrNextId writing
  (maybe_deduped_batch, dsubst) <- bracket
    (
      withLookupCache repo writing lookup $ \cache ->
      -- We need a snapshot here because we don't want lookups to
      -- return fact ids which conflict with ids in the renamed batch.
      Lookup.withSnapshot cache next_id $ \snapshot ->
      logExceptions (\s -> inRepo repo $ "dedup error: " ++ s) $
        FactSet.renameFacts
          (schemaInventory (odbSchema odb))
          snapshot
          next_id
          batch
          def
    )
    (\(deduped_facts, _) -> release deduped_facts)
      -- release the FactSet now that we're done with it, don't wait for
      -- the GC to free it.
    (\(deduped_facts, dsubst) -> do
      factCount <- FactSet.factCount deduped_facts
      if factCount == 0 && isNothing maybeOwn then
        return (Nothing, dsubst)
      else do
        deduped_batch <- FactSet.serialize deduped_facts
        return (Just deduped_batch, dsubst)
    )

  case maybe_deduped_batch of
    Nothing -> return dsubst
    Just deduped_batch -> do
      let !is = coerce Subst.substIntervals dsubst
            <$> Thrift.batch_owned batch
          !deps = substDependencies dsubst
            <$> Thrift.batch_dependencies batch
      forM_ maybeOwn $ \ownBatch ->
        Ownership.substDefineOwnership ownBatch dsubst
      -- And now write it do the DB, deduplicating again
      wsubst <- withMutex (wrLock writing) $ const $
        reallyWriteBatch env repo odb lookup writing original_size True
          deduped_batch
            { Thrift.batch_owned = is
            , Thrift.batch_dependencies = deps
            }
          maybeOwn
      return $ dsubst <> wsubst

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

substDependencies
 :: Subst
 -> [Thrift.FactDependencies]
 -> [Thrift.FactDependencies]
substDependencies subst dmap = map substFD dmap
  where
  substFD (Thrift.FactDependencies facts deps) =
    Thrift.FactDependencies facts' deps'
    where
    !facts' = under (Subst.substVector subst) facts
    !deps' = under (Subst.substVector subst) deps
    under f = Vector.unsafeCast . f . Vector.unsafeCast

batchSize :: Thrift.Batch -> Word64
batchSize = fromIntegral . BS.length . Thrift.batch_facts

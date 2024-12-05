{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Write.Batch
  ( syncWriteDatabase
  , syncWriteContentDatabase
  , writeDatabase
  , writeContentFromBatch
  ) where

import Control.DeepSeq (force)
import Control.Exception
import Control.Monad.Extra
import Control.Trace (traceMsg)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Coerce
import Data.Default
import Data.Int (Int64)
import Data.IORef
import Data.Maybe
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
import Glean.Database.Trace
import Glean.Database.Types
import Glean.FFI
import Glean.Internal.Types as Thrift
import Glean.RTS.Foreign.Define (trustRefs)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup (Lookup, withCanLookup)
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Foreign.Stacked (stacked)
import Glean.RTS.Foreign.Subst (Subst)
import qualified Glean.RTS.Foreign.Subst as Subst
import Glean.RTS.Types (Pid(..))
import Glean.Types (Repo)
import qualified Glean.Types as Thrift
import Glean.Util.Metric
import Glean.Util.Mutex
import qualified Glean.Write.Stats as Stats

writeContentFromBatch :: Thrift.Batch -> WriteContent
writeContentFromBatch writeBatch = WriteContent
  { writeBatch = writeBatch
  , writeOwnership = Nothing
  , writeSubst = id
  }

syncWriteDatabase
  :: Env
  -> Repo
  -> Thrift.Batch
  -> IO Subst
syncWriteDatabase env repo batch =
  syncWriteContentDatabase env repo (writeContentFromBatch batch)

syncWriteContentDatabase :: Env -> Repo -> WriteContent -> IO Subst
syncWriteContentDatabase env repo content = do
  tick <- beginTick 1
  writeDatabase env repo content tick

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

checkWritable :: Repo -> OpenDB s -> IO Writing
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
writeDatabase env repo WriteContent{..} latency =
  readDatabase env repo $ \odb lookup -> do
    writing <- checkWritable repo odb
    checkComplete env repo writeBatch
    Stats.bump (envStats env) Stats.mutatorLatency =<< endTick latency
    let !size = batchSize writeBatch

    tick env repo WriteTraceInput Stats.mutatorInput size $ do
      -- If nobody is writing to the DB just write the batch directly.
      --
      -- TODO: What if someone is already deduplicating another batch? Should we
      -- not write in that case?
      r <- tryWithMutex (wrLock writing) $ const $
        reallyWriteBatch
          env repo odb lookup writing size False writeBatch writeOwnership
      case r of
        Just cont -> cont
        Nothing ->
          -- Somebody is already writing to the DB - deduplicate the batch
          deDupBatch env repo odb lookup writing size writeBatch writeOwnership

reallyWriteBatch
  :: Storage.Storage s
  => Env
  -> Repo
  -> OpenDB s
  -> Lookup
  -> Writing
  -> Word64  -- ^ original size of the batch
  -> Bool  -- ^ has the batch already been de-duped?
  -> Thrift.Batch
  -> Maybe DefineOwnership
  -> IO (IO Subst)
reallyWriteBatch env repo OpenDB{..} lookup writing original_size deduped
    batch maybeOwn = do
  let !real_size = batchSize batch
  Stats.tick (envStats env) Stats.mutatorThroughput original_size
    $ Stats.tick (envStats env)
        (if deduped
          then Stats.mutatorDedupedThroughput
          else Stats.mutatorDupThroughput) real_size
    $ do

    next_id <- readIORef (wrNextId writing)

    (facts, subst) <-
      logExceptions (\s -> inRepo repo $ "rename error: " ++ s) $
        tick env repo WriteTraceRename Stats.renameThroughput real_size $ do
          let withBase f = do
                r <- readTVarIO (wrCommit writing)
                case r of
                  Nothing -> f lookup
                  Just (_, facts) -> withCanLookup (stacked lookup facts) f
          withBase $ \base -> do
            withLookupCache writing base $ \cache -> do
              FactSet.renameFacts
                (schemaInventory odbSchema)
                cache
                next_id
                batch
                def { trustRefs = deduped }

    logExceptions (\s -> inRepo repo $ "commit error: " ++ s) $ do
      updateLookupCacheStats env

      let
        commitOwnership = do
          Storage.addOwnership odbHandle $
            coerce (Subst.substIntervals subst) <$>
              Thrift.batch_owned batch
          let deps = substDependencies subst
                <$> Thrift.batch_dependencies batch
          derivedOwners <-
            if | Just owners <- maybeOwn -> do
                  Ownership.substDefineOwnership owners subst
                  return $ Just owners
               | not $ HashMap.null deps ->
                  makeDefineOwnership env repo deps
               | otherwise -> return Nothing
          forM_ derivedOwners $ \ownBatch ->
            Storage.addDefineOwnership odbHandle ownBatch

        doCommit =
          tick env repo WriteTraceCommit
            Stats.commitThroughput real_size $ do
              Storage.commit odbHandle facts
              commitOwnership

      -- we're going to perform the actual commit outside the write
      -- lock, so that the next batch can start renaming while we're
      -- committing. But the next rename will need the current batch
      -- to de-duplicate against, so we save it in wrCommit. There can
      -- only be one commit happening at a time and there's no way to
      -- queue more than one pending commit currently.
      commit <-
        if envMockWrites env
          then do release facts; return (return ())
          else do
            atomically $ do
               m <- readTVar (wrCommit writing)
               case m of
                 Just{} -> retry
                 Nothing -> writeTVar (wrCommit writing) (Just (next_id, facts))
            return $
              doCommit
                `finally`
              do atomically $ writeTVar (wrCommit writing) Nothing
                 withMutex (wrLock writing) $ const $ release facts

      new_next_id <- Lookup.firstFreeId facts
      atomicWriteIORef (wrNextId writing) new_next_id
      return (commit >> return subst)
        -- commit takes place outside the write lock

deDupBatch
  :: Storage.Storage s
  => Env
  -> Repo
  -> OpenDB s
  -> Lookup
  -> Writing
  -> Word64
  -> Thrift.Batch
  -> Maybe DefineOwnership
  -> IO Subst
deDupBatch env repo odb lookup writing original_size batch maybeOwn =
  logExceptions (\s -> inRepo repo $ "dedup error: " ++ s) $ do
    next_id <- do
      r <- readTVarIO (wrCommit writing)
      -- if there is a commit in progress, we use the ID boundary
      -- before that commit for the snapshot.
      case r of
        Just (commit_id, _) -> return commit_id
        _otherwise -> readIORef $ wrNextId writing
    (maybe_deduped_batch, dsubst) <- bracket
      (
        withLookupCache writing lookup $ \cache ->
        -- We need a snapshot here because we don't want lookups to
        -- return fact ids which conflict with ids in the renamed batch.
        Lookup.withSnapshot cache next_id $ \snapshot ->
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
        let !is = force $ coerce Subst.substIntervals dsubst
              <$> Thrift.batch_owned batch
            !deps = force $ substDependencies dsubst
              <$> Thrift.batch_dependencies batch
        forM_ maybeOwn $ \ownBatch ->
          Ownership.substDefineOwnership ownBatch dsubst
        -- And now write it do the DB, deduplicating again
        cont <- withMutex (wrLock writing) $ const $
          reallyWriteBatch env repo odb lookup writing original_size True
            deduped_batch
              { Thrift.batch_owned = is
              , Thrift.batch_dependencies = deps
              }
            maybeOwn
        wsubst <- cont
        return $ dsubst <> wsubst

withLookupCache
  :: Writing
  -> Lookup.Lookup
  -> (Lookup.Lookup -> IO a)
  -> IO a
withLookupCache Writing{..} lookup f = do
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

tick
  :: Env -> Repo -> WriteTraceEvent -> Stats.Bump Tick -> Word64 -> IO a -> IO a
tick env repo event ticker value cont =
  traceMsg (envTracer env) (GleanTraceWrite repo event value)
  $ Stats.tick (envStats env) ticker value cont

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
  ) where

import Control.Exception
import Control.Monad.Extra
import Control.Trace (traceMsg)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Default
import Data.Int (Int64)
import Data.IORef
import Data.Maybe
import Data.Word

import Util.Control.Exception
import Util.Log (logInfo)
import Util.STM

import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.Data as Data
import Glean.Database.Open
import Glean.Database.Exception
import Glean.Database.Meta (getACLMode, showACLMode, isACLEnabled)
import Glean.Database.AclKnobs (aclStoreEnabled)
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Schema
import Glean.Database.Trace
import Glean.Database.Types
import Glean.Database.Write.Queue
import Glean.FFI
import qualified Data.Text as Text
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
import Glean.RTS.Types (Pid(..), Fid(..))
import Glean.Types (Repo)
import qualified Glean.Types as Thrift
import Glean.Util.Metric
import Glean.Util.Mutex
import Glean.Util.Vector
import qualified Glean.Write.Stats as Stats

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
  -> Fid
  -> HashMap Int64 [Thrift.FactDependencies]
  -> IO (Maybe DefineOwnership)
makeDefineOwnership env repo nextId deps
  | HashMap.null deps = return Nothing
  | otherwise = do
  readDatabase env repo $ \odb lookup -> do
  maybeOwnership <- readTVarIO (odbOwnership odb)
  forM maybeOwnership $ \ownership -> do
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
  -- Read database properties to check ACL mode
  meta <- atomically $ Catalog.readMeta (envCatalog env) repo
  let dbAclEnabled = isACLEnabled (metaProperties meta)
  when (not $ HashMap.null batch_owned) $ do
    when (metaAxiomComplete meta) $
      throwIO $ Thrift.Exception
        "Attempting to write facts with ownership after 'glean complete'"
  when (not $ HashMap.null batch_dependencies) $ do
    -- TODO: enforce that not only the axiom predicates be complete
    -- but that every predicate depended upon be complete.
    when (not (metaAxiomComplete meta)) $
      throwIO $ Thrift.Exception
        "Attempting to write facts with dependencies before 'glean complete'"
  -- Check based on database ACL property, not server config
  when (dbAclEnabled && batch_count > 0 && HashMap.null batch_owned) $
    throwIO $ Thrift.Exception $ "ACLs are enabled for this database but " <>
      "batch contains no ownership data. Indexer must be run with " <>
      "--ownership to emit ownership information."
  -- When ACL storage is enabled, an ownership-bearing batch must carry an
  -- ACL config. We validate here -- before the write lock and renameFacts,
  -- and exactly once -- rather than in commitOwnership. An all-public batch
  -- must still send an explicit (possibly empty) config; only a missing
  -- config ('Nothing') indicates the write pipeline dropped it.
  storeAclsEnabled <- aclStoreEnabled
  when (dbAclEnabled && storeAclsEnabled
        && not (HashMap.null batch_owned)
        && isNothing batch_acl_config) $
    throwIO $ Thrift.Exception $ Text.concat
      [ "ACL assertion failed: database has ACLs enabled (glean.acl "
      , "property set) and the batch carries ownership data ("
      , Text.pack (show (HashMap.size batch_owned))
      , " units), but no ACL config was propagated with the batch "
      , "(batch_acl_config is Nothing). This usually indicates a bug in "
      , "the write pipeline (e.g. SendAndRebaseQueue not forwarding "
      , "batch_acl_config), or an indexer that does not set ACL config on "
      , "its batches."
      ]

-- | Validate and persist a batch's ACL config during the write.
--
-- When the database has ACLs enabled and the @store_acls@ knob is on, the
-- provided ACL config (@batch_acl_config@) is merged into the DB's
-- accumulated @path_acl_config@ (newer entries take precedence) and
-- persisted. An empty config map means \"every fact in this batch is
-- public\"; a non-empty map maps directory prefixes to the ACL groups that
-- may see them.
--
-- The case where @batch_acl_config@ is 'Nothing' (config dropped somewhere
-- in the write pipeline) is rejected earlier, in 'checkComplete', before the
-- write lock is taken. The ownership augmentation and the registration of
-- ACL group units happen later, at finalization (see
-- "Glean.Database.CompletePredicates" and "Glean.Database.ACLOwnership").
accumulateBatchACLConfig
  :: Storage.DatabaseOps db
  => Env
  -> Repo
  -> db
  -> Thrift.Batch
  -> IO ()
accumulateBatchACLConfig env repo odbHandle Thrift.Batch{..} = do
  dbMeta <- atomically $ Catalog.readMeta (envCatalog env) repo
  let dbAclEnabled = isACLEnabled (metaProperties dbMeta)

  logInfo $ "[Batch.hs] Received batch: facts=" ++
    show batch_count ++
    ", ownership_units=" ++ show (HashMap.size batch_owned) ++
    ", has_acl_config=" ++ show (isJust batch_acl_config) ++
    (case batch_acl_config of
       Just cfg -> " (" ++ show (HashMap.size cfg) ++ " ACL entries)"
       Nothing -> "") ++
    ", db_acl_enabled=" ++ show dbAclEnabled

  storeAclsEnabled <- aclStoreEnabled

  when (dbAclEnabled && storeAclsEnabled && not (HashMap.null batch_owned)) $
    -- 'Nothing' is rejected in checkComplete; here a provided (possibly
    -- empty == all-public) config is merged and persisted.
    forM_ batch_acl_config $ \batchACLConfig -> do
      let configKeys = take 5 $ HashMap.keys batchACLConfig
      logInfo $ inRepo repo $
        "[Batch.hs] ACL config prefixes (first 5): " ++ show configKeys

      existingPathConfig <- fromMaybe HashMap.empty <$>
        Data.retrievePathACLConfig odbHandle
      let mergedPathConfig = HashMap.union batchACLConfig existingPathConfig
      Data.storePathACLConfig odbHandle mergedPathConfig
      logInfo $ inRepo repo $
        "[Batch.hs] ACL config accumulated: " ++
        show (HashMap.size mergedPathConfig) ++ " total prefixes"

  when (dbAclEnabled && not storeAclsEnabled
    && not (HashMap.null batch_owned)) $
    logInfo $ inRepo repo
      "[Batch.hs] ACL storage skipped: store_acls knob is off"

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
    -- Log ACL mode for this write operation based on database property
    meta <- atomically $ Catalog.readMeta (envCatalog env) repo
    let aclMode = getACLMode (metaProperties meta)
    logInfo $ inRepo repo $ "[glean write] " ++ showACLMode aclMode
    Stats.bump (envStats env) Stats.mutatorLatency =<< endTick latency
    let !size = batchSize writeBatch

    tick env repo WriteTraceInput Stats.mutatorInput size $ do
      -- If nobody is writing to the DB just write the batch directly.
      --
      -- TODO: What if someone is already deduplicating another batch? Should we
      -- not write in that case?
      r <- tryWithMutexSafe (wrLock writing) $ \lock ->
        reallyWriteBatch
          env repo odb lock lookup writing size False writeBatch
          writeOwnership
      case r of
        Just cont -> cont
        Nothing ->
          -- Somebody is already writing to the DB - deduplicate the batch
          deDupBatch env repo odb lookup writing size writeBatch
            writeOwnership

reallyWriteBatch
  :: Env
  -> Repo
  -> OpenDB
  -> Storage.WriteLock w
  -> Lookup
  -> Writing
  -> Word64  -- ^ original size of the batch
  -> Bool  -- ^ has the batch already been de-duped?
  -> Thrift.Batch
  -> Maybe DefineOwnership
  -> IO (IO Subst)
reallyWriteBatch env repo OpenDB{..} lock lookup writing original_size deduped
    batch@Thrift.Batch{..} maybeOwn = do
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
          let apply v = unsafeCoerceVector <$>
                Subst.unsafeSubstIntervalsAndRelease subst
                  (unsafeCoerceVector v)
          owned <- mapM apply batch_owned
          Storage.addOwnership odbHandle lock owned
          deps <- mapM (substDependencies subst) batch_dependencies
          derivedOwners <-
            if | Just owners <- maybeOwn -> do
                  Ownership.substDefineOwnership owners subst
                  return $ Just owners
               | not $ HashMap.null deps ->
                  makeDefineOwnership env repo next_id deps
               | otherwise -> return Nothing
          forM_ derivedOwners $ \ownBatch ->
            Storage.addDefineOwnership odbHandle lock ownBatch

          accumulateBatchACLConfig env repo odbHandle batch

        doCommit =
          tick env repo WriteTraceCommit
            Stats.commitThroughput real_size $ do
              Storage.commit odbHandle facts

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
            commitOwnership -- must be done under the write lock
            atomically $ do
               m <- readTVar (wrCommit writing)
               case m of
                 Just{} -> retry
                 Nothing -> writeTVar (wrCommit writing) (Just (next_id, facts))
            return $
              doCommit
                `finally`
              do atomically $ writeTVar (wrCommit writing) Nothing
                 withMutexSafe (wrLock writing) $ const $ release facts

      new_next_id <- Lookup.firstFreeId facts
      atomicWriteIORef (wrNextId writing) new_next_id
      return (commit >> return subst)
        -- commit takes place outside the write lock

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
deDupBatch env repo odb lookup writing original_size
    batch@Thrift.Batch{..} maybeOwn =
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
        let apply v = unsafeCoerceVector <$>
              Subst.unsafeSubstIntervalsAndRelease dsubst
                (unsafeCoerceVector v)
        is <- mapM apply batch_owned
        deps <- mapM (substDependencies dsubst) batch_dependencies
        forM_ maybeOwn $ \ownBatch ->
          Ownership.substDefineOwnership ownBatch dsubst
        -- And now write it to the DB, deduplicating again
        cont <- withMutexSafe (wrLock writing) $ \lock ->
          reallyWriteBatch env repo odb lock lookup writing original_size True
            deduped_batch
              { Thrift.batch_owned = is
              , Thrift.batch_dependencies = deps
              , Thrift.batch_acl_config = batch_acl_config
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
 -> IO [Thrift.FactDependencies]
substDependencies subst dmap = mapM substFD dmap
  where
  substFD (Thrift.FactDependencies facts deps) = do
    Thrift.FactDependencies <$> apply facts <*> apply deps
  apply v =
    unsafeCoerceVector <$>
      Subst.substVector subst (unsafeCoerceVector v)

batchSize :: Thrift.Batch -> Word64
batchSize = fromIntegral . BS.length . Thrift.batch_facts

tick
  :: Env -> Repo -> WriteTraceEvent -> Stats.Bump Tick -> Word64 -> IO a -> IO a
tick env repo event ticker value cont =
  traceMsg (envTracer env) (GleanTraceWrite repo event value)
  $ Stats.tick (envStats env) ticker value cont

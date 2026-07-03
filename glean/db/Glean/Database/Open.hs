{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Open (
  usingActiveDatabase,
  withOpenDatabase, withOpenDatabaseStack, withWritableDatabase,
  readDatabase, readDatabaseWithBoundaries,
  asyncOpenDB,
  newDB, acquireDB, releaseDB,
  isDatabaseClosed,
  schemaUpdated,
  getDbSchemaVersion,
  updateLookupCacheStats,
  repoParent,
  repoParents,
  depParent
) where

import Control.Concurrent (modifyMVar_)
import Control.Concurrent.Async (Async, wait)
import Control.Exception hiding(try)
import Control.Monad.Catch (try)
import Control.Monad.Extra
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.IORef
import Data.List (sort)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
import Data.Word
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

import Util.Control.Exception
import qualified Util.Control.Exception.CallStack as CallStack
import Util.Log
import Util.Logger
import Util.STM

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter (Locality(..))
import Glean.Database.Data
  ( storeSchema, retrieveSchema
  , retrieveUnits, retrieveSlices, storeUnits, storeSlices
  , retrieveFirstACLID
  , retrieveACLGroupMapping )
import Glean.Database.Exception
import Glean.Database.Repo
import Glean.Database.Storage as Storage
import Glean.Database.Meta (Meta(..), getACLMode, ACLMode(..), showACLMode)
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Database.Types
import Glean.Database.Write.Queue
import Glean.Logger
import Glean.Query.Codegen (Boundaries, flatBoundaries, stackedBoundaries)
import Glean.Repo.Text
import qualified Glean.RTS.Foreign.Lookup as Lookup
import Glean.RTS.Foreign.Lookup (Lookup)
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import qualified Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Foreign.Ownership (UnitId(..), UsetId(..))
import qualified Glean.RTS.Foreign.Stacked as Stacked
import Glean.RTS.Types (Pid(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import qualified Glean.Internal.Types as Thrift
import qualified Glean.Types as Thrift
import Glean.Types (Repo, PredicateStats(..))
import Glean.Util.Mutex
import qualified Glean.Util.Observed as Observed
import Util.Time
import Glean.Util.Some
import qualified Glean.Util.Warden as Warden
import qualified Glean.Write.Stats as Stats

withOpenDatabase
  :: HasCallStack
  => Env
  -> Repo
  -> (OpenDB -> IO a)
  -> IO a
withOpenDatabase env@Env{..} repo action =
  withActiveDatabase env repo $ \db@DB{..} -> do
    odb <- mask $ \restore -> do
      r <- atomically $ do
        state <- readTVar dbState
        case state of
          Opening -> retry
          Open odb -> return $ Left odb
          Closing -> retry
          Closed -> do
            meta <- Catalog.readMeta envCatalog dbRepo
            let version = Thrift.metaVersion meta
                mode
                  | envReadOnly = ReadOnly
                  | Thrift.Complete{} <- completeness = ReadOnly
                  | otherwise = ReadWrite
                    -- Note: Finalizing also needs to be ReadWrite,
                    -- because compaction modifies the DB.
                  where completeness = metaCompleteness meta
            writeTVar dbState Opening
            return $ Right (version, mode)
      case r of
        Left odb -> return odb
        Right (version, mode) -> do
          meta <- atomically $ Catalog.readMeta envCatalog dbRepo
          let
            deps = metaDependencies meta
            onFailure ex = atomically $ Catalog.dbFailed envCatalog dbRepo ex
          -- opening a DB has long uninterruptible sections so do it on a
          -- separate thread in case we get cancelled
          withStorageFor env repo meta $ \storage -> do
            opener <-
              asyncOpenDB env storage db version mode deps (return ())
                onFailure
            restore $ wait opener
    action odb `finally` do
      t <- getTimePoint
      atomically $ writeTVar (odbIdleSince odb) t

-- | Runs the action on each DB in the stack, in top-to-bottom order
withOpenDatabaseStack
  :: Env
  -> Repo
  -> (OpenDB -> IO a)
  -> IO [a]
withOpenDatabaseStack env repo action = do
  parents <- repoParents env repo
  mapM (\repo -> withOpenDatabase env repo action) (repo : parents)

repoParents :: Env -> Repo -> IO [Repo]
repoParents Env{..} repo = go repo
  where
  go repo = do
    deps <- atomically $ metaDependencies <$> Catalog.readMeta envCatalog repo
    case deps of
      Nothing -> return []
      Just dep -> (parent :) <$> go parent
        where parent = depParent dep

repoParent :: Env -> Repo -> IO (Maybe Repo)
repoParent Env{..} repo = do
  deps <- atomically $ metaDependencies <$> Catalog.readMeta envCatalog repo
  return (fmap depParent deps)

depParent :: Thrift.Dependencies -> Repo
depParent deps = case deps of
  Thrift.Dependencies_pruned Thrift.Pruned{..} -> pruned_base
  Thrift.Dependencies_stacked Thrift.Stacked{..} ->
    Thrift.Repo stacked_name stacked_hash

withOpenDBLookup
  :: Env
  -> Repo
  -> OpenDB
  -> Maybe [Text]  -- ^ ACL group names, or Nothing to disable ACL filtering
  -> (Boundaries -> Lookup -> IO a)
  -> IO a
withOpenDBLookup env repo odb@OpenDB{ odbBaseSlices = baseSlices, .. }
    aclGroupNames f =
  Lookup.withCanLookup odbHandle $ \lookup -> do
  parent <- repoParent env repo
  case parent of
    Nothing -> do
      bounds <- flatBoundaries lookup
      withFlatACLLookup env repo odb aclGroupNames lookup $ \sliced ->
        f bounds sliced
    Just baseRepo ->
      withOpenDatabase env baseRepo $ \baseOdb@OpenDB{odbHandle = baseHandle} ->
        Lookup.withCanLookup baseHandle $ \baseLookup ->
          withOpenDBStack env baseRepo baseLookup $ \base -> do
            bounds <- stackedBoundaries base lookup
            let ownershipSlices = catMaybes baseSlices
            withStackedACLLookup env repo odb baseRepo baseOdb aclGroupNames
              ownershipSlices base lookup $ \stackedLookup ->
              f bounds stackedLookup

withOpenDBStack
  :: Env
  -> Repo  -- Stacked DB
  -> Lookup  -- Stacked DB
  -> (Lookup -> IO a)
  -> IO a
withOpenDBStack env repo lookup f = do
  parent <- repoParent env repo
  case parent of
    Nothing -> f lookup
    Just baseRepo ->
      withOpenDatabase env baseRepo $ \OpenDB{..} ->
        Lookup.withCanLookup odbHandle $ \baseLookup ->
        withOpenDBStack env baseRepo baseLookup $ \base -> do
          Lookup.withCanLookup (Stacked.stacked base lookup) f

withWritableDatabase
  :: Env
  -> Repo
  -> ((WriteQueue, DbSchema, Some DatabaseOps) -> IO a) -> IO a
withWritableDatabase env repo action =
  withOpenDatabase env repo $ \OpenDB{..} -> case odbWriting of
    Just Writing{..} -> action (wrQueue, odbSchema, odbHandle)
    Nothing -> dbError repo "can't write to a read-only database"

readDatabase
  :: Env
  -> Repo
  -> (OpenDB -> Lookup.Lookup -> IO a)
  -> IO a
readDatabase env repo f =
  readDatabaseWithBoundaries env repo Nothing $ \odb _ lookup ->
  f odb lookup

readDatabaseWithBoundaries
  :: Env
  -> Repo
  -> Maybe [Text]  -- ^ ACL group names, or Nothing to disable ACL filtering
  -> (OpenDB -> Boundaries -> Lookup -> IO a)
  -> IO a
readDatabaseWithBoundaries env repo aclGroupNames f =
  withOpenDatabase env repo $ \odb ->
  withOpenDBLookup env repo odb aclGroupNames $ \bounds lookup ->
    f odb bounds lookup

-- -----------------------------------------------------------------------------
-- ACL Slice Construction

-- | Resolve ACL group names to their unit ids in this DB. Each group is stored
-- as an "acl:<name>" ownership unit; names that don't resolve are dropped.
resolveAclGroupUnits
  :: Storage.DatabaseOps db => db -> [Text] -> IO [Word32]
resolveAclGroupUnits handle names =
  fmap catMaybes $ forM names $ \name -> do
    let unitName = TextE.encodeUtf8 ("acl:" <> name)
    mId <- Storage.getUnitId handle unitName
    return $ fmap (\(UnitId uid) -> uid) mId

-- | Read a DB's ACL metadata at open time: the firstACLID boundary and the
-- group name -> UsetId mapping. Both are DB-invariant; empty/Nothing for
-- DBs without ACLs.
loadAclMetadata
  :: Storage.DatabaseOps db
  => db
  -> IO (Maybe UsetId, Map.Map Text UsetId)
loadAclMetadata handle = do
  mFirstACLID <- retrieveFirstACLID handle
  mMapping <- retrieveACLGroupMapping handle
  let aclMapping = case mMapping of
        Nothing -> Map.empty
        Just parsed -> Map.fromList
          [ (name, UsetId uid)
          | (name, uid) <- HashMap.toList parsed ]
  return (mFirstACLID, aclMapping)

-- | Build an ACL-aware ownership slice for a single DB layer, and pass it to
-- a continuation. Passes Nothing if ACL filtering is not needed for this layer.
--
-- The slice makes file units (non-ACL units) visible by default, and ACL
-- groups invisible unless they match the query's ACL group names.
--
-- We build the slice in EXCLUDE mode: the units we pass are the ACL-group
-- units the caller is NOT a member of. Everything else (file
-- units, and the caller's own groups) is visible by default, so a fact is
-- hidden exactly when its ACL CNF requires a group the caller lacks.
--
-- The full set of ACL-group units is the contiguous range
-- [firstACLID, firstACLID + #groups) (see registerACLUnits); we
-- subtract the caller's own groups (resolved via Storage.getUnitId) and
-- exclude the rest.
buildLayerACLSlice
  :: Env
  -> Repo
  -> OpenDB
  -> Maybe [Text]  -- ^ ACL group names, or Nothing to disable ACL filtering
  -> (Maybe Ownership.Slice -> IO a)
  -> IO a
buildLayerACLSlice _env _repo _odb Nothing f = do
  vlog 1 "[ACL-Slice] ACL filtering disabled, skipping ACL slice"
  f Nothing
buildLayerACLSlice _env repo odb (Just aclGroupNames) f
  -- Not an ACL DB: there is nothing to filter, every fact is public. This
  -- is the only case where dropping the slice (serving all facts) is safe.
  | odbACLMode odb == ACLDisabled = do
      vlog 1 "[ACL-Slice] ACL mode disabled, skipping ACL slice"
      f Nothing
  -- ACL-enforcing layer. firstACLID and ownership are established at
  -- completion for every ACL-enabled DB (registerACLUnits + storeOwnership),
  -- so their absence here is an inconsistent DB, not a non-ACL one. Fail
  -- CLOSED: error out rather than silently serve unfiltered facts, which
  -- would be an ACL bypass.
  | otherwise =
      case odbFirstACLID odb of
        Nothing ->
          dbError repo $
            "ACL-enabled DB has no firstACLID; refusing to serve "
              <> "unfiltered results (fail closed)"
        Just (UsetId firstACL) -> do
          maybeOwnership <- readTVarIO (odbOwnership odb)
          case maybeOwnership of
            Nothing ->
              dbError repo $
                "ACL-enabled DB has no ownership data; refusing to serve "
                  <> "unfiltered results (fail closed)"
            Just ownership -> do
              -- All ACL-group units occupy the contiguous range
              -- [firstACLID, firstACLID + #groups) above every regular
              -- file unit (see registerACLUnits at completion).
              let numGroups = Map.size (odbACLMapping odb)
                  allGroups
                    | numGroups == 0 = []
                    | otherwise =
                        [firstACL .. firstACL + fromIntegral numGroups - 1]
              userGroups <- resolveAclGroupUnits (odbHandle odb) aclGroupNames
              vlog 1 $ "[ACL-Slice] (exclude) allGroups="
                ++ show (length allGroups)
                ++ ", userGroups=" ++ show (length userGroups)
              -- Exclude (in EXCLUDE mode) the ACL-group units the caller is
              -- NOT a member of; everything else stays visible by default.
              let userSet = Set.fromList userGroups
                  excludeUnits = coerce
                    (sort (filter (`Set.notMember` userSet) allGroups))
                    :: [UnitId]
              aclSlice <- Ownership.slice ownership [] excludeUnits True
              vlog 1 "[ACL-Slice] Slice created successfully"
              f (Just aclSlice)

-- | Wrap a lookup with the given ownership/ACL slices, or pass it through
-- unchanged when there are none. An empty list must NOT be handed to
-- slicedStack: an empty Slices treats every UsetId as not-covered and hides
-- all facts.
withMaybeSliced
  :: [Ownership.Slice] -> Lookup -> (Lookup -> IO a) -> IO a
withMaybeSliced [] lk k = k lk
withMaybeSliced slices lk k =
  Lookup.withCanLookup (Ownership.slicedStack slices lk) k

-- | Apply the ACL slice (if any) for a flat (non-stacked) database and
-- run the continuation with the resulting (possibly sliced) lookup.
withFlatACLLookup
  :: Env
  -> Repo
  -> OpenDB
  -> Maybe [Text] -- ^ ACL group names, or Nothing to disable ACL filtering
  -> Lookup     -- ^ the layer's lookup
  -> (Lookup -> IO a)
  -> IO a
withFlatACLLookup env repo odb aclGroupNames lookup f =
  buildLayerACLSlice env repo odb aclGroupNames $ \mAclSlice ->
    withMaybeSliced (maybeToList mAclSlice) lookup f

-- | Build ACL slices for all layers in a stacked DB (base layers only).
-- Walks the DB stack recursively and builds an ACL slice for each layer
-- that has ACL data (firstACLID). Returns the list of ACL slices.
withStackedACLSlices
  :: Env
  -> Repo       -- ^ Base repo (immediate parent)
  -> OpenDB     -- ^ Base OpenDB
  -> Maybe [Text] -- ^ ACL group names, or Nothing to disable ACL filtering
  -> ([Ownership.Slice] -> IO a)
  -> IO a
withStackedACLSlices env baseRepo baseOdb aclGroupNames f = do
  -- Build ACL slice for this base layer
  buildLayerACLSlice env baseRepo baseOdb aclGroupNames $ \mSlice -> do
    -- Recurse to deeper layers
    parent <- repoParent env baseRepo
    case parent of
      Nothing -> f (maybeToList mSlice)
      Just parentRepo ->
        withOpenDatabase env parentRepo $ \parentOdb ->
          withStackedACLSlices env parentRepo parentOdb aclGroupNames
            $ \parentSlices ->
            f (maybeToList mSlice ++ parentSlices)

-- | Apply ownership + ACL slices for a stacked database and run the
-- continuation with the combined stacked lookup. This wires together the
-- base ACL slices (walked over the base stack), the top-layer ACL slice,
-- and the pre-existing ownership slices into a single sliced/stacked lookup.
withStackedACLLookup
  :: Env
  -> Repo               -- ^ Top (stacked) repo
  -> OpenDB             -- ^ Top OpenDB
  -> Repo               -- ^ Base repo (immediate parent)
  -> OpenDB             -- ^ Base OpenDB
  -> Maybe [Text]       -- ^ ACL group names; Nothing disables ACL filtering
  -> [Ownership.Slice]  -- ^ Ownership slices for the base stack
  -> Lookup             -- ^ Base stack lookup
  -> Lookup             -- ^ Top layer lookup
  -> (Lookup -> IO a)
  -> IO a
withStackedACLLookup env repo odb baseRepo baseOdb aclGroupNames
    ownershipSlices base lookup f =
  -- Build ACL slices for base layers (walk the base stack)
  withStackedACLSlices env baseRepo baseOdb aclGroupNames $ \baseACLSlices ->
  -- Build ACL slice for the top layer
  buildLayerACLSlice env repo odb aclGroupNames $ \mTopACLSlice ->
    -- Slice each side only if it has slices (empty would hide everything),
    -- then stack the (possibly sliced) base under the top layer.
    withMaybeSliced (ownershipSlices ++ baseACLSlices) base $ \slicedBase ->
    withMaybeSliced (maybeToList mTopACLSlice) lookup $ \slicedTop ->
    Lookup.withCanLookup (Stacked.stacked slicedBase slicedTop) f

newDB :: Repo -> STM DB
newDB repo = DB repo
  <$> newTVar Closed
  <*> newTVar 0

acquireDB :: DB -> STM ()
acquireDB db = modifyTVar' (dbUsers db) (+1)

-- | MUST be paired with 'acquireDB'
releaseDB
  :: Catalog.Catalog
  -> TVar (HashMap Thrift.Repo DB)
  -> DB
  -> STM ()
releaseDB catalog active DB{..} = do
  users <- readTVar dbUsers
  writeTVar dbUsers $! users - 1
  when (users == 1) $ do
    state <- readTVar dbState
    case state of
      Closed -> do
        meta <- try $ Catalog.readMeta catalog dbRepo
        case meta :: Either SomeException Meta of
          Right Meta{metaCompleteness = Thrift.Incomplete{}} -> return ()
          _ -> modifyTVar' active $ HashMap.delete dbRepo
      _ -> return ()

withActiveDatabase
  :: HasCallStack
  => Env
  -> Repo
  -> (DB -> IO a)
  -> IO a
withActiveDatabase Env{..} repo act = bracket
  (atomically $ do
    r <- HashMap.lookup repo <$> readTVar envActive
    db <- case r of
      Just db -> return db
      Nothing -> do
        exists <- Catalog.exists envCatalog [Local] repo
        when (not exists) $ CallStack.throwSTM $ Thrift.UnknownDatabase repo
        deleting <- HashMap.member repo <$> readTVar envDeleting
        -- TODO: different error?
        when deleting $ CallStack.throwSTM $ Thrift.UnknownDatabase repo
        db <- newDB repo
        modifyTVar' envActive $ HashMap.insert repo db
        return db
    acquireDB db
    return db)
  (atomically . releaseDB envCatalog envActive)
  (\db -> act db)

usingActiveDatabase
  :: Env
  -> Repo
  -> (Maybe DB -> IO a)
  -> IO a
usingActiveDatabase Env{..} repo = bracket
  (atomically $ do
    r <- HashMap.lookup repo <$> readTVar envActive
    mapM_ acquireDB r
    return r)
  (atomically . mapM_ (releaseDB envCatalog envActive))

withMaybeActiveDatabase
  :: Env
  -> Repo
  -> (Maybe DB -> STM a)
  -> STM a
withMaybeActiveDatabase Env{..} repo fn = do
  active <- readTVar envActive
  fn (HashMap.lookup repo active)

updateLookupCacheStats :: Env -> IO ()
updateLookupCacheStats env =
  Stats.bump (envStats env) Stats.lookupCacheStats
  =<< LookupCache.readStatsAndResetCounters (envLookupCacheStats env)

setupSchema
  :: DatabaseOps db
  => Env
  -> Repo
  -> db
  -> Mode
  -> IO DbSchema
setupSchema Env{..} _ handle (Create _ _ initial _) = do
  schema <- Observed.get envSchemaSource
  dbSchema <- case initial of
    UseDefaultSchema ->
      newDbSchema (Just envDbSchemaCache) schema
        LatestSchema readWriteContent envDebug
    UseSpecificSchema schemaId ->
      newDbSchema (Just envDbSchemaCache) schema
        (SpecificSchemaId schemaId) readWriteContent envDebug
    UseThisSchema info ->
      fromStoredSchema (Just envDbSchemaCache) info readWriteContent envDebug
  storeSchema handle $ toStoredSchema dbSchema
  return dbSchema
setupSchema env@Env{..} repo handle mode = do
  stored <- retrieveSchema repo handle
  case stored of
    Just info
      | ReadOnly <- mode -> mergeSchema
      | otherwise ->
        fromStoredSchema (Just envDbSchemaCache) info readWriteContent envDebug
          -- while writing, we don't allow new predicates to be added to
          -- the schema. This is the easiest way to prevent facts being
          -- added to the DB that aren't in the original stored schema.
          -- It might be possible to do something safe here, e.g. only
          -- merge non-stored derived predicates. (TODO)
      where
      stackStats :: IO (HashMap Pid PredicateStats)
      stackStats = do
        parents <- repoParents env repo
        statss <- forM parents $ \repo ->
          withOpenDatabase env repo $ \OpenDB{..} ->
            Storage.predicateStats odbHandle
        stats <- Storage.predicateStats handle
        return $ HashMap.fromListWith (<>) $ concat (stats : statss)

      mergeSchema = do
        -- merge the schema in the DB with the current schema, so
        -- that we can query for derived predicates that weren't
        -- stored in the DB when it was created.
        schema <- Observed.get envSchemaSource
        stats <- stackStats
        newMergedDbSchema (Just envDbSchemaCache) info schema
          (readOnlyContent stats) envDebug
    Nothing -> do
      meta <- atomically $ Catalog.readMeta envCatalog repo
      let failure = case metaCompleteness meta of
            Thrift.Broken (Thrift.DatabaseBroken task reason) ->
              Text.unpack $ Text.unwords $
                [ "Broken:" ] ++
                [ task <> ":" | not (Text.null task) ] ++
                [ reason ]
            _ -> ""
      dbError repo $ "DB has no stored schema. " <> failure

-- | Update the schema for all open DBs when the prevailing schema has
-- changed.
--
-- Preconditions:
--
--  * when schemaUpdated is invoked, calls to Observed.get on
--    envSchemaSource are guaranteed to get the updated schema.
--
--  * schemaUpdated does not run concurrently with itself.
--
schemaUpdated
  :: Env
  -> Maybe Repo
       -- ^ Just repo => only update the schema for this repo,
       -- otherwise update all of them.
  -> IO ()
schemaUpdated env@Env{..} mbRepo = do
  let
    just = case mbRepo of
      Nothing -> HashMap.elems
      Just repo -> maybeToList . HashMap.lookup repo
    acquire = atomically $ do
      active <- just <$> readTVar envActive
      mapM_ acquireDB active
      return active
    release = atomically . mapM_ (releaseDB envCatalog envActive)

  -- Empty the DbSchema cache. We probably have a new SchemaIndex now
  -- which invalidates all the entries anyway, and also we don't prune
  -- this cache anywhere else.
  when (isNothing mbRepo) $ do
    modifyMVar_ envDbSchemaCache $ \_ -> return HashMap.empty
    -- previously failed DBs might now open successfully
    atomically $ Catalog.resetFailed envCatalog

  bracket acquire release $ \active -> do
    forM_ active $ \DB{..} -> do
      maybeOpenDB <- atomically $ do
        state <- readTVar dbState
        case state of
          Opening -> retry
          Open odb -> return $ Just odb
          Closing -> return Nothing
          Closed -> return Nothing
      forM_ maybeOpenDB $ \OpenDB{..} -> do
        case odbWriting of
          Just{} -> do
            logInfo $ "not updating schema for writable DB: " <>
              showRepo dbRepo
            return ()
            -- see setupSchema above, we don't update the schema for
            -- a writable DB.
          Nothing -> do
            logInfo $ "updating schema for: " <> showRepo dbRepo
            r <- tryAll $
              loggingAction
                (runLogRepo "update-schema" env dbRepo) (const mempty) $
                  setupSchema env dbRepo odbHandle ReadOnly
            case r of
              Left err -> logError $ "schema update for " <> showRepo dbRepo <>
                " failed: " <> show err
              Right schema -> atomically $ do
                state <- readTVar dbState
                case state of
                  Opening -> return ()
                   -- if we are Opening now, this must have happened
                   -- after the transaction above, so it will already
                   -- pick up the new schema.
                  Open odb ->
                    writeTVar dbState $ Open odb { odbSchema = schema }
                  Closing -> return ()
                  Closed -> return ()
  logInfo "done updating schema for open DBs"


setupWriting :: Lookup.CanLookup lookup => Env -> lookup -> IO Writing
setupWriting Env{..} lookup = do
  scfg <- Observed.get envServerConfig
  -- Convert to Word64 from Int32 to prevent silent truncation.
  let cache_limit_mb :: Word64 = fromIntegral $
        ServerConfig.config_db_lookup_cache_limit_mb scfg
  lookupCache <- LookupCache.new
    (fromIntegral$ cache_limit_mb * 1024 * 1024)
    (fromIntegral $ ServerConfig.config_db_writer_threads scfg)
    envLookupCacheStats
  next_id <- newIORef =<< Lookup.firstFreeId lookup
  mutex <- newMutex (Storage.WriteLock ())
  queue <- WriteQueue <$> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0
    <*> newTVarIO 0 <*> newTVarIO 0
  anchorName <- newTVarIO Nothing
  commit <- newTVarIO Nothing
  return Writing
    { wrLock = mutex
    , wrNextId = next_id
    , wrLookupCache = lookupCache
    , wrLookupCacheAnchorName = anchorName
    , wrQueue = queue
    , wrCommit = commit
    }

-- | Open a database asynchronously, returning an 'Async' that can be waited on.
-- This is the only way to open a database since this operation has
-- uninterruptible chunks which might take a long time.
--
-- NOTE: This should be called with 'dbState' set to 'Opening' and async
-- exceptions masked.
asyncOpenDB
  :: Storage s
  => Env
  -> s
  -> DB
  -> DBVersion
  -> Mode
  -> Maybe Thrift.Dependencies
  -> IO ()
      -- ^ Action to run on success before setting the db status to
      -- 'Open'. If this fails, we will call the failure action.
  -> (SomeException -> IO ())
      -- ^ Action to run on any failure.
  -> IO (Async OpenDB)
asyncOpenDB env@Env{..} storage db@DB{..} version mode deps
    on_success on_failure =
  -- Be paranoid about 'spawnMask' itself throwing.
  handling_failures $ Warden.spawnMask envWarden $ \restore ->
  loggingAction (runLogRepo "open" env dbRepo) (const mempty) $ do
  when (not $ canOpenVersion storage mode version) $
    dbError dbRepo $
      "can't open database version " ++ show (unDBVersion version)
  bracket_
    (atomically $ acquireDB db)
    (atomically $ releaseDB envCatalog envActive db) $ do
  handling_failures $ do
    logInfo $ inRepo dbRepo "opening"
    bracketOnError
      (Storage.open storage dbRepo mode version)
      Storage.close
      $ \handle -> do
          odb <- restore $ do
            logInfo $ inRepo dbRepo "opened"
            dbSchema <- setupSchema env dbRepo handle mode
            logInfo $ inRepo dbRepo $
              "schema has " ++ show (schemaSize dbSchema) ++ " predicates"
            writing <- case mode of
              ReadOnly -> return Nothing
              _ -> Just <$> setupWriting env handle
            maybeSlices <- case mode of
              Create{} -> do
                let units = case deps of
                      Just (Thrift.Dependencies_pruned Thrift.Pruned{..}) ->
                        pruned_units
                      _ -> []
                storeUnits handle units
                slices <- baseSlices env deps (Just units)
                storeSlices handle (catMaybes slices)
                return slices
              _ -> do
                m <- retrieveSlices dbRepo handle
                case m of
                  Nothing -> do
                    stored_units <- retrieveUnits dbRepo handle
                    baseSlices env deps stored_units
                  Just slices -> return $ map Just slices
            idle <- newTVarIO =<< getTimePoint
            ownership <- newTVarIO =<< Storage.getOwnership handle
            -- Get database properties to check ACL mode
            meta <- atomically $ Catalog.readMeta envCatalog dbRepo
            let aclMode = getACLMode (metaProperties meta)
            -- Log ACL metadata on database open
            logInfo $ inRepo dbRepo $
              "[Open.hs] Database opening, ACL mode: " ++ showACLMode aclMode
            (mFirstACLID, aclMapping) <- loadAclMetadata handle
            on_success
            return OpenDB
              { odbHandle = Some handle
              , odbWriting = writing
              , odbSchema = dbSchema
              , odbIdleSince = idle
              , odbBaseSlices = maybeSlices
              , odbOwnership = ownership
              , odbACLMode = aclMode
              , odbACLMapping = aclMapping
              , odbFirstACLID = mFirstACLID
              }
          atomically $ writeTVar dbState $ Open odb
          enqueueBatchDescriptorsFromDatabase env db odb
          return odb
  where
    handling_failures :: IO a -> IO a
    handling_failures = handle $ \exc -> do
      atomically (writeTVar dbState Closed)
      logError $ inRepo dbRepo "couldn't open: " ++ show (exc :: SomeException)
      on_failure exc
      throwIO exc

-- | Re-enqueue all unprocessed batch descriptors which were enqueued before
-- but never materialised and written to the DB
enqueueBatchDescriptorsFromDatabase :: Env -> DB -> OpenDB -> IO ()
enqueueBatchDescriptorsFromDatabase env DB{..} OpenDB{..} =
  case odbWriting of
    Just Writing{..} -> do
      descriptors <- Storage.getUnprocessedBatchDescriptors odbHandle
      forM_ descriptors $ \descriptor ->
        enqueueBatchDescriptorWithResultHandling env dbRepo wrQueue
          odbSchema odbHandle descriptor True
    Nothing -> return ()

-- | Fetch the glean.schema_id property of a DB, if it has one. This
-- property is used to resolve queries.
getDbSchemaVersion
  :: Env
  -> Thrift.Repo
  -> IO (Maybe Thrift.SchemaId)
getDbSchemaVersion env repo = do
  props <- atomically $ Thrift.metaProperties <$>
    Catalog.readMeta (envCatalog env) repo
  return $ case HashMap.lookup "glean.schema_id" props of
    Just txt -> Just (Thrift.SchemaId txt)
    _otherwise -> Nothing

type Unit = ByteString
  -- why is pruned_units a list<binary> instead of list<UnitName>?

-- | Create the slices for a DB stack.
--
-- A stack has a set of units that are pruned, so we need to create a
-- slice for each DB in the stack that hides those units. As we go
-- down the stack, we pick up more units to exclude.
--
-- For each sub-stack we already have a set of slices for it
-- (odbBaseSlices in OpenDB), and this will be sufficient for the
-- current stack if we are excluding a subset of the units already
-- excluded by the stack.
--
-- Complicating this all somewhat is that a stack may specify the
-- units to *include* rather than *exclude*.
--
baseSlices
  :: Env
  -> Maybe Thrift.Dependencies
  -> Maybe [Unit] -- ^ units stored in the DB
  -> IO [Maybe Ownership.Slice]
baseSlices env deps stored_units = case deps of
  Nothing -> return []
  Just (Thrift.Dependencies_stacked Thrift.Stacked{..}) ->
    dbSlices (Thrift.Repo stacked_name stacked_hash) Set.empty True
  Just (Thrift.Dependencies_pruned Thrift.Pruned{..}) -> do
    let real_units = fromMaybe pruned_units stored_units
    dbSlices pruned_base (Set.fromList real_units) pruned_exclude
 where
  dbSlices
    :: Repo
       -- ^ DB (stack) to slice
    -> Set Unit
       -- ^ Pruned units
    -> Bool
       -- ^ True <=> exclude, False <=> include
    -> IO [Maybe Ownership.Slice]
  dbSlices repo units exclude = do
    withOpenDatabase env repo $ \OpenDB{..} -> do
      baseDeps <- atomically $ metaDependencies <$>
        Catalog.readMeta (envCatalog env) repo
      baseUnits <- retrieveUnits repo odbHandle
      rest <- depSlices baseDeps baseUnits units exclude odbBaseSlices
      slice <- if exclude && Set.null units
        then return Nothing
        else do
          let !n = Set.size units
          vlog 1 $ "computing slice for " <> showRepo repo <> " with " <>
            show n <>
            (if exclude then " excluded: " else " included: ") <>
            show (Set.toList units)
          unitIds <- fmap catMaybes $
            forM (Set.toList units) $ \name -> do
              id <- Storage.getUnitId odbHandle name
              vlog 2 $ "unit: " <> show name <> " = " <> show id
              return id
          maybeOwnership <- readTVarIO odbOwnership
          r <- forM maybeOwnership $ \ownership ->
            Ownership.slice ownership (catMaybes rest) unitIds exclude
          logInfo $ "completed " <> show n <> " slice for " <> showRepo repo
          return r
      return (slice : rest)

  -- | Create the slices for a stack of dependencies
  depSlices
    :: Maybe Thrift.Dependencies
    -> Maybe [Unit] -- ^ units stored in the DB
    -> Set Unit
    -> Bool
    -> [Maybe Ownership.Slice]
       -- ^ slices for this dependency, if we already have them
    -> IO [Maybe Ownership.Slice]
  depSlices dep dep_units units exclude slices = do
    case dep of
      Nothing -> return []
      Just (Thrift.Dependencies_stacked Thrift.Stacked{..}) ->
        dbSlices (Thrift.Repo stacked_name stacked_hash) units exclude
      Just (Thrift.Dependencies_pruned Thrift.Pruned{..})
        -- optimisation: check if the slices for the stack will be the
        -- same as the slices we already have for this repo. TODO:
        -- could handle more of the exclude/pruned_exclude combos here
        | exclude && pruned_exclude,
          units `Set.isSubsetOf` depUnits ->
          return slices
        | otherwise ->
          dbSlices pruned_base newUnits (exclude && pruned_exclude)
        where
        depUnits = Set.fromList $ fromMaybe pruned_units dep_units
          -- prefer the units stored in the DB
        newUnits
          | exclude && pruned_exclude =
            units `Set.union` depUnits
          | not exclude && pruned_exclude =
            units `Set.difference` depUnits
          | exclude && not pruned_exclude =
            depUnits `Set.difference` units
          | otherwise {- not exclude && not pruned_exclude -} =
            units `Set.intersection` depUnits

isDatabaseClosed :: Env -> Repo -> STM Bool
isDatabaseClosed env repo =
  withMaybeActiveDatabase env repo $ \r ->
    case r of
      Just db -> do
        st <- readTVar $ dbState db
        case st of
          Closed -> return True
          _ -> return False
      _ -> return True

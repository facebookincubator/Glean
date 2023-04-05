{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Open (
  lookupActiveDatabase, withActiveDatabase, usingActiveDatabase,
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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import Data.Word
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

import Util.Control.Exception (tryAll)
import qualified Util.Control.Exception.CallStack as CallStack
import Util.Log
import Util.Logger
import Util.STM
import Util.Text

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter (Locality(..))
import Glean.Database.Data
import Glean.Database.Exception
import Glean.Database.Repo
import qualified Glean.Database.Stats as Stats
import Glean.Database.Storage as Storage
import Glean.Database.Meta (Meta(..))
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Database.Types
import Glean.Logger
import Glean.Query.Codegen (Boundaries, flatBoundaries, stackedBoundaries)
import Glean.Repo.Text
import qualified Glean.RTS.Foreign.Lookup as Lookup
import Glean.RTS.Foreign.Lookup (Lookup)
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import qualified Glean.RTS.Foreign.Ownership as Ownership
import qualified Glean.RTS.Foreign.Stacked as Stacked
import Glean.RTS.Types (Pid(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types (Repo)
import qualified Glean.Internal.Types as Thrift
import qualified Glean.Types as Thrift
import Glean.Types (PredicateStats(..))
import Glean.Util.Mutex
import qualified Glean.Util.Observed as Observed
import Glean.Util.Time
import qualified Glean.Util.Warden as Warden

withOpenDatabase :: HasCallStack => Env -> Repo -> (OpenDB -> IO a) -> IO a
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
            when (not $ canOpenVersion mode version) $ dbError dbRepo
              $ "can't open database version " ++ show (unDBVersion version)
            writeTVar dbState Opening
            return $ Right (version, mode)
      case r of
        Left odb -> return odb
        Right (version, mode) -> do
          deps <- atomically $ metaDependencies <$>
            Catalog.readMeta envCatalog dbRepo
          let
            onFailure ex = atomically $ do
              openFailed <- readTVar envOpenFailed
              writeTVar envOpenFailed $ HashMap.insert dbRepo ex openFailed
          -- opening a DB has long uninterruptible sections so do it on a
          -- separate thread in case we get cancelled
          opener <-
            asyncOpenDB env db version mode deps (return ()) onFailure
          restore $ wait opener
    action odb `finally` do
      t <- getTimePoint
      atomically $ writeTVar (odbIdleSince odb) t

-- | Runs the action on each DB in the stack, in top-to-bottom order
withOpenDatabaseStack :: Env -> Repo -> (OpenDB -> IO a) -> IO [a]
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
  Thrift.Dependencies_stacked Thrift.Stacked{..} -> Thrift.Repo stacked_name stacked_hash

withOpenDBLookup
  :: Env
  -> Repo
  -> OpenDB
  -> (Boundaries -> Lookup -> IO a)
  -> IO a
withOpenDBLookup env repo OpenDB{ odbBaseSlices = baseSlices, .. } f =
  Lookup.withCanLookup odbHandle $ \lookup -> do
  parent <- repoParent env repo
  case parent of
    Nothing -> do
      bounds <- flatBoundaries lookup
      f bounds lookup
    Just baseRepo ->
      withOpenDatabase env baseRepo $ \OpenDB{..} -> do
        -- stacked (sliced base lookup)
        Lookup.withCanLookup odbHandle $ \baseLookup -> do
          withOpenDBStack env baseRepo baseLookup $ \base -> do
            bounds <- stackedBoundaries base lookup
            let slices = catMaybes baseSlices
            if null slices
              then Lookup.withCanLookup (Stacked.stacked base lookup)
                (f bounds)
              else
                Lookup.withCanLookup
                  (Ownership.slicedStack slices base) $ \sliced ->
                Lookup.withCanLookup (Stacked.stacked sliced lookup)
                  (f bounds)

withOpenDBStack
  :: Env
  -> Repo  -- Stacked DB
  -> Lookup  -- Stacked DB
  -> (Lookup -> IO a)
  -> IO a
withOpenDBStack env@Env{..} repo lookup f = do
  parent <- repoParent env repo
  case parent of
    Nothing -> f lookup
    Just baseRepo ->
      withOpenDatabase env baseRepo $ \OpenDB{..} ->
        Lookup.withCanLookup odbHandle $ \baseLookup ->
        withOpenDBStack env baseRepo baseLookup $ \base -> do
          Lookup.withCanLookup (Stacked.stacked base lookup) f

withWritableDatabase :: Env -> Repo -> (WriteQueue -> IO a) -> IO a
withWritableDatabase env repo action =
  withOpenDatabase env repo $ \OpenDB{..} -> case odbWriting of
    Just Writing{..} -> action wrQueue
    Nothing -> dbError repo "can't write to a read-only database"

readDatabase
  :: Env
  -> Repo
  -> (OpenDB -> Lookup.Lookup -> IO a)
  -> IO a
readDatabase env repo f =
  readDatabaseWithBoundaries env repo $ \odb _ lookup ->
  f odb lookup

readDatabaseWithBoundaries
  :: Env
  -> Repo
  -> (OpenDB -> Boundaries -> Lookup -> IO a)
  -> IO a
readDatabaseWithBoundaries env repo f =
  withOpenDatabase env repo $ \odb ->
  withOpenDBLookup env repo odb $ \bounds lookup ->
    f odb bounds lookup

newDB :: Repo -> STM DB
newDB repo = DB repo
  <$> newTVar Closed
  <*> newTVar 0
  <*> newTVar mempty

acquireDB :: DB -> STM ()
acquireDB db = modifyTVar' (dbUsers db) (+1)

-- | MUST be paired with 'acquireDB'
releaseDB :: Env -> DB -> STM ()
releaseDB env DB{..} = do
  users <- readTVar dbUsers
  writeTVar dbUsers $! users - 1
  when (users == 1) $ do
    state <- readTVar dbState
    case state of
      Closed -> do
        meta <- try $ Catalog.readMeta (envCatalog env) dbRepo
        case meta :: Either SomeException Meta of
          Right Meta{metaCompleteness = Thrift.Incomplete{}} -> return ()
          _ -> modifyTVar' (envActive env) $ HashMap.delete dbRepo
      _ -> return ()

withActiveDatabase :: HasCallStack => Env -> Repo -> (DB -> IO a) -> IO a
withActiveDatabase env@Env{..} repo = bracket
  (atomically $ do
    r <- lookupActiveDatabase env repo
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
  (atomically . releaseDB env)

usingActiveDatabase :: Env -> Repo -> (Maybe DB -> IO a) -> IO a
usingActiveDatabase env repo = bracket
  (atomically $ do
    r <- lookupActiveDatabase env repo
    mapM_ acquireDB r
    return r)
  (atomically . mapM_ (releaseDB env))

lookupActiveDatabase :: Env -> Repo -> STM (Maybe DB)
lookupActiveDatabase Env{..} repo = HashMap.lookup repo <$> readTVar envActive

updateLookupCacheStats :: Env -> IO ()
updateLookupCacheStats env =
  Stats.bump (envStats env) Stats.lookupCacheStats
  =<< LookupCache.readStatsAndResetCounters (envLookupCacheStats env)

setupSchema :: Storage s => Env -> Repo -> Database s -> Mode -> IO DbSchema
setupSchema Env{..} _ handle (Create _ _ initial) = do
  schema <- Observed.get envSchemaSource
  dbSchema <- case initial of
    UseDefaultSchema ->
      newDbSchema (Just envDbSchemaCache) schema
        LatestSchemaAll readWriteContent
    UseSpecificSchema schemaId ->
      newDbSchema (Just envDbSchemaCache) schema
        (SpecificSchemaId schemaId) readWriteContent
    UseThisSchema info ->
      fromStoredSchema (Just envDbSchemaCache) info readWriteContent
  storeSchema handle $ toStoredSchema dbSchema
  return dbSchema
setupSchema env@Env{..} repo handle mode = do
  stored <- retrieveSchema repo handle
  case stored of
    Just info
      | ReadOnly <- mode -> mergeSchema
      | otherwise ->
        fromStoredSchema (Just envDbSchemaCache) info readWriteContent
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
          (readOnlyContent stats)
    Nothing ->
      dbError repo "DB has no stored schema"

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
    release = atomically . mapM_ (releaseDB env)

  -- Empty the DbSchema cache. We probably have a new SchemaIndex now
  -- which invalidates all the entries anyway, and also we don't prune
  -- this cache anywhere else.
  when (isNothing mbRepo) $ do
    modifyMVar_ envDbSchemaCache $ \_ -> return HashMap.empty
    -- reset the record of DB open failures, these might succeed now
    -- that we've updated the schema.
    atomically $ writeTVar envOpenFailed HashMap.empty

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
                "failed: " <> show err
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
  let cache_limit_mb :: Word64 = fromIntegral $ ServerConfig.config_db_lookup_cache_limit_mb scfg
  lookupCache <- LookupCache.new
    (fromIntegral$ cache_limit_mb * 1024 * 1024)
    (fromIntegral $ ServerConfig.config_db_writer_threads scfg)
    envLookupCacheStats
  next_id <- newIORef =<< Lookup.firstFreeId lookup
  mutex <- newMutex ()
  queue <- WriteQueue <$> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0
    <*> newTVarIO 0 <*> newTVarIO 0
  anchorName <- newTVarIO Nothing
  return Writing
    { wrLock = mutex
    , wrNextId = next_id
    , wrLookupCache = lookupCache
    , wrLookupCacheAnchorName = anchorName
    , wrQueue = queue
    }

-- | Open a database asynchronously, returning an 'Async' that can be waited on.
-- This is the only way to open a database since this operation has
-- uninterruptible chunks which might take a long time.
--
-- NOTE: This should be called with 'dbState' set to 'Opening' and async
-- exceptions masked.
asyncOpenDB
  :: Env
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
asyncOpenDB env@Env{..} db@DB{..} version mode deps on_success on_failure =
  -- Be paranoid about 'spawnMask' itself throwing.
  handling_failures $ Warden.spawnMask envWarden $ \restore ->
  loggingAction (runLogRepo "open" env dbRepo) (const mempty) $
  bracket_ (atomically $ acquireDB db) (atomically $ releaseDB env db) $
  handling_failures $ do
    logInfo $ inRepo dbRepo "opening"
    bracketOnError
      (Storage.open envStorage dbRepo mode version)
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
            stored_units <- case mode of
              Create{} -> do
                let units = case deps of
                      Just (Thrift.Dependencies_pruned Thrift.Pruned{..}) ->
                        pruned_units
                      _ -> []
                storeUnits handle units
                return (Just units)
              _ -> retrieveUnits dbRepo handle
            maybeSlices <- baseSlices env deps stored_units
            idle <- newTVarIO =<< getTimePoint
            ownership <- newTVarIO =<< Storage.getOwnership handle
            on_success
            return OpenDB
              { odbHandle = handle
              , odbWriting = writing
              , odbSchema = dbSchema
              , odbIdleSince = idle
              , odbBaseSlices = maybeSlices
              , odbOwnership = ownership
              }
          atomically $ writeTVar dbState $ Open odb
          return odb
  where
    handling_failures :: IO a -> IO a
    handling_failures = handle $ \exc -> do
      atomically (writeTVar dbState Closed)
      logError $ inRepo dbRepo "couldn't open: " ++ show (exc :: SomeException)
      on_failure exc
      throwIO exc


-- | Fetch the glean.schema_version property of a DB, if it has
-- one. This property is used to resolve queries that don't specify
-- precise predicate versions.
getDbSchemaVersion
  :: Env
  -> Thrift.Repo
  -> IO (Maybe Thrift.Version, Maybe Thrift.SchemaId)
getDbSchemaVersion env repo = do
  props <- atomically $ Thrift.metaProperties <$>
    Catalog.readMeta (envCatalog env) repo
  let
    ver = case HashMap.lookup "glean.schema_version" props of
      Just txt | Right v <- textToInt txt -> Just (fromIntegral v)
      _otherwise -> Nothing
    id = case HashMap.lookup "glean.schema_id" props of
      Just txt -> Just (Thrift.SchemaId txt)
      _otherwise -> Nothing
  return (ver, id)

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
-- TODO: later we will store the slices in the stacked DB, and read it
-- back directly from there.
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
          vlog 1 $ "computing slice for " <> showRepo repo <> " with " <>
            show (Set.toList units) <>
            if exclude then " excluded" else " included"
          unitIds <- fmap catMaybes $
            forM (Set.toList units) $ \name -> do
              id <- Storage.getUnitId odbHandle name
              vlog 2 $ "unit: " <> show name <> " = " <> show id
              return id
          maybeOwnership <- readTVarIO odbOwnership
          forM maybeOwnership $ \ownership ->
            Ownership.slice ownership (catMaybes rest) unitIds exclude
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
isDatabaseClosed env repo = do
  r <- lookupActiveDatabase env repo
  case r of
    Just db -> do
      st <- readTVar $ dbState db
      case st of
        Closed -> return True
        _ -> return False
    _ -> return True

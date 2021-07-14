-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Open (
  lookupActiveDatabase, withActiveDatabase, usingActiveDatabase,
  withOpenDatabase, withOpenDatabaseStack, withWritableDatabase,
  readDatabase,
  asyncOpenDB,
  newDB, acquireDB, releaseDB,
  isDatabaseClosed,
  schemaUpdated,
  getDbSchemaVersion,
  updateLookupCacheStats
) where

import Control.Concurrent.Async (Async, wait)
import Control.Concurrent.STM
import Control.Exception hiding(try)
import Control.Monad.Catch (try)
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe
import qualified Data.Text as Text

import Util.Log
import Util.Text

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter (Locality(..))
import Glean.Database.Data
import Glean.Database.Exception
import Glean.Database.Repo
import qualified Glean.Database.Stats as Stats
import Glean.Database.Storage
import qualified Glean.Database.Storage as Storage
import Glean.Database.Meta (Meta(..))
import Glean.Database.Schema
import Glean.Database.Types
import Glean.Repo.Text
import qualified Glean.RTS.Foreign.Lookup as Lookup
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import qualified Glean.RTS.Foreign.Ownership as Ownership
import qualified Glean.RTS.Foreign.Stacked as Stacked
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types (Repo)
import qualified Glean.Internal.Types as Thrift
import qualified Glean.Types as Thrift
import Glean.Util.Mutex
import qualified Glean.Util.Observed as Observed
import Glean.Util.Time
import qualified Glean.Util.Warden as Warden

withOpenDatabase :: Env -> Repo -> (OpenDB -> IO a) -> IO a
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
                  | Thrift.Finalizing{} <- completeness = ReadOnly
                  | Thrift.Complete{} <- completeness = ReadOnly
                  | otherwise = ReadWrite
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
          -- opening a DB has long uninterruptible sections so do it on a
          -- separate thread in case we get cancelled
          opener <-
            asyncOpenDB env db version mode deps (return ()) (const $ return ())
          restore $ wait opener
    action odb `finally` do
      t <- getTimePoint
      atomically $ writeTVar (odbIdleSince odb) t

-- | Runs the action on each DB in the stack, in top-to-bottom order
withOpenDatabaseStack :: Env -> Repo -> (OpenDB -> IO a) -> IO [a]
withOpenDatabaseStack env@Env{..} repo action = go repo [] where
  go repo results = do
    deps <- atomically $ metaDependencies <$> Catalog.readMeta envCatalog repo
    results' <- withOpenDatabase env repo $ fmap (: results) . action
    case deps of
      Nothing -> return $ reverse results'
      Just (Thrift.Dependencies_pruned pruned)
        | Thrift.Pruned{Thrift.pruned_base=baseRepo} <- pruned ->
            go baseRepo results'
      Just (Thrift.Dependencies_stacked baseRepo) ->
        go baseRepo results'

withOpenDBLookup :: Env -> Repo -> OpenDB -> (Lookup.Lookup -> IO a) -> IO a
withOpenDBLookup env@Env{..} repo
    OpenDB{ odbHandle = handle, odbBaseSlice = baseSlice } f = do
  deps <- atomically $ metaDependencies <$> Catalog.readMeta envCatalog repo
  case deps of
    Nothing -> Lookup.withLookup handle f
    Just (Thrift.Dependencies_stacked base_repo) ->
      readDatabase env base_repo $ \_ base ->
      Lookup.withLookup handle $ \lookup ->
      Lookup.withLookup (Stacked.stacked base lookup) f
    Just (Thrift.Dependencies_pruned update) -> do
      let base_repo = Thrift.pruned_base update
      case baseSlice of
        Nothing -> throwIO $ Thrift.Exception $
          repoToText repo <> ": missing base slice"
        Just slice ->
          readDatabase env base_repo $ \baseOdb base -> do
          own <- case baseOdb of OpenDB{..} -> Storage.getOwnership odbHandle
          Lookup.withLookup (Ownership.sliced own slice base) $ \sliced ->
            Lookup.withLookup handle $ \lookup ->
            Lookup.withLookup (Stacked.stacked sliced lookup) f


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
readDatabase env repo f = withOpenDatabase env repo $ \odb ->
  withOpenDBLookup env repo odb $ f odb

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

withActiveDatabase :: Env -> Repo -> (DB -> IO a) -> IO a
withActiveDatabase env@Env{..} repo = bracket
  (atomically $ do
    r <- lookupActiveDatabase env repo
    db <- case r of
      Just db -> return db
      Nothing -> do
        exists <- Catalog.exists envCatalog [Local] repo
        when (not exists) $ throwSTM $ Thrift.UnknownDatabase repo
        deleting <- HashMap.member repo <$> readTVar envDeleting
        -- TODO: different error?
        when deleting $ throwSTM $ Thrift.UnknownDatabase repo
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
setupSchema Env{..} _ handle (Create _ initial) = do
  (source, currentSchema) <- Observed.get envSchemaSource
  schema <- case initial of
    Nothing -> newDbSchema source currentSchema readWriteContent
    Just info ->
      newMergedDbSchema info source currentSchema TakeOld readWriteContent
  storeSchema handle $ toSchemaInfo schema
  return schema
setupSchema Env{..} repo handle mode = do
  stored <- retrieveSchema repo handle
  case stored of
    Just info
      | ReadOnly <- mode -> mergeSchema
      | otherwise -> fromSchemaInfo info readWriteContent
          -- while writing, we don't allow new predicates to be added to
          -- the schema. This is the easiest way to prevent facts being
          -- added to the DB that aren't in the original stored schema.
          -- It might be possible to do something safe here, e.g. only
          -- merge non-stored derived predicates. (TODO)
      where
      mergeSchema = do
        -- merge the schema in the DB with the current schema, so
        -- that we can query for derived predicates that weren't
        -- stored in the DB when it was created.
        (source, currentSchema) <- Observed.get envSchemaSource
        stats <- Storage.predicateStats handle
        newMergedDbSchema info source currentSchema
          (if envSchemaOverride then TakeNew else TakeOld)
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
            schema <- setupSchema env dbRepo odbHandle ReadOnly
            atomically $ do
              state <- readTVar dbState
              case state of
                Opening -> return ()
                 -- if we are Opening now, this must have happened
                 -- after the transaction above, so it will already
                 -- pick up the new schema.
                Open odb -> writeTVar dbState $ Open odb { odbSchema = schema }
                Closing -> return ()
                Closed -> return ()
  logInfo "done updating schema for open DBs"


setupWriting :: Lookup.CanLookup lookup => Env -> lookup -> IO Writing
setupWriting Env{..} lookup = do
  scfg <- Observed.get envServerConfig
  lookupCache <- LookupCache.new
    (fromIntegral
      $ ServerConfig.config_db_lookup_cache_limit_mb scfg * 1024 * 1024)
    (fromIntegral $ ServerConfig.config_db_writer_threads scfg)
    envLookupCacheStats
  next_id <- newIORef =<< Lookup.firstFreeId lookup
  mutex <- newMutex ()
  queue <- WriteQueue <$> newTQueueIO <*> newTVarIO 0 <*> newTVarIO 0
    <*> newTVarIO 0 <*> newTVarIO 0
  return Writing
    { wrLock = mutex
    , wrNextId = next_id
    , wrLookupCache = lookupCache
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
            maybeSlice <- baseSlice env deps
            idle <- newTVarIO =<< getTimePoint
            on_success
            return OpenDB
              { odbHandle = handle
              , odbWriting = writing
              , odbSchema = dbSchema
              , odbIdleSince = idle
              , odbBaseSlice = maybeSlice
              }
          atomically $ writeTVar dbState $ Open odb
          return odb
  where
    handling_failures :: IO a -> IO a
    handling_failures = handle $ \exc -> do
      atomically (writeTVar dbState Closed)
      logError $ inRepo dbRepo "couldn't open:" ++ show (exc :: SomeException)
      on_failure exc
      throwIO exc


-- | Fetch the glean.schema_version property of a DB, if it has
-- one. This property is used to resolve queries that don't specify
-- precise predicate versions.
getDbSchemaVersion :: Env -> Thrift.Repo -> IO (Maybe Thrift.Version)
getDbSchemaVersion env repo = do
  props <- atomically $ Thrift.metaProperties <$>
    Catalog.readMeta (envCatalog env) repo
  case HashMap.lookup "glean.schema_version" props of
    Just txt | Right v <- textToInt txt -> return (Just (fromIntegral v))
    _otherwise -> return Nothing


-- TODO: later we will store the slice in the stacked DB, and read it
-- back directly from there.
baseSlice :: Env -> Maybe Thrift.Dependencies -> IO (Maybe Ownership.Slice)
baseSlice env (Just (Thrift.Dependencies_pruned update)) = do
  let base_repo = Thrift.pruned_base update
  withOpenDatabase env base_repo $ \OpenDB{..} -> do
    unitIds <- forM (Thrift.pruned_units update) $ \unit -> do
      r <- Storage.getUnitId odbHandle unit
      case r of
        Nothing -> throwIO $ Thrift.BadQuery $
          "unknown unit: " <> Text.pack (show unit)
        Just uid -> return uid
    ownership <- Storage.getOwnership odbHandle
    Just <$> Ownership.slice ownership unitIds
      (Thrift.pruned_exclude update)
baseSlice _ _ =
  return Nothing


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

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
{- | A Catalog stores metadata about available databases.
A thread running Glean.Database.Backup.backuper will continuously read the
state of the catalog to decide which dbs to restore/backup/finalize
-}

module Glean.Database.Catalog
  ( Catalog
  , Entry
  , CatalogClosedError(..)
  , EntryAlreadyExists(..)
  , open
  , close
  , create
  , delete
  , list
  , listMostRecent
  , exists
  , readMeta
  , writeMeta
  , modifyMeta
  , readExpiring
  , setExpiring
  , unsetExpiring
  , startRestoring
  , finishRestoring
  , abortRestoring
  , resetElsewhere
  , getLocalDatabases
  , getLocalDatabase
  , dbFailed
  , resetFailed
  -- for testing
  , getEntries
  , list'
  , Entries
  , EntriesF(..)
  , EntryF(..)
  , mkEntry
  ) where

import Control.Concurrent.Async
import Control.Exception
    ( Exception,
      SomeException,
      bracket,
      bracket_,
      try,
      mask_,
      uninterruptibleMask_ )
import Control.Monad
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import GHC.Stack (HasCallStack)
import System.IO (fixIO)

import Util.Control.Exception (tryBracket)
import qualified Util.Control.Exception.CallStack as CallStack
import Util.Log
import Util.STM

import Glean.Database.Catalog.Filter
import Glean.Database.Catalog.Store (Store)
import qualified Glean.Database.Catalog.Store as Store
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Repo
import Glean.Types (Repo(..))
import qualified Glean.Internal.Types as Thrift
import qualified Glean.Types as Thrift

-- | Catalog entry
type Entry = EntryF TVar

-- | Catalog entry, parameterized by the state type;
--   This extra generality comes handy in tests.
data EntryF ref = Entry
  { entryRepo :: Repo
  , entryStatus :: ref ItemStatus
      -- ^ This is the combined status of this db and the entryStatus of its
      -- dependencies. Sent with restorable status as 'Thrift.DatabaseStatus'.
  , entryMeta :: ref Meta
  , entryDirty :: ref Bool
      -- ^ does the metadata need to be written back (Entry must be in
      -- 'catDirtyQueue' in that case)
  , entryComitting :: ref Bool
      -- ^ is the metadata being committed (Entry can't be deleted while this
      -- is happening)
  , entryExpiring :: ref (Maybe UTCTime)
  }

deriving instance Show (EntryF Identity)
-- deriving instance Eq (EntryF Identity)

-- | All databases known to the 'Catalog'.
type Entries = EntriesF Entry

-- | The type of all databases in the 'Catalog', parameterized by the entry type
--   This extra generality comes handy in tests.
data EntriesF entry = Entries
  { entriesLiveHere :: HashMap Repo entry
      -- ^ available databases
  , entriesLiveElsewhere :: HashMap Repo entry
      -- ^ available databases in another shard
      --   ignored for all purposes except by getLocalDatabases
  , entriesRestoring :: HashMap Repo Meta
      -- ^ databases that are being restored
  , entriesEphemeral :: HashSet Repo
      -- ^ databases that are being created or deleted
  , entriesFailed :: HashMap Repo SomeException
      -- ^ databases that failed to open, and their exceptions
  }
  deriving (Show, Functor, Foldable, Traversable)


-- | Catalog of database entries
data Catalog = forall local. Store local => Catalog
  { catLocal :: local
      -- ^ local metadata storage
  , catEntries :: TVar (Maybe Entries)
      -- ^ all known entries ('Nothing' means catalog is closed)
  , catDirtyQueue :: TQueue Entry
      -- ^ queue of entries which need to have their metadata committed
  , catCommitter :: Async ()
      -- ^ metadata committer thread
  , catRepoDependents :: TVar (HashMap Repo [Repo])
      -- ^ keep track of repos that depend on a repo
  }

data CatalogClosedError = CatalogClosedError
  deriving(Show,Typeable)

instance Exception CatalogClosedError

newtype EntryAlreadyExists = EntryAlreadyExists Repo
  deriving(Eq,Show,Typeable)

instance Exception EntryAlreadyExists

mkEntry :: Repo -> Meta -> STM Entry
mkEntry repo meta = Entry repo
  <$> newTVar ItemMissing
  <*> newTVar meta
  <*> newTVar False
  <*> newTVar False
  <*> newTVar Nothing

-- | Updates entryStatus. Needs be called whenever the status of this db or one
-- of its dependencies changes
recalculateStatus :: Catalog -> Entry -> STM ()
recalculateStatus Catalog{..} entry = do
  meta <- readTVar $ entryMeta entry
  entries <- readTVar catEntries
  case entries of
    Nothing -> return ()
    Just Entries{..} -> do
      let
        repo = entryRepo entry

        missingStatus repo | repo `HashMap.member` entriesRestoring =
          ItemRestoring
        missingStatus _ = ItemMissing

        dependencies = case metaDependencies meta of
          Just (Thrift.Dependencies_stacked Thrift.Stacked{..}) ->
            [(Repo stacked_name stacked_hash, stacked_guid)]
          Just (Thrift.Dependencies_pruned up) ->
            [(Thrift.pruned_base up, Thrift.pruned_guid up)]
          Nothing -> []
        live = repo `HashMap.member` entriesLiveHere
      forM_ dependencies $ \(dep, _) -> if live then
          modifyTVar' catRepoDependents $
            HashMap.insertWith (<>) dep [repo]
        else
          modifyTVar' catRepoDependents $
            HashMap.adjust (filter (repo /=)) dep

      let
        itemStatusFor :: Thrift.Completeness -> ItemStatus
        itemStatusFor Thrift.Complete{} = ItemComplete
        itemStatusFor Thrift.Incomplete{} = ItemIncomplete
        itemStatusFor Thrift.Broken{} = ItemBroken
        itemStatusFor Thrift.Finalizing{} = ItemFinalizing

      dependencyStatuses <- forM dependencies $ \(dep, guid) -> do
        let missing = missingStatus dep
        case HashMap.lookup dep entriesLiveHere of
          Nothing -> return missing
          Just entry -> do
            meta <- readTVar $ entryMeta entry
            status <- readTVar $ entryStatus entry
            let properties = Thrift.metaProperties meta
            if HashMap.lookup "glean.guid" properties == guid then
              return status
            else
              return missing

      meta <- readTVar $ entryMeta entry
      oldStatus <- readTVar $ entryStatus entry
      let
        status
          | Just{} <- HashMap.lookup repo entriesFailed = ItemBroken
          | live = itemStatusFor $ metaCompleteness meta
          | otherwise = missingStatus repo
        newStatus = mconcat $ status:dependencyStatuses

      when (newStatus /= oldStatus) $ do
        writeTVar (entryStatus entry) newStatus
        recalculateDepsStatus Catalog{..} repo

recalculateDepsStatus :: Catalog -> Repo -> STM ()
recalculateDepsStatus Catalog{..} repo = do
  repoDependents <- readTVar catRepoDependents
  entries <- readTVar catEntries
  forM_ (HashMap.lookupDefault [] repo repoDependents) $
    \dep -> forM_ (entries >>= HashMap.lookup dep . entriesLiveHere) $
      \entry -> recalculateStatus Catalog{..} entry

itemDatabaseStatus :: ItemStatus -> Thrift.DatabaseStatus
itemDatabaseStatus ItemComplete = Thrift.DatabaseStatus_Complete
itemDatabaseStatus ItemIncomplete = Thrift.DatabaseStatus_Incomplete
itemDatabaseStatus ItemRestoring = Thrift.DatabaseStatus_Restoring
itemDatabaseStatus ItemBroken = Thrift.DatabaseStatus_Broken
itemDatabaseStatus ItemMissing = Thrift.DatabaseStatus_Missing
itemDatabaseStatus ItemFinalizing = Thrift.DatabaseStatus_Finalizing

dirtyEntry :: Catalog -> Entry -> STM ()
dirtyEntry cat entry = do
  dirty <- swapTVar (entryDirty entry) True
  when (not dirty) $ writeTQueue (catDirtyQueue cat) entry

-- | Metadata committer thread. This thread should *never* be cancelled since it
-- needs to process outstanding commit requests before shutting down.
-- Accordingly, it runs in uninterruptibleMask_. Note that crashing in case of
-- heap/stack overflow is preferable to this thread dying.
commit :: Catalog -> IO ()
commit cat@Catalog{..} = uninterruptibleMask_ loop
  where
    loop = do
      continue <- bracket
        -- get Just the next entry or Nothing if the Catalog has been closed
        (atomically $
          do
            entry <- readTQueue catDirtyQueue
            writeTVar (entryComitting entry) True
            return $ Just entry
          `orElse`
          do
            open <- isJust <$> readTVar catEntries
            when open retry
            return Nothing)
        (\r -> forM_ r $ \entry ->
            atomically $ writeTVar (entryComitting entry) False)
        $ \r -> case r of
            Just entry@Entry{..} -> do
              meta <- atomically $ do
                writeTVar entryDirty False
                readTVar entryMeta
              -- catch *all* exceptions
              r <- try $ Store.put catLocal entryRepo meta
              case r of
                Right ok -> when (not ok) $
                  -- TODO: What do we do in this case?
                  logError $ inRepo entryRepo
                    "couldn't commit meta because entry no longer exists"
                Left (exc :: SomeException) -> do
                  -- TODO: this is lame - it's not clear what to do if we can't
                  -- write the metadata
                  logError $ inRepo entryRepo $
                    "couldn't commit meta: " ++ show exc
                  atomically $ dirtyEntry cat entry
              return True
            Nothing -> return False
      when continue loop

getEntries :: Catalog -> STM Entries
getEntries cat = do
  r <- readTVar $ catEntries cat
  case r of
    Just entries -> return entries
    Nothing -> throwSTM CatalogClosedError

lookupEntry :: Catalog -> Repo -> STM (Maybe Entry)
lookupEntry cat repo = HashMap.lookup repo . entriesLiveHere <$> getEntries cat

getEntry :: HasCallStack => Catalog -> Repo -> STM Entry
getEntry cat repo = do
  r <- lookupEntry cat repo
  case r of
    Just entry -> return entry
    Nothing -> CallStack.throwSTM $ Thrift.UnknownDatabase repo

-- | Open a 'Catalog'
open :: Store local => local -> IO Catalog
open local = do
  metas <- Store.list local
  live <- atomically $ HashMap.traverseWithKey mkEntry metas
  entries <- newTVarIO $ Just Entries
    { entriesLiveHere = live
    , entriesLiveElsewhere = mempty
    , entriesRestoring = mempty
    , entriesEphemeral = mempty
    , entriesFailed = mempty
    }
  dependents <- newTVarIO mempty
  dirty_queue <- newTQueueIO
  cat <- fixIO $ \cat -> do
    committer <- mask_ $ async $ commit cat
    return Catalog
      { catLocal = local
      , catEntries = entries
      , catDirtyQueue = dirty_queue
      , catCommitter = committer
      , catRepoDependents = dependents
      }
  forM_ (HashMap.elems live) $ \entry ->
    atomically $ recalculateStatus cat entry
  return cat

-- | Close a 'Catalog'
close :: Catalog -> IO ()
close cat = do
  atomically $ do
    Entries{..} <- getEntries cat
    when (not $ HashSet.null entriesEphemeral) retry
    writeTVar (catEntries cat) Nothing
  void $ waitCatch $ catCommitter cat

-- | Create a new entry in a 'Catalog' and execute the supplied action if
-- creation was successful.
create :: Catalog -> Repo -> Meta -> STM () -> IO ()
create cat@Catalog{..} repo meta on_success = tryBracket
  (atomically $ do
    Entries{..} <- getEntries cat
    when
      (repo `HashMap.member` entriesLiveHere
        || repo `HashSet.member` entriesEphemeral
        || repo `HashMap.member` entriesRestoring) $
      throwSTM $ EntryAlreadyExists repo
    writeTVar catEntries $ Just Entries
      { entriesEphemeral = HashSet.insert repo entriesEphemeral, .. })
  (\_ r -> atomically $ do
    entry <- mkEntry repo meta
    modifyTVar' catEntries $ fmap $ \Entries{..} -> Entries
      { entriesEphemeral = HashSet.delete repo entriesEphemeral
      , entriesLiveHere = case r of
          Right _ -> HashMap.insert repo entry entriesLiveHere
          Left _ -> entriesLiveHere
      , ..
      }
    recalculateStatus cat entry
    forM_ r $ const on_success)
  $ const $ do
    ok <- Store.create catLocal repo meta
    -- This is *not* an EntryAlreadyExists error
    when (not ok) $ dbError repo
      "database already exists in local metadata store"

-- | Permanently delete an entry from a 'Catalog'
delete :: Catalog -> Repo -> IO ()
delete cat@Catalog{..} repo = bracket_
  (atomically $ do
    entry <- getEntry cat repo
    committing <- readTVar (entryComitting entry)
    when committing retry
    dirty <- readTVar (entryDirty entry)
    when dirty $ do
      es <- flushTQueue catDirtyQueue
      mapM_ (writeTQueue catDirtyQueue)
        $ filter (\e -> entryRepo e /= repo) es
    modifyTVar' catEntries $ fmap $ \Entries{..} -> Entries
      { entriesLiveHere = HashMap.delete repo entriesLiveHere
      , entriesEphemeral = HashSet.insert repo entriesEphemeral
      , ..
      }
    recalculateStatus cat entry)
  (atomically $ modifyTVar' catEntries $ fmap $ \Entries{..} -> Entries
    { entriesEphemeral = HashSet.delete repo entriesEphemeral, .. })
  -- Ignore if it doesn't exist for now
  $ void $ Store.delete catLocal repo

-- | List all databases in the given localities that match the given filters
list :: Catalog -> [Locality] -> Filter () -> STM [Item]
list cat locs f = getEntries cat >>= list' readTVar locs f

list'
  :: Monad m
  => (forall a. f a -> m a)
  -> [Locality]
  -> Filter ()
  -> EntriesF (EntryF f)
  -> m [Item]
list' read locs f Entries{..} = do
  fmap (runFilter f . concat) $ forM locs $ \loc -> do
    xs <- case loc of
      Local -> mapM statusAndMeta entriesLiveHere
      Restoring -> return $ fmap (ItemRestoring,) entriesRestoring
      Cloud -> mapM statusAndMeta entriesLiveElsewhere
    return $
      [ Item repo loc meta status
      | (repo, (status, meta)) <- HashMap.toList xs]
  where
    -- statusAndMeta :: Entry -> f (ItemStatus, Meta)
    statusAndMeta Entry{..} = do
      status <- read entryStatus
      meta <- read entryMeta
      return (status, meta)

-- | List the most recent instances of all DBs in the Catalog
listMostRecent :: Catalog -> IO [Item]
listMostRecent catalog =  atomically $
  list catalog [Local, Restoring, Cloud] $ groupF repoNameV $ do
    sortF createdV Descending
    limitF 1

-- | Check if a database exists in the catalog
exists :: Catalog -> [Locality] -> Repo -> STM Bool
exists cat locs repo = do
  Entries{..} <- getEntries cat
  let exists_in Local = HashMap.member repo entriesLiveHere
      exists_in Restoring = HashMap.member repo entriesRestoring
      exists_in Cloud = False
  return $ any exists_in locs

-- | Read the metadata of a database
readMeta :: HasCallStack => Catalog -> Repo -> STM Meta
readMeta cat repo = do
  entry <- getEntry cat repo
  readTVar (entryMeta entry)

-- | Update the metadata of a database
writeMeta :: Catalog -> Repo -> Meta -> STM ()
writeMeta cat repo meta = do
  entry <- getEntry cat repo
  writeTVar (entryMeta entry) meta
  recalculateStatus cat entry
  dirtyEntry cat entry

-- | Modify the metadata of a database and return the new metadata
modifyMeta :: Catalog -> Repo -> (Meta -> STM Meta) -> STM Meta
modifyMeta cat repo f = do
  entry <- getEntry cat repo
  old_meta <- readTVar (entryMeta entry)
  new_meta <- f old_meta
  writeTVar (entryMeta entry) new_meta
  recalculateStatus cat entry
  dirtyEntry cat entry
  return new_meta

dbFailed :: Catalog -> Repo -> SomeException -> STM ()
dbFailed cat repo exception = do
  Entries{..} <- getEntries cat
  writeTVar (catEntries cat) $ Just Entries
    { entriesFailed = HashMap.insert repo exception entriesFailed
    , ..
    }
  entry <- getEntry cat repo
  recalculateStatus cat entry

resetFailed :: Catalog -> STM ()
resetFailed cat = do
  Entries{..} <- getEntries cat
  writeTVar (catEntries cat) $ Just Entries
    { entriesFailed = HashMap.empty
    , ..
    }

readExpiring :: Catalog -> Repo -> STM (Maybe UTCTime)
readExpiring cat repo = do
  entry <- getEntry cat repo
  readTVar $ entryExpiring entry

setExpiring :: Catalog -> Repo -> UTCTime -> STM ()
setExpiring cat repo time = do
  entry <- getEntry cat repo
  writeTVar (entryExpiring entry) $ Just time

-- | Unset the expiring bit, to no longer mark the 'Repo' as expiring
unsetExpiring :: Catalog -> Repo -> STM ()
unsetExpiring cat repo = do
  entry <- getEntry cat repo
  writeTVar (entryExpiring entry) Nothing

-- | Schedule a database for download/restore
startRestoring :: Catalog -> Repo -> Meta -> STM ()
startRestoring cat repo meta = do
  Entries{..} <- getEntries cat
  when
    (repo `HashMap.member` entriesLiveHere
      || repo `HashMap.member` entriesRestoring
      || repo `HashSet.member` entriesEphemeral)
    $ dbError repo "can't restore: database already exists"
  writeTVar (catEntries cat) $ Just Entries
    { entriesRestoring = HashMap.insert repo meta entriesRestoring
    , ..
    }
  recalculateDepsStatus cat repo

-- | Notify the catalog that the database has been restored and is available
-- locally
finishRestoring :: Catalog -> Repo -> IO ()
finishRestoring cat@Catalog{..} repo = tryBracket
  (atomically $ do
    Entries{..} <- getEntries cat
    case HashMap.lookup repo entriesRestoring of
      Just meta -> do
        writeTVar catEntries $ Just Entries
          { entriesRestoring = HashMap.delete repo entriesRestoring
          , entriesEphemeral = HashSet.insert repo entriesEphemeral
          , ..
          }
        return meta
      Nothing -> dbError repo "finishRestoring: unknown database")
  (\meta r -> atomically $ do
    entry <- mkEntry repo meta
    modifyTVar' catEntries $ fmap $ \Entries{..} -> Entries
      { entriesEphemeral = HashSet.delete repo entriesEphemeral
      , entriesLiveHere = case r of
          Right _ -> HashMap.insert repo entry entriesLiveHere
          Left _ -> entriesLiveHere
      , .. }
    recalculateStatus cat entry)
  $ \meta -> do
    ok <- Store.create catLocal repo meta
    when (not ok) $ dbError repo
      "finishRestoring: entry already exists in local metadata store"

-- | Notify the catalog that the database is no longer being restored
abortRestoring :: Catalog -> Repo -> STM ()
abortRestoring cat repo = do
  Entries{..} <- getEntries cat
  when (not $ repo `HashMap.member` entriesRestoring) $ dbError repo
    "abortRestoring: unknown database"
  writeTVar (catEntries cat) $ Just Entries
    { entriesRestoring = HashMap.delete repo entriesRestoring
    , .. }
  recalculateDepsStatus cat repo

-- | Reset the catalog of items available elsewhere to the ones given
resetElsewhere :: Catalog -> [Item] -> STM ()
resetElsewhere cat items = do
  Entries{..} <- getEntries cat
  entriesElsewhere <- forM items $ \Item{..} -> do
    e <- mkEntry itemRepo itemMeta
    return (itemRepo, e)
  writeTVar (catEntries cat) $ Just Entries
    { entriesLiveElsewhere =
        HashMap.fromList entriesElsewhere
    , ..
    }

getLocalDatabases :: Catalog -> STM (HashMap Repo Thrift.GetDatabaseResult)
getLocalDatabases cat = do
  Entries{..} <- getEntries cat
  local <- HashMap.traverseWithKey (local_db Nothing) entriesLiveHere
  elsewhere <- HashMap.traverseWithKey
    (local_db (Just Thrift.DatabaseStatus_Available))
    entriesLiveElsewhere
  return $ mconcat
    [ HashMap.mapWithKey restoring_db entriesRestoring
    , updateFailed entriesFailed local
    , elsewhere
    ]
  where
    updateFailed entriesFailed = HashMap.mapWithKey $
      \repo r@Thrift.GetDatabaseResult{..} ->
        if
          | Just exception <- HashMap.lookup repo entriesFailed ->
            Thrift.GetDatabaseResult {
              getDatabaseResult_database = getDatabaseResult_database {
                Thrift.database_broken = Just $ Thrift.DatabaseBroken
                  { databaseBroken_task = "open"
                  , databaseBroken_reason = Text.pack (show exception)
                  }
                },
              ..
            }
          | otherwise -> r

    local_db overrideStatus repo entry = do
      meta <- readTVar $ entryMeta entry
      exp <- readTVar $ entryExpiring entry
      status <- readTVar $ entryStatus entry
      return Thrift.GetDatabaseResult
        { getDatabaseResult_database = metaToThriftDatabase
            (fromMaybe (itemDatabaseStatus status) overrideStatus)
            exp
            repo
            meta
        , getDatabaseResult_tasks = completenessTasks meta
        }

    restoring_db repo meta = Thrift.GetDatabaseResult
      { getDatabaseResult_database = metaToThriftDatabase
          Thrift.DatabaseStatus_Restoring
          Nothing
          repo
          meta
      , getDatabaseResult_tasks = Nothing
      }

getLocalDatabase :: Catalog -> Repo -> STM (Maybe Thrift.GetDatabaseResult)
-- TODO: This isn't very efficient
getLocalDatabase cat repo = HashMap.lookup repo <$> getLocalDatabases cat

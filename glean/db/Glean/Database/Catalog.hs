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
  , exists
  , readMeta
  , writeMeta
  , modifyMeta
  , readExpiring
  , writeExpiring
  , startRestoring
  , finishRestoring
  , abortRestoring
  , getLocalDatabases
  , getLocalDatabase
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import Data.Time
import Data.Typeable
import System.IO (fixIO)

import Util.Control.Exception (tryBracket)
import Util.Log

import Glean.Database.Catalog.Filter
import Glean.Database.Catalog.Store (Store)
import qualified Glean.Database.Catalog.Store as Store
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Repo
import Glean.Types (Repo(..))
import qualified Glean.Types as Thrift

-- | Catalog entry
data Entry = Entry
  { entryRepo :: Repo
  , entryStatus :: TVar EntryStatus
      -- ^ This is the combined status of this db and the entry status of its
      -- dependencies. Sent with restorable status as 'Thrift.DatabaseStatus'.
  , entryMeta :: TVar Meta
  , entryDirty :: TVar Bool
      -- ^ does the metadata need to be written back (Entry must be in
      -- 'catDirtyQueue' in that case)
  , entryComitting :: TVar Bool
      -- ^ is the metadata being committed (Entry can't be deleted while this
      -- is happening)
  , entryExpiring :: TVar (Maybe UTCTime)
  }

-- The status of a stacked database, ordered by increasing severity
data EntryStatus
  = EntryComplete
  | EntryIncomplete
  | EntryBroken
  | EntryRestoring
  | EntryMissing
  deriving (Eq,Ord,Show)

instance Semigroup EntryStatus where
  (<>) = max

instance Monoid EntryStatus where
  mempty = EntryComplete

-- | All databases known to the 'Catalog'
data Entries = Entries
  { entriesLive :: HashMap Repo Entry
      -- ^ available databases
  , entriesRestoring :: HashMap Repo Meta
      -- ^ databases that are being restored
  , entriesEphemeral :: HashSet Repo
      -- ^ databases that are being created or deleted
  }

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
  <$> newTVar EntryMissing
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
        missingStatus repo | repo `HashMap.member` entriesRestoring =
          EntryRestoring
        missingStatus _ = EntryMissing

        dependencies = case metaDependencies meta of
          Just (Thrift.Dependencies_stacked repo) -> [repo]
          Nothing -> []
        live = entryRepo entry `HashMap.member` entriesLive
      forM_ dependencies $ \dep -> if live then
          modifyTVar' catRepoDependents $
            HashMap.insertWith (<>) dep [entryRepo entry]
        else
          modifyTVar' catRepoDependents $
            HashMap.adjust (filter (entryRepo entry /=)) dep

      let
        entryStatusFor :: Thrift.Completeness -> EntryStatus
        entryStatusFor Thrift.Complete{} = EntryComplete
        entryStatusFor Thrift.Incomplete{} = EntryIncomplete
        entryStatusFor Thrift.Broken{} = EntryBroken
        entryStatusFor Thrift.Finalizing{} = EntryComplete
        -- Report Finalizing as Complete: the DB is ready to be queried,
        -- the finalize steps are administrative only and don't change
        -- anything observable.

      dependencyStatuses <- forM dependencies $ \dep ->
        maybe (return $ missingStatus dep) (readTVar . entryStatus) $
          HashMap.lookup dep entriesLive

      meta <- readTVar $ entryMeta entry
      oldStatus <- readTVar $ entryStatus entry
      let
        status = if live then
            entryStatusFor $ metaCompleteness meta
          else
            missingStatus $ entryRepo entry
        newStatus = mconcat $ status:dependencyStatuses

      when (newStatus /= oldStatus) $ do
        writeTVar (entryStatus entry) newStatus
        recalculateDepsStatus Catalog{..} (entryRepo entry)

recalculateDepsStatus :: Catalog -> Repo -> STM ()
recalculateDepsStatus Catalog{..} repo = do
  repoDependents <- readTVar catRepoDependents
  entries <- readTVar catEntries
  forM_ (HashMap.lookupDefault [] repo repoDependents) $
    \dep -> forM_ (entries >>= HashMap.lookup dep . entriesLive) $
      \entry -> recalculateStatus Catalog{..} entry

entryDatabaseStatus :: EntryStatus -> Thrift.DatabaseStatus
entryDatabaseStatus EntryComplete = Thrift.DatabaseStatus_Complete
entryDatabaseStatus EntryIncomplete = Thrift.DatabaseStatus_Incomplete
entryDatabaseStatus EntryRestoring = Thrift.DatabaseStatus_Restoring
entryDatabaseStatus EntryBroken = Thrift.DatabaseStatus_Broken
entryDatabaseStatus EntryMissing = Thrift.DatabaseStatus_Missing

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
lookupEntry cat repo = HashMap.lookup repo . entriesLive <$> getEntries cat

getEntry :: Catalog -> Repo -> STM Entry
getEntry cat repo = do
  r <- lookupEntry cat repo
  case r of
    Just entry -> return entry
    Nothing -> throwSTM $ Thrift.UnknownDatabase repo

-- | Open a 'Catalog'
open :: Store local => local -> IO Catalog
open local = do
  metas <- Store.list local
  live <- atomically $ HashMap.traverseWithKey mkEntry metas
  entries <- newTVarIO $ Just Entries
    { entriesLive = live
    , entriesRestoring = mempty
    , entriesEphemeral = mempty
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
      (repo `HashMap.member` entriesLive
        || repo `HashSet.member` entriesEphemeral
        || repo `HashMap.member` entriesRestoring) $
      throwSTM $ EntryAlreadyExists repo
    writeTVar catEntries $ Just Entries
      { entriesEphemeral = HashSet.insert repo entriesEphemeral, .. })
  (\_ r -> atomically $ do
    entry <- mkEntry repo meta
    modifyTVar' catEntries $ fmap $ \Entries{..} -> Entries
      { entriesEphemeral = HashSet.delete repo entriesEphemeral
      , entriesLive = case r of
          Right _ -> HashMap.insert repo entry entriesLive
          Left _ -> entriesLive
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
      { entriesLive = HashMap.delete repo entriesLive
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
list cat locs f = do
  Entries{..} <- getEntries cat
  fmap (runFilter f . concat) $ forM locs $ \loc -> do
    xs <- case loc of
      Local -> mapM (readTVar . entryMeta) entriesLive
      Restoring -> return entriesRestoring
      Cloud -> return mempty
    return [Item repo loc meta | (repo, meta) <- HashMap.toList xs]

-- | Check if a database exists in the catalog
exists :: Catalog -> [Locality] -> Repo -> STM Bool
exists cat locs repo = do
  Entries{..} <- getEntries cat
  let exists_in Local = HashMap.member repo entriesLive
      exists_in Restoring = HashMap.member repo entriesRestoring
      exists_in Cloud = False
  return $ any exists_in locs

-- | Read the metadata of a database
readMeta :: Catalog -> Repo -> STM Meta
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

readExpiring :: Catalog -> Repo -> STM (Maybe UTCTime)
readExpiring cat repo = do
  entry <- getEntry cat repo
  readTVar $ entryExpiring entry

writeExpiring :: Catalog -> Repo -> UTCTime -> STM ()
writeExpiring cat repo time = do
  entry <- getEntry cat repo
  writeTVar (entryExpiring entry) $ Just time

-- | Schedule a database for download/restore
startRestoring :: Catalog -> Repo -> Meta -> STM ()
startRestoring cat repo meta = do
  Entries{..} <- getEntries cat
  when
    (repo `HashMap.member` entriesLive
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
      , entriesLive = case r of
          Right _ -> HashMap.insert repo entry entriesLive
          Left _ -> entriesLive
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

getLocalDatabases :: Catalog -> STM (HashMap Repo Thrift.GetDatabaseResult)
getLocalDatabases cat = do
  Entries{..} <- getEntries cat
  HashMap.union (HashMap.mapWithKey restoring_db entriesRestoring)
    <$> HashMap.traverseWithKey local_db entriesLive
  where
    local_db repo entry = do
      meta <- readTVar $ entryMeta entry
      exp <- readTVar $ entryExpiring entry
      status <- readTVar $ entryStatus entry
      return Thrift.GetDatabaseResult
        { getDatabaseResult_database = metaToThriftDatabase
            (entryDatabaseStatus status)
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

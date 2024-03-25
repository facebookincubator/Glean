{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Glean.Database.Backup
  ( backuper
  , Event(..)
  , backupDatabase
  -- for testing
  , newestByRepo
  , bestRestore
  ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics hiding (Meta)
import System.Directory
import System.FilePath
import Text.Printf
import TextShow

import Util.Control.Exception
import Util.Graph
import Util.IO (safeRemovePathForcibly)
import Util.Log
import Util.Logger
import Util.STM

import Glean.Database.Backup.Backend as Backend
import Glean.Database.Backup.Locator
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.CompletePredicates
import qualified Glean.Database.Logger as Logger
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Open
import Glean.Database.Types
import Glean.Database.Schema
import Glean.Logger
import Glean.RTS.Foreign.Ownership (getOwnershipStats, showOwnershipStats)
import Glean.ServerConfig.Types (DatabaseBackupPolicy(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Internal.Types as Thrift
import Glean.Types as Thrift
import Glean.Util.Some
import Glean.Util.Observed as Observed
import Glean.Util.Trace

data Event
  = BackupStarted Repo
  | BackupFinished Repo
  | BackupFailed Repo
  | RestoreStarted Repo
  | RestoreFinished Repo
  | RestoreFailed Repo
  | RestoreAborted Repo
  | FinalizeStarted Repo
  | FinalizeFinished Repo
  | FinalizeFailed Repo
  | Waiting
  deriving (Eq, Generic, Show, Typeable)

instance Hashable Event

type Todo = (Repo, IO Bool)

-- | Set of Repos that shouldn't be backed up (because we already tried and
-- failed). We currently only remove things from here when they get deleted from
-- our DB list which matches previous behaviour. Eventually, we should retry
-- with exponential back-off.
type SinBin = HashSet Repo

-- | In order to download a DB we need twice the space to untar it, plus a bit
--   more for RocksDB to expand it
dbSizeDownloadFactor :: Double
dbSizeDownloadFactor = 2.1

-- | Get the next thing to do. Prioritises restores over backups and prefers
-- newer databases.
getTodo :: Env -> SinBin -> STM Todo
getTodo env@Env{..} sinbin = getFinalize <|> getRestore <|> getBackup
  where
    getFinalize = do
      dbs <- Catalog.list envCatalog [Local] $ do
        statusV .==. DatabaseStatus_Finalizing
        latest
      case dbs of
        [] -> retry
        Item{..} : _ -> return (itemRepo, doFinalize env itemRepo)

    getRestore = do
      restoring <- Catalog.list envCatalog [Restoring] $ do
        repoV `notInF` sinbin
        newestByRepo
      when (null restoring) retry
      avail <-  Catalog.list envCatalog [Local] $ do
        repoV `notInF` sinbin
        statusV .==. DatabaseStatus_Complete
        newestByRepo
      case bestRestore restoring avail of
        [] -> retry
        Item{..} : _ -> return (itemRepo, doRestore env itemRepo itemMeta)

    getBackup = do
      policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
      let allowedRepoNames = databaseBackupPolicy_allowed policy
      dbs <- Catalog.list envCatalog [Local] $ do
        repoNameV `inF` allowedRepoNames
        statusV .==. DatabaseStatus_Complete
        backedUpV .==. False
        latest
      case dbs of
        [] -> retry
        Item{..} : _ -> do
          dsite <- getSite env (repo_name itemRepo)
          case dsite of
            Just (prefix, Some site) ->
              return (itemRepo, doBackup env itemRepo prefix site)
            Nothing -> retry

    latest = do
      repoV `notInF` sinbin
      sortF createdV Descending
      limitF 1

newestByRepo :: Filter ()
newestByRepo = groupF repoNameV $ do
  sortF createdV Descending
  limitF 1
-- To find the next DB to restore, we
--   - find the newest DB of each repo that needs restoring
--   - restore the DB which is currently the most stale; that is
--     where the newest, complete, local DB of the same repo is the oldest
bestRestore :: [Item] -> [Item] -> [Item]
bestRestore restoring avail = depsFirst (sortBy order restoring)
  where
    -- stalest first, and then most recent if none are stale
    order a b =
        comparing staleness a b <>
        comparing (metaCreated . itemMeta) b a -- NB. descending

    availMap = HashMap.fromList
      [ (repo_name (itemRepo item), item) | item <- avail ]

    -- creation time of the local DB if stale, or maxBound
    staleness backup =
      case HashMap.lookup (repo_name (itemRepo backup)) availMap of
        Nothing -> NoLocalDb
        Just local
          | tlocal > tbackup -> LocalDbNewer
          | otherwise -> LocalDbOlder tlocal
          where
            tlocal = timestamp local
            tbackup = timestamp backup
            repoHashTime = metaRepoHashTime . itemMeta
            creationTime = metaCreated . itemMeta
            timestamp item = fromMaybe (creationTime item) (repoHashTime item)
            -- ^ prefer the exact repoHash time if we have it, otherwise use the
            -- creationTime as the best guess we have

    -- ensure we're restoring dependencies of a stacked DB first, if
    -- they need restoring.
    depsFirst items = postorder items itemRepo itemDeps
      where
      itemDeps item = case metaDependencies (itemMeta item) of
        Nothing -> []
        Just (Dependencies_stacked Thrift.Stacked{..}) ->
          [Thrift.Repo stacked_name stacked_hash]
        Just (Dependencies_pruned Pruned{..}) -> [pruned_base]

data Staleness
  = NoLocalDb
  | LocalDbOlder PosixEpochTime
  | LocalDbNewer
  deriving (Eq,Ord)

doBackup :: Site site => Env -> Repo -> Text -> site -> IO Bool
doBackup env@Env{..} repo prefix site =
  loggingAction (runLogRepo "backup" env repo) (const mempty) $ do
    atomically $ notify envListener $ BackupStarted repo
    say logInfo "starting"
    withOpenDatabaseStorage env repo $ \_storage OpenDB{..} -> do
    say logInfo "packing"
    stats <- mapMaybe
      (\(pid,stats) -> (,stats) . predicateRef <$> lookupPid pid odbSchema)
      <$> Storage.predicateStats odbHandle
    ownershipStats <- do
      maybeOwnership <- readTVarIO odbOwnership
      mapM getOwnershipStats maybeOwnership
    Backend.Data{..} <- withScratchDirectory envStorage repo $ \scratch ->
      Storage.backup odbHandle scratch $ \path Data{dataSize} -> do
        say logInfo "uploading"
        policy <- ServerConfig.databaseBackupPolicy_repos
          . ServerConfig.config_backup <$> Observed.get envServerConfig
        let ttl = case ServerConfig.backup_delete_after_seconds <$>
              Map.lookup (repo_name repo) policy of
                Just 0 -> Nothing
                ttl -> fromIntegral <$> ttl
        meta <- atomically $ Catalog.readMeta envCatalog repo
        let metaWithBytes = meta {
              metaCompleteness = case metaCompleteness meta of
                Complete DatabaseComplete{..} ->
                  Complete DatabaseComplete
                    {databaseComplete_bytes = Just (fromIntegral dataSize)
                    ,..}
                _ -> metaCompleteness meta
        }
        Backend.backup site repo (metaToProps metaWithBytes) ttl path
    let locator = toRepoLocator prefix site repo
    Logger.logDBStatistics env repo stats ownershipStats dataSize locator
    say logInfo "finished"
    atomically $ do
      void $ Catalog.modifyMeta envCatalog repo $ \meta -> return meta
        { metaBackup = Just locator
        , metaCompleteness = case metaCompleteness meta of
          Complete DatabaseComplete{..} ->
            Complete DatabaseComplete
              {databaseComplete_bytes = Just (fromIntegral dataSize)
              ,..}
          other -> other
        }
      notify envListener $ BackupFinished repo
    return True
  `catch` \exc -> do
    atomically $ notify envListener $ BackupFailed repo
    say logError $ "failed: " ++ show (exc :: SomeException)
    rethrowAsync exc
    return False
  where
    say log s = log $ inRepo repo $ "backup: " ++ s

backupDatabase :: Env -> Repo -> Text -> IO Bool
backupDatabase env repo loc
  | Just (prefix, site) <- fromSiteLocator (envBackupBackends env) loc =
      doBackup env repo prefix site
  | otherwise = throwIO $
      Thrift.InvalidLocator $ "invalid locator '" <> loc <>  "'"

doRestore :: Env -> Repo -> Meta -> IO Bool
doRestore env@Env{..} repo meta
  | Just loc <- metaBackup meta
  , Complete DatabaseComplete{databaseComplete_bytes}
  <- metaCompleteness meta
  , Just (_, Some site, r_repo) <- fromRepoLocator envBackupBackends loc
  , r_repo == repo =
    loggingAction (runLogRepo "restore" env repo) (const mempty) (do
      atomically $ notify envListener $ RestoreStarted repo
      mbFreeBytes <- (Just <$> Storage.getFreeCapacity envStorage)
                    `catch` \(_ :: IOException) -> return Nothing
      case (mbFreeBytes, databaseComplete_bytes)  of
        (Just freeBytes, Just size) -> do
          let neededBytes = ceiling $ dbSizeDownloadFactor * fromIntegral size
          when (freeBytes < neededBytes) $
            -- the catch-all exception handler will log and cancel the download
            throwIO $ ErrorCall $
              printf "Not enough disk space: %d needed, %d available"
                neededBytes freeBytes
        _ -> return ()

      withScratchDirectory envStorage repo $ \scratch -> do
      say logInfo "starting"
      say logInfo "downloading"
      let scratch_restore = scratch </> "restore"
          scratch_file = scratch </> "file"
      -- TODO: implement buffered downloads in Manifold client
      void $ Backend.restore site repo scratch_file
      say logInfo "restoring"
      createDirectoryIfMissing True scratch_restore
      Storage.restore envStorage repo scratch_restore scratch_file
      say logInfo "adding"
      Catalog.finishRestoring envCatalog repo
      atomically $ notify envListener $ RestoreFinished repo
      say logInfo "finished"
      return True
  )
  `catch` \exc -> do
    failed <- atomically $ do
      failed <- Catalog.exists envCatalog [Restoring] repo
      when failed $ do
        Catalog.abortRestoring envCatalog repo
        notify envListener $ RestoreFailed repo
      return failed
    when failed $ do
      say logError $ "failed: " ++ show exc
      swallow $ Storage.safeRemoveForcibly envStorage repo
    rethrowAsync exc
    -- NOTE: No point in adding the repo to the sinbin, we just removed
    -- it from the list of known DBs anyway.
    return True

  | otherwise = do
      atomically $ notify envListener $ RestoreStarted repo
      say logError $ case (metaCompleteness meta, metaBackup meta) of
        (Complete{}, Just loc) -> "invalid location " ++ show loc
        (incomplete, Just{}) -> "database incomplete: " <> show incomplete
        _ -> "missing location"
      atomically $ notify envListener $ RestoreFailed repo
      return False
  where
    say log s = log $ inRepo repo $ "restore: " ++ s

doFinalize :: Env -> Repo -> IO Bool
doFinalize env@Env{..} repo =
  loggingAction (runLogRepo "finalize" env repo) (const mempty) $ do
    atomically $ notify envListener $ FinalizeStarted repo

    -- If the client didn't explicitly call completePredicates, we'll
    -- do that now. Do this *before* optimising/compacting, because
    -- that deletes all the temporary ownership information consumed
    -- by this pass.
    meta <- atomically $ Catalog.readMeta envCatalog repo
    when (not (metaAxiomComplete meta)) $ syncCompletePredicates env repo

    config <- Observed.get envServerConfig
    withOpenDatabase env repo $ \OpenDB{..} -> do
      Storage.prepareFactOwnerCache odbHandle
      maybeOwnership <- readTVarIO odbOwnership
      forM_ maybeOwnership $ \ownership -> do
        stats <- getOwnershipStats ownership
        logInfo $ "final ownership: " <> Text.unpack (showOwnershipStats stats)
      let compact = ServerConfig.config_compact_on_completion config
      say logInfo $ if compact then "optimizing(compacting)" else "optimizing"
      Storage.optimize odbHandle compact
      Storage.cacheOwnership odbHandle

    -- update and re-merge our internal representation of the schema
    schemaUpdated env (Just repo)

    time <- envGetCurrentTime
    atomically $ do
      void $ Catalog.modifyMeta envCatalog repo $ \meta -> return meta
        { metaCompleteness = Complete $ DatabaseComplete
          (utcTimeToPosixEpochTime time)
          Nothing -- the size gets updated after a backup
        }
      notify envListener $ FinalizeFinished repo
    say logInfo "finished"
    return True

  `catch` \exc -> do
    let
      interrupted
        | Just AsyncCancelled{} <- fromException exc = True
        | otherwise = False

    -- If we were interrupted, then keep the DB in the finalizing state
    -- and we'll try to finalize it again later.  If there was some other
    -- exception, we'll mark the DB broken.
    atomically $ do
      notify envListener $ FinalizeFailed repo
      when (not interrupted) $
        void $ Catalog.modifyMeta envCatalog repo $ \meta -> return meta
          { metaCompleteness = Broken $ DatabaseBroken "finalize" (showt exc) }
    say logError $
      (if interrupted then "interrupted" else "failed") <>
      ": " <> show (exc :: SomeException)
    rethrowAsync exc
    return False
  where
    say log s = log $ inRepo repo $ "finalize: " ++ s

-- | Back up databases forever.
backuper :: Env -> IO ()
backuper env@Env{..} = loop mempty `catchAll` \exc -> do
  logError $ "unexpected error in backup thread: " ++ show exc
  backuper env
  where
    loop sinbin = do
      (repo, action) <- do
        r <- atomically $ Just <$> getTodo env sinbin <|> return Nothing
        case r of
          Just todo -> return todo
          Nothing -> do
            atomically $ notify envListener Waiting
            atomically $ getTodo env sinbin
      ok <- action `catchAll` \exc -> do
        -- NOTE: action shouldn't throw so this shouldn't ever run
        logError $ "unexpected error during backup: " ++ show exc
        return False
      sinbin' <- HashSet.fromList . map itemRepo <$> atomically
        (Catalog.list envCatalog [Local, Restoring] $
          repoV `inF` if ok then sinbin else HashSet.insert repo sinbin)
      loop sinbin'

withScratchDirectory
  :: Storage.Storage s => s -> Repo -> (FilePath -> IO a) -> IO a
withScratchDirectory storage repo action =
  Storage.withScratchRoot storage $ \root -> do
    let scratch = root </> databaseSubdir repo
    safeRemovePathForcibly scratch
    bracket_
      (createDirectoryIfMissing True scratch)
      (logExceptions id $ safeRemovePathForcibly scratch)
      (action scratch)

rethrowAsync :: SomeException -> IO ()
rethrowAsync exc
  | isAsyncException exc = throwIO exc
  | otherwise = return ()

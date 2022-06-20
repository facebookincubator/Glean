{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}
module Glean.Database.Backup
  ( backuper
  , Event(..)
) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
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
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import GHC.Generics hiding (Meta)
import System.Directory
import System.FilePath
import TextShow

import Util.Control.Exception
import Util.Graph
import Util.IO (safeRemovePathForcibly)
import Util.Log
import Util.Logger

import Glean.Database.Backup.Backend as Backend
import Glean.Database.Backup.Locator
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.CompletePredicates
import qualified Glean.Database.Logger as Logger
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Open (withOpenDatabase, schemaUpdated)
import Glean.Database.Types
import Glean.Database.Schema
import Glean.Logger
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
      let newestByRepo = groupF repoNameV $ do
            sortF createdV Descending
            limitF 1
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
            Just (prefix, Some site, _policy) ->
                return (itemRepo, doBackup env itemRepo prefix site)
            Nothing -> retry

    latest = do
      repoV `notInF` sinbin
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
        Just (Dependencies_stacked repo) -> [repo]
        Just (Dependencies_pruned Pruned{..}) -> [pruned_base]

data Staleness
  = NoLocalDb
  | LocalDbOlder PosixEpochTime
  | LocalDbNewer
  deriving (Eq,Ord)

doBackup :: Site site => Env -> Repo -> Text -> site -> IO Bool
doBackup env@Env{..} repo prefix site =
  do
    atomically $ notify envListener $ BackupStarted repo
    say logInfo "starting"
    withOpenDatabase env repo $ \OpenDB{..} -> do
    say logInfo "packing"
    stats <- mapMaybe
      (\(pid,stats) -> (,stats) . predicateRef <$> lookupPid pid odbSchema)
      <$> Storage.predicateStats odbHandle
    Backend.Data{..} <- withScratchDirectory envRoot repo $ \scratch ->
      Storage.backup odbHandle scratch $ \bytes -> do
        say logInfo "uploading"
        policy <- ServerConfig.databaseBackupPolicy_repos
          . ServerConfig.config_backup <$> Observed.get envServerConfig
        let ttl = case ServerConfig.backup_delete_after_seconds <$>
              Map.lookup (repo_name repo) policy of
                Just 0 -> Nothing
                ttl -> fromIntegral <$> ttl
        meta <- atomically $ Catalog.readMeta envCatalog repo
        Backend.backup site repo (metaToProps meta) ttl bytes
    Logger.logDBStatistics env repo stats dataSize
    say logInfo "finished"
    atomically $ do
      void $ Catalog.modifyMeta envCatalog repo $ \meta -> return meta
        { metaBackup = Just $ toRepoLocator prefix site repo }
      notify envListener $ BackupFinished repo
    return True
  `catch` \exc -> do
    atomically $ notify envListener $ BackupFailed repo
    say logError $ "failed: " ++ show (exc :: SomeException)
    rethrowAsync exc
    return False
  where
    say log s = log $ inRepo repo $ "backup: " ++ s

doRestore :: Env -> Repo -> Meta -> IO Bool
doRestore env@Env{..} repo meta
  | Just loc <- metaBackup meta
  , Just (_, Some site, r_repo) <- fromRepoLocator envBackupBackends loc
  , r_repo == repo =
    loggingAction (runLogRepo "restore" env repo) (const mempty) (do
      atomically $ notify envListener $ RestoreStarted repo
      withScratchDirectory envRoot repo $ \scratch -> do
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
      swallow $ safeRemovePathForcibly $ databasePath envRoot repo
    rethrowAsync exc
    -- NOTE: No point in adding the repo to the sinbin, we just removed
    -- it from the list of known DBs anyway.
    return True

  | otherwise = do
      atomically $ notify envListener $ RestoreStarted repo
      say logError $ case metaBackup meta of
        Just loc -> "invalid location " ++ show loc
        Nothing -> "missing location"
      atomically $ notify envListener $ RestoreFailed repo
      return False
  where
    say log s = log $ inRepo repo $ "restore: " ++ s

doFinalize :: Env -> Repo -> IO Bool
doFinalize env@Env{..} repo =
  do
    atomically $ notify envListener $ FinalizeStarted repo

    -- If the client didn't explicitly call completePredicates, we'll
    -- do that now. Do this *before* optimising/compacting, because
    -- that deletes all the temporary ownership information consumed
    -- by this pass.
    meta <- atomically $ Catalog.readMeta envCatalog repo
    when (not (metaAxiomComplete meta)) $ syncCompletePredicates env repo

    config <- Observed.get envServerConfig
    withOpenDatabase env repo $ \OpenDB{..} -> do
      when (ServerConfig.config_compact_on_completion config) $ do
        say logInfo "optimising"
        Storage.optimize odbHandle

    -- update and re-merge our internal representation of the schema
    schemaUpdated env (Just repo)

    time <- getCurrentTime
    atomically $ do
      void $ Catalog.modifyMeta envCatalog repo $ \meta -> return meta
        { metaCompleteness = Complete $ DatabaseComplete $
            utcTimeToPosixEpochTime time }
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

withScratchDirectory :: FilePath -> Repo -> (FilePath -> IO a) -> IO a
withScratchDirectory root repo action = do
  safeRemovePathForcibly scratch
  bracket_
    (createDirectoryIfMissing True scratch)
    (logExceptions id $ safeRemovePathForcibly scratch)
    (action scratch)
  where
    scratch = root </> ".scratch" </> databaseSubdir repo

rethrowAsync :: SomeException -> IO ()
rethrowAsync exc
  | isAsyncException exc = throwIO exc
  | otherwise = return ()

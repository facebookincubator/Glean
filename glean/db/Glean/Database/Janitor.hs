{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Janitor
  ( runDatabaseJanitor
  , runDatabaseJanitorPureish
  , JanitorSideEffect(..)
  -- for testing
  , mergeLocalAndRemote
  , dbIndex
  , DbIndex(..)
  ) where

import Control.Concurrent.Stream (stream)
import Control.Concurrent (getNumCapabilities)
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Writer.CPS
import Data.ByteString (ByteString)
import Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Time

import ServiceData.GlobalStats as Stats
import Util.Control.Exception
import Util.List
import Util.Log
import Util.Logger
import Util.STM
import Util.Time
import Util.TimeSec

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Close
import Glean.Database.Delete
import Glean.Database.List
import Glean.Database.Meta
import Glean.Database.Repo
import Glean.Database.Restore
import Glean.Database.Open
import Glean.Database.Retention
import Glean.Database.Types
import Glean.Internal.Types
import Glean.Logger
import Glean.Repo.Text
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.ShardManager
    ( ShardManager(getAssignedShards, computeShardMapping),
      SomeShardManager(SomeShardManager), countersForShardSizes, noSharding )
import Glean.Util.TransitiveClosure

{- |
The database janitor has the following functions:
  - ensure the newest db for each repo is open to speed-up queries to it.
  - close databases that haven't been used in a while.
  - kick-off downloads of dbs available in remote backups.
  - delete dbs we don't need any longer.
  - publish counters of local db states.
-}
runDatabaseJanitor :: Env -> IO ()
runDatabaseJanitor env = do
  runDatabaseJanitorPureish env >>= executeJanitorSideEffects env

executeJanitorSideEffects :: Env -> [JanitorSideEffect] -> IO ()
executeJanitorSideEffects env sideEffects =
  do
  forM_ sideEffects $ \case
    PublishCounter n v -> void $ Stats.setCounter n v
    _ -> pure ()
  numCaps <- getNumCapabilities
  stream numCaps (forM_ sideEffects) $ \case
    PreOpenDB repo ->
      void $ tryAll $ logExceptions (inRepo repo) $
        withOpenDatabaseStack env repo (\_ -> return ())
    _ -> pure ()

  -- Record the published counters and clear stale ones
  let countersPublished =
        HashSet.fromList [n | PublishCounter n _ <- sideEffects]
  lastPublishedCounters <- atomically $ do
    last <- readTVar (envDatabaseJanitorPublishedCounters env)
    writeTVar (envDatabaseJanitorPublishedCounters env) countersPublished
    return last
  forM_ (HashSet.difference lastPublishedCounters countersPublished)
    clearCounter


data JanitorSideEffect
  = PublishCounter !ByteString !Int
  | PreOpenDB !Repo
  deriving (Eq, Show)

publishCounter :: ByteString -> Int -> WriterT [JanitorSideEffect] IO ()
publishCounter name value = tell [PublishCounter name value]

preOpenDB :: Monad m => Repo -> WriterT [JanitorSideEffect] m ()
preOpenDB repo = tell [PreOpenDB repo]

-- WIP making the Janitor more testable by returning the list of side effects
runDatabaseJanitorPureish :: Env -> IO [JanitorSideEffect]
runDatabaseJanitorPureish env@Env{envShardManager = SomeShardManager sm} = do
  maybeShards <- getAssignedShards sm
  case maybeShards of
    Just shards ->
        runWithShards env (Set.fromList shards) sm
    Nothing -> do
      Just myShards <- getAssignedShards noSharding
      runWithShards env (Set.fromList myShards) noSharding

runWithShards
  :: (Show shard, Ord shard)
  => Env
  -> Set.Set shard
  -> ShardManager shard
  -> IO [JanitorSideEffect]
runWithShards env myShards sm = do
  loggingAction (runLogCmd "janitor" env) (const mempty) $ do
  logInfo $ "running database janitor with shard assignment "
    <> show (toList myShards)

  ServerConfig.Config{..} <- Observed.get (envServerConfig env)

  let
    !ServerConfig.DatabaseRetentionPolicy{} = config_retention
    !ServerConfig.DatabaseClosePolicy{..} = config_close

  fetchBackupsResult <- fetchBackups env

  now <- envGetCurrentTime env

  backups <- case fetchBackupsResult of
    ReusedPreviousBackups{reusedBackups = (dbs,_utc)} -> return dbs
    FetchedNewBackups{newBackups} -> return newBackups
    FetchFailure{..} -> do
      logError $ "couldn't list restorable databases: " <> show fetchError

      case previousGoodBackups of
        Nothing -> throwIO $ JanitorFetchBackupsFailure fetchError
        Just (dbs, utc) -> do
          let previousAgeInSeconds = fromIntegral (timeDiffInSeconds now utc)
              tooOld = previousAgeInSeconds > config_max_remote_db_list_age
          when tooOld $ throwIO $ JanitorFetchBackupsFailure fetchError

          return dbs

  localAndRestoring <- atomically $
    Catalog.list (envCatalog env) [Local,Restoring] everythingF

  dbToShard <- computeShardMapping sm

  cachedAvailableDBs <- readTVarIO (envCachedAvailableDBs env)
  let
    isAvailable db
      | HashSet.member db cachedAvailableDBs = return True
      | otherwise = not . null <$> envFilterAvailableDBs env [db]

    itemAvailable Item{itemRepo} = do
      st <- State.get
      case HashMap.lookup itemRepo st of
        Just isAvailable -> return isAvailable
        Nothing -> do
          isAvailable <- lift $ isAvailable itemRepo
          State.put $! HashMap.insert itemRepo isAvailable st
          return isAvailable

    allDBsByAge :: [Item]
    allDBsByAge = mergeLocalAndRemote backups localAndRestoring

    index@DbIndex{..} = dbIndex allDBsByAge

  (RetentionChanges{..}, cachedAvailable) <- (`runStateT` mempty) $
    retentionChanges
      config_retention
      config_restore
      now
      index
      itemAvailable
      dbToShard
      myShards

  atomically $ writeTVar
    (envCachedAvailableDBs env)
    (HashMap.keysSet $ HashMap.filter id cachedAvailable)

  let
    checkDependencies dbs msg1 msg2 =
      unless (null dbs) $
        logWarning $ msg1 <> ": " <> show dbs <> "\n" <> msg2

  checkDependencies allMissingDependencies
    "dependencies missing in Catalog"
    ("This suggests a dependency db has been deleted from the cloud, " <>
     "or a failure to enumerate the cloud catalog")

  checkDependencies localMissingDependencies
    "dependencies not downloaded"
    ("This probably means a bug in the db->shard mapping, " <>
     "or a failure to enumerate the cloud catalog")

  forM_ retentionLocal $ \(Item{itemRepo, itemLocality}, _) ->
    when (itemLocality == Local) $
      atomically $ Catalog.unsetExpiring (envCatalog env) itemRepo

  forM_ retentionDelete $ \Item{itemRepo = repo} -> do
    let
      ServerConfig.Retention{..} =
        NonEmpty.head $ repoRetention config_retention $ Thrift.repo_name repo
    expireDatabase (fromIntegral <$> retention_expire_delay) env repo
      `catch` \UnknownDatabase{} ->
        -- DBs are deleted asynchronously and this code is not transactional,
        -- so it's possible that the DB is not in the Catalog at this point
        return ()

  restores <- fmap catMaybes $ forM retentionRestore $ \Item{..} ->
    ifRestoreRepo env Nothing itemRepo $ do
      logInfo $ "Restoring: " ++ showRepo itemRepo ++
        " ("  ++ showNominalDiffTime (dbAge now itemMeta) ++ " old)"
      return $ Just $ Catalog.startRestoring (envCatalog env) itemRepo itemMeta
  -- register all the restoring DBs together in a single transaction,
  -- so that the backup thread can't jump in early and pick one
  atomically $ sequence_ restores

  atomically $ Catalog.resetElsewhere (envCatalog env) retentionElsewhere

  deleting <- readTVarIO (envDeleting env)

  -- Open the most recent local, complete DB for each repo in order
  -- to avoid lag spikes. This will be the DB that clients will get
  -- by default unless they specify a particular DB instance.
  let
      toOpen = uniqBy (comparing itemRepo) $
        [ item
        | (_, dbsByAge) <- byRepoName
        , item : _ <- [filter shouldOpen (NonEmpty.toList dbsByAge)]
        ] <>
        [ item
        | (repoNm, dbs) <- byRepoName
        , ServerConfig.Retention{retention_keep_open = True, ..} <-
            NonEmpty.toList (repoRetention config_retention repoNm)
        , item <- NonEmpty.toList dbs
        , shouldOpen item
        , hasProperties retention_required_properties item
        ]

      isComplete Complete{} = True
      isComplete _ = False

      shouldOpen item =
        isComplete (metaCompleteness (itemMeta item)) &&
        itemLocality item == Local &&
        not (HashMap.member (itemRepo item) deleting)

      closeDeps = transitiveClosureBy itemRepo (catMaybes . dependencies)

  -- close any DBs that are idle, avoiding the set of DBs that we will
  -- be proactively keeping open (and their dependencies).
  closeIdleDatabases env
     (seconds $ fromIntegral databaseClosePolicy_close_after)
     (map itemRepo $ closeDeps toOpen)

  execWriterT $ do
    forM_ toOpen $ \item ->
      whenM (liftIO $ atomically $ isDatabaseClosed env $ itemRepo item) $
        preOpenDB (itemRepo item)

    publishCounter "glean.db.remote.oldness" $ case fetchBackupsResult of
      ReusedPreviousBackups{reusedBackups = (_,utc)} ->
        timeDiffInSeconds now utc
      FetchFailure{previousGoodBackups = Just (_, utc)} ->
        timeDiffInSeconds now utc
      FetchFailure{previousGoodBackups = Nothing} -> 0
      FetchedNewBackups{} -> 0

    forM_ byRepoName $ \(repoNm, dbsByAge) -> do
      let prefix = "glean.db." <> Text.encodeUtf8 repoNm
      let repoKeep =
            [ item
            | (item,_) <- retentionLocal
            , repoNm == Thrift.repo_name (itemRepo item) ]

      -- upsert counters
      publishCounter (prefix <> ".all") $ length repoKeep
      publishCounter (prefix <> ".available") $ length $ filter
        (\Item{..} ->
          itemLocality == Local
          && completenessStatus itemMeta == Thrift.DatabaseStatus_Complete)
        repoKeep
      publishCounter (prefix <> ".restoring") $
        length restores +
        length (NonEmpty.filter (\Item{..} -> itemLocality == Restoring)
          dbsByAge)
      publishCounter (prefix <> ".indexing") $ length $ NonEmpty.filter
        (\Item{..} ->
          itemLocality == Local
          && completenessStatus itemMeta == Thrift.DatabaseStatus_Incomplete)
        dbsByAge
      publishCounter (prefix <> ".backups") $ length $ NonEmpty.filter
        (\Item{..} -> itemLocality == Cloud)
        dbsByAge

      -- Report the age of the newest local DB. We only want to report
      -- the age for DBs that clients can query, hence the locality
      -- filter. The global DB age will be calculated by taking the
      -- minimum age reported by all the servers.
      case [ item | item <- NonEmpty.toList dbsByAge,
                itemLocality item == Local ] of
        [] -> return ()
        db:_ -> do
          let
              meta = itemMeta db
              dbStart = dbTime meta
              ageFrom t0 = timeSpanInSeconds $
                fromUTCTime now `timeDiff` posixEpochTimeToTime t0
          -- .age is the age of the data, .span is the age of the DB
          publishCounter (prefix <> ".age") (ageFrom dbStart)
          publishCounter (prefix <> ".span") (ageFrom (metaCreated meta))
          publishCounter (prefix <> ".newest")
            (fromIntegral (unPosixEpochTime dbStart))

    -- Report shard stats for dynamic sharding assignment
    mapM_ (\(n,v) -> publishCounter (Text.encodeUtf8 n) v) $
      countersForShardSizes sm $
      Map.fromListWith (+) $
      [ (shard, bytes)
      | (item, shard) <- retentionLocal
      , Complete DatabaseComplete{databaseComplete_bytes = Just bytes} <-
          [metaCompleteness (itemMeta item)]
      ] ++
      [ (shard, 0)
      | shard <- toList myShards
      , shard `notElem` Set.fromList (map snd retentionLocal)]


mergeLocalAndRemote :: [(Repo, Meta)] -> [Item] -> [Item]
mergeLocalAndRemote backups localAndRestoring =
  sortOn (dbTime . itemMeta) $
      localAndRestoring ++
        [ Item repo Cloud meta ItemMissing
          -- DBs we could restore
        | (repo, meta) <- backups
        , repo `notElem` map itemRepo localAndRestoring  ]

data FetchBackups
  = ReusedPreviousBackups {
    reusedBackups :: ([(Repo, Meta)], UTCTime)
    }
  | FetchedNewBackups {
    newBackups :: [(Repo, Meta)]
    }
  | FetchFailure {
    previousGoodBackups :: Maybe ([(Repo, Meta)], UTCTime),
    fetchError :: SomeException
  }

-- Fetches backups only if they haven't been fetched recently
fetchBackups :: Env -> IO FetchBackups
fetchBackups env = do
  ServerConfig.Config{..} <- Observed.get (envServerConfig env)
  let syncPeriodSeconds = fromIntegral config_backup_list_sync_period
  maybeLastFetch <- readTVarIO (envCachedRestorableDBs env)
  now <- envGetCurrentTime env
  case maybeLastFetch of
    Nothing -> fetch now Nothing
    Just (lastFetch, dbs)
      | now `timeDiffInSeconds` lastFetch > syncPeriodSeconds ->
        fetch now (Just (dbs, lastFetch))
      | otherwise ->
        return $ ReusedPreviousBackups (dbs, lastFetch)
  where
    fetch now previousGood = do
      logInfo "fetching restorable databases list"
      dbs <- tryAll $
        loggingAction (runLogCmd "listRestorable" env) (const mempty) $
          concatMap HashMap.toList <$>
            forRestoreSitesM env mempty listRestorable

      when (null dbs) $
        logWarning "found no restorable databases"

      for_ dbs $ \dbs ->
        atomically $ writeTVar (envCachedRestorableDBs env) (Just (now,dbs))

      return $ case dbs of
        Right dbs -> FetchedNewBackups dbs
        Left error -> FetchFailure previousGood error

timeDiffInSeconds :: UTCTime -> UTCTime -> Int
timeDiffInSeconds t1 t2 =
  timeSpanInSeconds $ fromUTCTime t1 `timeDiff` fromUTCTime t2

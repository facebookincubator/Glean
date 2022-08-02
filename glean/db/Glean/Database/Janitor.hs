{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Database.Janitor
  ( runDatabaseJanitor
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Extra
import Data.Foldable as Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time

import ServiceData.GlobalStats as Stats
import Util.Control.Exception
import Util.List
import Util.Log
import Util.Logger
import Util.TimeSec (timeDiff)

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Close
import Glean.Database.Delete
import Glean.Database.List
import Glean.Database.Meta
import Glean.Database.Repo
import Glean.Database.Restore
import Glean.Database.Open
import Glean.Database.Types
import Glean.Internal.Types
import Glean.Logger
import Glean.Repo.Text
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.ShardManager
    ( ShardManager(getAssignedShards, dbToShard),
      SomeShardManager(SomeShardManager), BaseOfStack (BaseOfStack),
      countersForShardSizes, noSharding )
import Glean.Util.Time

{- |
The database janitor has the following functions:
  - ensure the newest db for each repo is open to speed-up queries to it.
  - close databases that haven't been used in a while.
  - kick-off downloads of dbs available in remote backups.
  - delete dbs we don't need any longer.
  - publish counters of local db states.
-}

runDatabaseJanitor :: Env -> IO ()
runDatabaseJanitor env@Env{envShardManager = SomeShardManager sm} = do
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
  -> IO ()
runWithShards env myShards sm = do
  loggingAction (runLogCmd "janitor" env) (const mempty) $ do
  logInfo "running database janitor"
  logInfo $ "Assigned shards: " <> show (toList myShards)

  config@ServerConfig.Config{..} <- Observed.get (envServerConfig env)

  let
    !ServerConfig.DatabaseRetentionPolicy{} = config_retention
    !ServerConfig.DatabaseRestorePolicy{} = config_restore
    !ServerConfig.DatabaseClosePolicy{..} = config_close

  backups <- fetchBackups env

  localAndRestoringByAge <- atomically $
    Catalog.list (envCatalog env) [Local,Restoring] $ do
      sortF createdV Ascending
      everythingF

  mostRecent <- fmap (Set.fromList . map itemRepo) $ atomically $
    Catalog.list (envCatalog env) [Local] $ groupF repoNameV $ do
      sortF createdV Descending
      limitF 1

  closeIdleDatabases env
     (seconds $ fromIntegral databaseClosePolicy_close_after)
     (toList mostRecent)

  t <- getCurrentTime

  -- on completion, record the time we last ran the janitor. This is
  -- used by the server to know when to advertise the server as alive.
  let done = atomically $ writeTVar (envDatabaseJanitor env) (Just t)
  flip finally done $ do

  let
    allDBsByAge :: [Item]
    allDBsByAge =
      localAndRestoringByAge ++
        [ Item repo Cloud meta ItemMissing -- DBs we could restore
        | (repo, meta) <- backups
        , repo `notElem` map itemRepo localAndRestoringByAge  ]

    byRepoAndAge = byRepoName allDBsByAge
    byRepoMap = Map.fromList $ [(itemRepo item, item) | item <- allDBsByAge]

    keepRoots = concatMap (dbKeepRoots config t) byRepoAndAge

    -- Ensure we keep dependencies for stacked dbs
    keep =
      transitiveClosureBy itemRepo (catMaybes . dependencies) keepRoots
    dependencies = stacked . metaDependencies . itemMeta
    stacked (Just (Thrift.Dependencies_stacked repo)) =
      [repo `Map.lookup` byRepoMap]
    stacked (Just (Thrift.Dependencies_pruned update)) =
      [pruned_base update `Map.lookup` byRepoMap]
    stacked Nothing = []

    keepAnnotatedWithShard =
      [ (item, guard (shard `Set.member` myShards) >> pure shard)
      | item <- keep
      -- We get Nothing when a dependency is missing, which should never happen.
      -- If it does a 'logWarning' will be emitted by 'checkDependencies' below
      , Just shard <- [itemToShard item]
      ]

    keepInThisNode =
      mapMaybe (\(item, shard) -> (item,) <$> shard) keepAnnotatedWithShard

    delete =
      [ repo | Item repo Local _ _ <- allDBsByAge
      , repo `notElem` map (itemRepo . fst) keepInThisNode ]

    fetch = filter (\(Item{..},_) -> itemLocality == Cloud) keepInThisNode

    -- itemToShard :: Item -> Maybe shard
    itemToShard item = do
      stack <- repoStack item
      return $ dbToShard sm (BaseOfStack $ last $ itemRepo item : stack)

    repoStack :: Item -> Maybe [Repo]
    repoStack Item{..} = case metaDependencies itemMeta of
      Just (Dependencies_stacked base) -> do
        baseItem <- Map.lookup base byRepoMap
        rest <- repoStack baseItem
        return (base : rest)

      Just (Dependencies_pruned Pruned{..}) -> do
        baseItem <- Map.lookup pruned_base byRepoMap
        rest <- repoStack baseItem
        return (pruned_base : rest)
      Nothing -> return []

    checkDependencies dbs msg1 msg2 = do
      let missingDependencies = nubOrdOn itemRepo
            [db | db <- dbs, Nothing <- dependencies db]
      unless (null missingDependencies) $
        logWarning $
          msg1 <> ": " <> show missingDependencies <> "\n" <> msg2

  checkDependencies keep
    "dependencies missing in Catalog"
    ("This suggests a dependency db has been deleted from the cloud, " <>
     "or a failure to enumerate the cloud catalog")

  checkDependencies (map fst keepInThisNode)
    "dependencies not downloaded"
    ("This probably means a bug in the db->shard mapping, " <>
     "or a failure to enumerate the cloud catalog")

  forM_ keepInThisNode $ \(Item{itemRepo, itemLocality}, _) ->
    when (itemLocality == Local) $
      atomically $ Catalog.unsetExpiring (envCatalog env) itemRepo

  forM_ delete $ \repo -> do
    let
      ServerConfig.Retention{..} =
        repoRetention config_retention $ Thrift.repo_name repo
    expireDatabase (fromIntegral <$> retention_expire_delay) env repo
      `catch` \UnknownDatabase{} ->
        -- DBs are deleted asynchronously and this code is not transactional,
        -- so it's possible that the DB is not in the Catalog at this point
        return ()

  restores <- fmap catMaybes $ forM fetch $ \(Item{..}, _) ->
    ifRestoreRepo env Nothing itemRepo $ do
      logInfo $ "Restoring: " ++ showRepo itemRepo ++
        " ("  ++ showNominalDiffTime (dbAge t itemMeta) ++ " old)"
      return $ Just $ Catalog.startRestoring (envCatalog env) itemRepo itemMeta
  -- register all the restoring DBs together in a single transaction,
  -- so that the backup thread can't jump in early and pick one
  atomically $ sequence_ restores

  atomically $ Catalog.resetElsewhere (envCatalog env) $
    [ item
      -- Nothing means the db is not in any of the shards assigned to this node
    | (item, Nothing) <- keepAnnotatedWithShard]

  forM_ byRepoAndAge $ \(repoNm, dbsByAge) -> do
    let prefix = "glean.db." <> Text.encodeUtf8 repoNm
    let repoKeep =
          filter (\item -> repoNm == Thrift.repo_name (itemRepo item)) keep

    -- Open the most recent local DB for each repo in order to avoid lag spikes
    let newestLocalDb = head dbsByAge
    when (itemRepo newestLocalDb `elem` mostRecent
        && itemLocality newestLocalDb == Local) $
      whenM (atomically $ isDatabaseClosed env $ itemRepo newestLocalDb)
        $ void
        $ tryAll
        $ logExceptions (inRepo $ itemRepo newestLocalDb)
        $ withOpenDatabase env (itemRepo newestLocalDb) (\_ -> return ())

    -- upsert counters
    void $ setCounter (prefix <> ".all") $ length repoKeep
    void $ setCounter (prefix <> ".available") $ length $ filter
      (\Item{..} ->
        itemLocality == Local
        && completenessStatus itemMeta == Thrift.DatabaseStatus_Complete)
      repoKeep
    void $ setCounter (prefix <> ".restoring") $
      length restores +
      length (filter (\Item{..} -> itemLocality == Restoring) dbsByAge)
    void $ setCounter (prefix <> ".indexing") $ length $ filter
      (\Item{..} ->
        itemLocality == Local
        && completenessStatus itemMeta == Thrift.DatabaseStatus_Incomplete)
      dbsByAge
    void $ setCounter (prefix <> ".backups") $ length $ filter
      (\Item{..} -> itemLocality == Cloud)
      dbsByAge
    let pt = fromUTCTime t
        dbAges =
          [ timeSpanInSeconds $ pt `timeDiff`
              Time (fromIntegral (unPosixEpochTime (metaCreated itemMeta)))
            | Item{itemLocality=Local, ..} <- dbsByAge ]
    unless (null dbAges) $ void $
      setCounter (prefix <> ".age") $ minimum dbAges

  -- Report shard stats for dynamic sharding assignment
  mapM_ (\(n,v) -> setCounter (Text.encodeUtf8 n) v) $
    countersForShardSizes sm $
    Map.fromListWith (+) $
    [ (shard, bytes)
    | (item, shard) <- keepInThisNode
    , Complete DatabaseComplete{databaseComplete_bytes = Just bytes} <-
        [metaCompleteness (itemMeta item)]
    ] ++
    [ (shard, 0)
    | shard <- toList myShards
    , shard `notElem` Set.fromList (map snd keepInThisNode)]

-- The target set of DBs we want usable on the disk. This is a set of
-- DBs that satisfies the policy.
--   - start from the set of DBs satisfying
--     delete_if_older and retain_at_most
--   - ensure that we have retain_at_least DBs
--   - ensure that we have retain_at_least local DBs
--     (i.e. avoid deleting local DBs while we wait for restores)
dbKeepRoots :: ServerConfig.Config -> UTCTime -> (Text, [Item]) -> [Item]
dbKeepRoots ServerConfig.Config{..} t (repoNm, dbs) = keepRoots
  where
    ServerConfig.Retention{..} = repoRetention config_retention repoNm
    retainAtLeast = fromIntegral $ fromMaybe 0 retention_retain_at_least
    retainAtMost = fmap fromIntegral retention_retain_at_most
    deleteIfOlder = fmap fromIntegral retention_delete_if_older
    deleteIncompleteIfOlder =
      fmap fromIntegral retention_delete_incomplete_if_older

    sorted = sortOn (Down . metaCreated . itemMeta) dbs

    keepAccordingToPolicy = filter (fresh . itemMeta) (dropExcess sorted)
      where
        dropExcess = maybe id take retainAtMost
        fresh meta
          | completenessStatus meta /= Thrift.DatabaseStatus_Complete
          , Just secs <- deleteIncompleteIfOlder
          , dbAge t meta >= secs = False
          | Just secs <- deleteIfOlder = dbAge t meta < secs
          | otherwise = True

    viableWith f Item{..} =
      f itemLocality
        && completenessStatus itemMeta == Thrift.DatabaseStatus_Complete

    viable = viableWith (const True)
    viableNow = viableWith (== Local)

    viableLocalOrRemoteDBs = filter viable sorted
    viableLocalDBs = filter viableNow sorted

    keepRoots =
      uniqBy (comparing itemRepo) $
      keepAccordingToPolicy
        ++ take retainAtLeast viableLocalOrRemoteDBs
        ++ take retainAtLeast viableLocalDBs


-- Fetches backups only if they haven't been fetched recently
fetchBackups :: Env -> IO [(Repo, Meta)]
fetchBackups env = do
  ServerConfig.Config{..} <- Observed.get (envServerConfig env)
  let syncPeriodSeconds = fromIntegral config_backup_list_sync_period
  maybeLastFetch <- readTVarIO (envCachedRestorableDBs env)
  now <- getCurrentTime
  case maybeLastFetch of
    Nothing -> fetch now
    Just (lastFetch, dbs)
      | now `timeDiffInSeconds` lastFetch > syncPeriodSeconds ->
        fetch now
      | otherwise -> return dbs
  where
    timeDiffInSeconds t1 t2 =
      timeSpanInSeconds $ fromUTCTime t1 `timeDiff` fromUTCTime t2
    fetch now = do
      logInfo "fetching restorable databases list"
      dbs <- loggingAction (runLogCmd "listRestorable" env) (const mempty) $
        concatMap HashMap.toList <$>
          forRestoreSitesM env mempty listRestorable
      atomically $ writeTVar (envCachedRestorableDBs env) (Just (now,dbs))
      return dbs

-- Group databases by repository name
byRepoName :: [Item] -> [(Text, [Item])]
byRepoName dbs = HashMap.toList $ HashMap.fromListWith (++)
  [ (Thrift.repo_name $ itemRepo item, [item]) | item <- dbs ]

repoRetention
  :: ServerConfig.DatabaseRetentionPolicy -> Text -> ServerConfig.Retention
repoRetention ServerConfig.DatabaseRetentionPolicy{..} repoNm =
  Map.findWithDefault
    databaseRetentionPolicy_default_retention
    repoNm
    databaseRetentionPolicy_repos

transitiveClosureBy
  :: (Eq k, Hashable k, Foldable f)
  => (a -> k) -> (a -> f a) -> [a] -> [a]
transitiveClosureBy k fn xs = HashMap.elems $ go xs HashMap.empty
  where
    go [] r = r
    go (n:ns) r
      | k n `HashMap.member` r = go ns r
      | otherwise =
        go
          (Foldable.toList (fn n) ++ ns)
          (HashMap.insert (k n) n r)

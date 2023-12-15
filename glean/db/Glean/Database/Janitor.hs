{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Database.Janitor
  ( runDatabaseJanitor
  , runDatabaseJanitorPureish
  , JanitorSideEffect(..)
  -- for testing
  , mergeLocalAndRemote
  , computeRetentionSet
  , dbIndex
  , DbIndex(..)
  ) where

import Control.Concurrent.Stream (stream)
import Control.Concurrent (getNumCapabilities)
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Writer.CPS
import Data.ByteString (ByteString)
import Data.Foldable as Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
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
import Util.STM
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
    ( ShardManager(getAssignedShards, computeShardMapping),
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

    allDBsByAge :: [Item]
    allDBsByAge = mergeLocalAndRemote backups localAndRestoring

    index@DbIndex{byRepo, byRepoName, dependencies} = dbIndex allDBsByAge

  (keep, cachedAvailable) <- computeRetentionSet
    config_retention config_restore now isAvailable index `runStateT` mempty

  atomically $ writeTVar
    (envCachedAvailableDBs env)
    (HashMap.keysSet $ HashMap.filter id cachedAvailable)

  let
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
      return $
        dbToShard (BaseOfStack $ last $ itemRepo item : stack) (itemRepo item)

    repoStack :: Item -> Maybe [Repo]
    repoStack Item{..} = case metaDependencies itemMeta of
      Just (Dependencies_stacked Stacked{..}) -> do
        let base = Thrift.Repo stacked_name stacked_hash
        baseItem <- Map.lookup base byRepo
        rest <- repoStack baseItem
        return (base : rest)

      Just (Dependencies_pruned Pruned{..}) -> do
        baseItem <- Map.lookup pruned_base byRepo
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
        " ("  ++ showNominalDiffTime (dbAge now itemMeta) ++ " old)"
      return $ Just $ Catalog.startRestoring (envCatalog env) itemRepo itemMeta
  -- register all the restoring DBs together in a single transaction,
  -- so that the backup thread can't jump in early and pick one
  atomically $ sequence_ restores

  let elsewhereDBs =
        [ item
          -- Nothing means it is not in any of the shards assigned to this node
        | (item, Nothing) <- keepAnnotatedWithShard
        ]

  atomically $ Catalog.resetElsewhere (envCatalog env) elsewhereDBs

  deleting <- readTVarIO (envDeleting env)

  -- Open the most recent local, complete DB for each repo in order
  -- to avoid lag spikes. This will be the DB that clients will get
  -- by default unless they specify a particular DB instance.
  let
      toOpen =
        [ item
        | (_, dbsByAge) <- byRepoName
        , item : _ <- [filter shouldOpen (NonEmpty.toList dbsByAge)]
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
            filter (\item -> repoNm == Thrift.repo_name (itemRepo item)) keep

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
              dbConceived = metaRepoHashTime meta
              dbCreated = metaCreated meta
              dbStart = fromMaybe dbCreated dbConceived
              ageFrom t0 = timeSpanInSeconds $
                fromUTCTime now `timeDiff` posixEpochTimeToTime t0
          -- .age is the age of the data, .span is the age of the DB
          publishCounter (prefix <> ".age") (ageFrom dbStart)
          publishCounter (prefix <> ".span") (ageFrom dbCreated)
          publishCounter (prefix <> ".newest")
            (fromIntegral (unPosixEpochTime dbStart))

    -- Report shard stats for dynamic sharding assignment
    mapM_ (\(n,v) -> publishCounter (Text.encodeUtf8 n) v) $
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


mergeLocalAndRemote :: [(Repo, Meta)] -> [Item] -> [Item]
mergeLocalAndRemote backups localAndRestoring =
  sortOn (metaCreated . itemMeta) $
      localAndRestoring ++
        [ Item repo Cloud meta ItemMissing
          -- DBs we could restore
        | (repo, meta) <- backups
        , repo `notElem` map itemRepo localAndRestoring  ]

-- | Information about the set of DBs that we have
data DbIndex = DbIndex
  { byRepo :: Map.Map Repo Item
    -- ^ Items indexed by Repo
  , byRepoName :: [(Text, NonEmpty Item)]
    -- ^ Items grouped by Repo name
  , dependencies :: Item -> [Maybe Item]
    -- ^ DB dependencies, Nothing indicates that there is a dependency
    -- but it is missing in the catalog.
  }

dbIndex :: [Item] -> DbIndex
dbIndex items = DbIndex{..}
  where
    byRepo =
      Map.fromListWith pickFromDuplicates
        $ [(itemRepo item, item) | item <- items]
    pickFromDuplicates x y = -- Pick local one in a conflict
      minimumBy (\x y -> compare (itemLocality x) (itemLocality y)) [x, y]

    byRepoName =
      HashMap.toList $ HashMap.fromListWith (<>)
        [ (Thrift.repo_name $ itemRepo item, item :| []) | item <- items ]

    dependencies = stacked . metaDependencies . itemMeta

    stacked (Just (Thrift.Dependencies_stacked Thrift.Stacked{..})) =
      [ Thrift.Repo stacked_name stacked_hash `Map.lookup` byRepo ]
    stacked (Just (Thrift.Dependencies_pruned update)) =
      [ pruned_base update `Map.lookup` byRepo ]
    stacked Nothing = []

-- | The final set of DBs we want usable on disk.
--  This is the set of 'keepRoots' DB extended with all the stacked dependencies
computeRetentionSet
  :: Monad m
  => ServerConfig.DatabaseRetentionPolicy
  -> ServerConfig.DatabaseRestorePolicy
  -> UTCTime
  -> (Repo -> m Bool)
  -> DbIndex
  -> StateT (HashMap.HashMap Repo Bool) m [Item]
computeRetentionSet config_retention config_restore
    time isAvailableM DbIndex{..} =
  transitiveClosureBy itemRepo (catMaybes . depsRestored) <$>
    concatMapM
      (\(repoNm, dbs) ->
        dbRetentionForRepo
          (repoRetention config_retention repoNm)
          time
          isAvailableM
          dbs
      )
      byRepoName
  where
    -- Add transitive dependencies of a DB to the retention set only
    -- if the DB is local or will be restored.
    depsRestored :: Item -> [Maybe Item]
    depsRestored item@Item{..}
      | itemLocality == Local
        || restorable config_restore itemRepo = dependencies item
      | otherwise = []

-- | The target set of DBs we want usable on the disk. This is a set of
-- DBs that satisfies the policy.
dbRetentionForRepo
  :: Monad m
  => ServerConfig.Retention
  -> UTCTime
  -> (Repo -> m Bool)
  -> NonEmpty Item
  -> StateT (HashMap.HashMap Repo Bool) m [Item]
dbRetentionForRepo ServerConfig.Retention{..} t isAvailableM dbs = do
  let
    itemAvailable Item{itemRepo} = do
      st <- State.get
      case HashMap.lookup itemRepo st of
        Just isAvailable -> return isAvailable
        Nothing -> do
          isAvailable <- lift $ isAvailableM itemRepo
          State.put $! HashMap.insert itemRepo isAvailable st
          return isAvailable

    -- retention policy parameters
    retainAtLeast' = fromIntegral $ fromMaybe 0 retention_retain_at_least
    retainAtMost' = fmap fromIntegral retention_retain_at_most
    -- enforce invariant: atLeast <= atMost
    retainAtLeast = min retainAtLeast' (fromMaybe maxBound retainAtMost')
    retainAtMost  = max retainAtLeast' <$> retainAtMost'
    deleteIfOlder = fmap fromIntegral retention_delete_if_older
    deleteIncompleteIfOlder =
      fmap fromIntegral retention_delete_incomplete_if_older

    f &&& g = \x -> f x && g x
    f ||| g = \x -> f x || g x
    f &&&> g = \x -> if not(f x) then return False else g x
    f |||> g = \x -> if f x then return True else g x

    ifSet (Just a) f = f a
    ifSet Nothing _ = const False

    -- predicates
    isLocal Item{..} = itemLocality == Local
    isComplete Item{..} =
      completenessStatus itemMeta == Thrift.DatabaseStatus_Complete
    isOlderThan secs Item{..} = dbAge t itemMeta >= secs
    isAvailable = isLocal |||> itemAvailable

    -- all DBs with the required properties, sorted by most recent first
    sorted =
      sortOn (Down . metaCreated . itemMeta) $
      filter (hasProperties retention_required_properties) $
      NonEmpty.toList dbs

    hasProperties req Item{..} = all has (HashMap.toList req)
      where
      has (name,val) =
        HashMap.lookup name (metaProperties itemMeta) == Just val

    -- whether to delete a DB according to the deletion policy
    delete =
      ifSet deleteIfOlder isOlderThan |||
      (ifSet deleteIncompleteIfOlder $ \secs ->
        (not . isComplete) &&& isOlderThan secs)

    -- ensure we have retain_at_least DBs from the available set
  atLeast <- takeFilterM
    retainAtLeast
    (isComplete &&&> isAvailable)
    -- bound the search since isAvailable is expensive
    -- this matters only for tier bootstraps where all DBs are unavailable
    (take (retainAtLeast*10) sorted)

    -- delete DBs according to the deletion policy, and keep retain_at_most
  let atMost = maybe id take retainAtMost (filter (not . delete) sorted)

  return $ uniqBy (comparing itemRepo) (atLeast ++ atMost)


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

      for_ dbs $ \dbs ->
        atomically $ writeTVar (envCachedRestorableDBs env) (Just (now,dbs))

      return $ case dbs of
        Right dbs -> FetchedNewBackups dbs
        Left error -> FetchFailure previousGood error

timeDiffInSeconds :: UTCTime -> UTCTime -> Int
timeDiffInSeconds t1 t2 =
  timeSpanInSeconds $ fromUTCTime t1 `timeDiff` fromUTCTime t2

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

-- | Take the first n items that satisfy the predicate
takeFilterM :: (Monad m) => Int -> (a -> m Bool) -> [a] -> m [a]
takeFilterM n pred = loop [] n where
  loop acc _ [] = return (reverse acc)
  loop acc 0 _ = return (reverse acc)
  loop acc n (x:xx) = do
    accept <- pred x
    if accept
      then loop (x:acc) (n-1) xx
      else loop acc n xx

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
  , RetentionSet(..)
  , dbIndex
  , DbIndex(..)
  ) where

import Control.Concurrent.Stream (stream)
import Control.Concurrent (getNumCapabilities)
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class (liftIO)
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
  logInfo "running database janitor"
  logInfo $ "Assigned shards: " <> show (toList myShards)

  ServerConfig.Config{..} <- Observed.get (envServerConfig env)

  let
    !ServerConfig.DatabaseRetentionPolicy{} = config_retention
    !ServerConfig.DatabaseRestorePolicy{} = config_restore
    !ServerConfig.DatabaseClosePolicy{..} = config_close

  backups <- fetchBackups env

  localAndRestoring <- atomically $
    Catalog.list (envCatalog env) [Local,Restoring] everythingF

  t <- envGetCurrentTime env

  dbToShard <- computeShardMapping sm

  -- on completion, record the time we last ran the janitor. This is
  -- used by the server to know when to advertise the server as alive.
  let done = atomically $ writeTVar (envDatabaseJanitor env) (Just t)
  flip finally done $ do

  let
    allDBsByAge :: [Item]
    allDBsByAge = mergeLocalAndRemote backups localAndRestoring

    index@DbIndex{byRepo, byRepoName, dependencies} = dbIndex allDBsByAge
    RetentionSet{retentionSetRemote, retentionSetLocalAndRemote = keep} =
      computeRetentionSet config_retention t backups index

    keepAnnotatedWithShard =
      [ (item, guard (shard `Set.member` myShards) >> pure shard)
      | item <- keep
      -- We get Nothing when a dependency is missing, which should never happen.
      -- If it does a 'logWarning' will be emitted by 'checkDependencies' below
      , Just shard <- [itemToShard item]
      ]

    keepInThisNode =
      mapMaybe (\(item, shard) -> (item,) <$> shard) keepAnnotatedWithShard

    retentionSlackInThisNode =
      [ item
      | (item@Item{itemRepo}, _) <- keepInThisNode
      , not $ HashSet.member itemRepo retentionSetRemote ]

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
        " ("  ++ showNominalDiffTime (dbAge t itemMeta) ++ " old)"
      return $ Just $ Catalog.startRestoring (envCatalog env) itemRepo itemMeta
  -- register all the restoring DBs together in a single transaction,
  -- so that the backup thread can't jump in early and pick one
  atomically $ sequence_ restores

  atomically $ Catalog.resetElsewhere (envCatalog env) $
    [ item
      -- Nothing means the db is not in any of the shards assigned to this node
    | (item, Nothing) <- keepAnnotatedWithShard]

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

    -- see "Retention set slack" note
    let totalSlack = sum
            [ fromIntegral b
            | Item{ itemMeta = Meta {
                  metaCompleteness =
                    Complete DatabaseComplete{databaseComplete_bytes = Just b}
                  }
                } <- retentionSlackInThisNode
            ]
    publishCounter "glean.db.slack.bytes" totalSlack
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
        db:_ ->
          let dbCreated = posixEpochTimeToTime (metaCreated $ itemMeta db)
              dbAge = timeSpanInSeconds $ fromUTCTime t `timeDiff` dbCreated
          in
          publishCounter (prefix <> ".age") dbAge

      -- see "Retention set slack" note
      let leaked = sum
            [ fromIntegral b
            | Item{
                itemRepo,
                itemMeta = Meta {
                  metaCompleteness =
                    Complete DatabaseComplete{databaseComplete_bytes = Just b}
                  }
                } <- retentionSlackInThisNode
            , repo_name itemRepo == repoNm
            ]
      unless (leaked == 0) $ publishCounter (prefix <> ".slack.bytes") leaked

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
        [ Item repo Cloud meta ItemMissing -- DBs we could restore
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

{- NOTE Retention set slack

  The slack is the difference between the remote and local+remote
  retention sets. It is only justified  when no other shard is
  serving DB instances for a given DB name. This happens very rarely
  with hash-based sharding, and constitutes a resource leak that we track.

  Example using sharding per DB name:

  Remote Retention Set: [DB1(I1, I2), DB2(I1,I2), DB3(I1)]
  Shard 1: DB1(I1,I2)
  Shard 2: DB2(I1,I2)
  Shard 3: DB3(I1)

  --> Event: DB1(I3) gets published

  Retention Set: [DB1(I2, I3), DB2(I1,I2), DB3(I1)]
  Shard 1: DB1(I2, *I3*) --I3 is getting restored
  Shard 2: DB2(I1,I2)
  Shard 3: DB3(I1)

  --> Event: DB1(I4) gets published
  Retention Set: [DB1(I3, I4), DB2(I1,I2), DB3(I1)]
  Shard 1: DB1(*I3*, *I4*) --I3 and I4 are getting restored
           -- in this case the local retention set will include DB1(I2)
           -- to continue serving DB1 until I3 and I4 are restored
  Shard 2: DB2(I1,I2)
  Shard 3: DB3(I1)

  A different sharding policy for the same event sequence could render:

  Remote Retention Set: [DB1(I1, I2), DB2(I1,I2), DB3(I1)]
  Shard 1: DB1(I1,I2)
  Shard 2: DB2(I1,I2)
  Shard 3: DB3(I1)

  --> Event: DB1(I3) gets published
  --> Event: DB1(I4) gets published
  --> Event: DB1(I3) gets restored
  --> Event: DB1(I4) gets restored

  Retention Set: [DB1(I2, I3), DB2(I1,I2), DB3(I1)]
  Shard 1: DB1(I2)
           -- DB1(I2) is still in the local retention set after DB1(I3)
           -- has been restored, which constitutes a leak.
  Shard 2: DB1(I4), DB2(I1,I2)
  Shard 3: DB1(I3), DB3(I1)
  --
 -}

data RetentionSet = RetentionSet
  { retentionSetRemote :: HashSet.HashSet Repo
  -- ^ The retention set ignoring local DBs
  , retentionSetLocalAndRemote :: [Item]
  -- ^ The retention set that applies to this server
  }
-- | The final set of DBs we want usable on disk.
--  This is the set of 'keepRoots' DB extended with all the stacked dependencies
computeRetentionSet
  :: ServerConfig.DatabaseRetentionPolicy
  -> UTCTime
  -> [(Repo, Meta)] -- ^ Remote backups
  -> DbIndex
  -> RetentionSet
computeRetentionSet config_retention time backups DbIndex{..} = RetentionSet{..}
  where
    retentionSetRemote =
      HashSet.fromList $ map itemRepo $ retentionSet backupItemsByRepoName
    retentionSetLocalAndRemote = retentionSet byRepoName
    retentionSet dbsByRepoName =
      transitiveClosureBy itemRepo (catMaybes . dependencies) $
        concatMap
          (\(repoNm, dbs) ->
            dbKeepRoots (repoRetention config_retention repoNm) time
              dbs
          )
          dbsByRepoName
    backupItemsByRepoName = Map.toList $ Map.fromListWith (<>)
      [ (repo_name itemRepo, pure Item{..})
      | let itemLocality = Cloud
      , let itemStatus = ItemComplete
      , (itemRepo, itemMeta) <- backups
      ]

-- The target set of DBs we want usable on the disk. This is a set of
-- DBs that satisfies the policy.
--   - start from the set of DBs satisfying
--     delete_if_older and retain_at_most
--   - ensure that we have retain_at_least DBs
--   - ensure that we have retain_at_least local DBs
--     (i.e. avoid deleting local DBs while we wait for restores)
dbKeepRoots
  :: ServerConfig.Retention
  -> UTCTime
  -> NonEmpty Item
  -> [Item]
dbKeepRoots ServerConfig.Retention{..} t dbs = keepRoots
  where
    retainAtLeast = fromIntegral $ fromMaybe 0 retention_retain_at_least
    retainAtMost = fmap fromIntegral retention_retain_at_most
    deleteIfOlder = fmap fromIntegral retention_delete_if_older
    deleteIncompleteIfOlder =
      fmap fromIntegral retention_delete_incomplete_if_older

    sorted = sortOn (Down . metaCreated . itemMeta) (NonEmpty.toList dbs)

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
  now <- envGetCurrentTime env
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

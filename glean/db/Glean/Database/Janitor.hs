{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
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
import Glean.Logger
import Glean.Repo.Text
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.Time


runDatabaseJanitor :: Env -> IO ()
runDatabaseJanitor env =
  loggingAction (runLogCmd "janitor" env) (const mempty) $ do
  logInfo "running database janitor"

  config@ServerConfig.Config{..} <- Observed.get (envServerConfig env)

  let
    !ServerConfig.DatabaseRetentionPolicy{..} = config_retention
    !ServerConfig.DatabaseRestorePolicy{..} = config_restore
    !ServerConfig.DatabaseClosePolicy{..} = config_close

  mostRecent <- newestDbs env

  -- Make sure the most recent DB for each repo name is open
  forM_ mostRecent $ \repo ->
    whenM (atomically $ isDatabaseClosed env repo)
      $ void
      $ tryAll
      $ logExceptions (inRepo repo)
      $ withOpenDatabase env repo (\_ -> return ())

  closeIdleDatabases env
     (seconds $ fromIntegral databaseClosePolicy_close_after) mostRecent

  backups <- fromMaybe [] <$> fetchBackups env

  localAndRestoring <- atomically $
    Catalog.list (envCatalog env) [Local,Restoring] everythingF

  t <- getCurrentTime

  -- on completion, record the time we last ran the janitor. This is
  -- used by the server to know when to advertise the server as alive.
  let done = atomically $ writeTVar (envDatabaseJanitor env) (Just t)
  flip finally done $ do

  let
    allDBs :: [Item]
    allDBs =
      localAndRestoring ++
        [ Item repo Cloud meta ItemMissing -- DBs we could restore
        | (repo, meta) <- backups
        , repo `notElem` map itemRepo localAndRestoring  ]

    byRepo = byRepoName allDBs

    keepRoots = concatMap (dbKeepRoots config t) byRepo

    -- Ensure we keep dependencies for stacked dbs
    keep = transitiveClosureBy itemRepo (catMaybes . dependencies) keepRoots
    dependencies = stacked . metaDependencies . itemMeta
    stacked (Just (Thrift.Dependencies_stacked repo)) =
      [repo `Map.lookup` repoMap]
    stacked (Just (Thrift.Dependencies_pruned update)) =
      [pruned_base update `Map.lookup` repoMap]
    stacked Nothing = []
    repoMap =
      Map.fromList $ map (\item -> (itemRepo item, item)) allDBs

    missingDependencies = any isNothing $ concatMap dependencies keep
    delete =
      [ repo | Item repo Local _ _ <- allDBs, repo `notElem` map itemRepo keep ]
    fetch = filter ((==Cloud) . itemLocality) keep

  when missingDependencies $ logInfo "some dbs are missing dependencies"

  forM_ delete $ \repo -> do
    let
      ServerConfig.Retention{..} =
        repoRetention config_retention $ Thrift.repo_name repo
    expireDatabase (fromIntegral <$> retention_expire_delay) env repo

  restores <- fmap catMaybes $ forM fetch $ \Item{..} ->
    ifRestoreRepo env Nothing itemRepo $ do
      logInfo $ "Restoring: " ++ showRepo itemRepo ++
        " ("  ++ showNominalDiffTime (dbAge t itemMeta) ++ " old)"
      return $ Just $ Catalog.startRestoring (envCatalog env) itemRepo itemMeta
  -- register all the restoring DBs together in a single transaction,
  -- so that the backup thread can't jump in early and pick one
  atomically $ sequence_ restores

  forM_ byRepo $ \(repoNm, dbs) -> do
    let prefix = "glean.db." <> Text.encodeUtf8 repoNm
    let repoKeep =
          filter (\item -> repoNm == Thrift.repo_name (itemRepo item)) keep
    void $ setCounter (prefix <> ".all") $ length repoKeep
    void $ setCounter (prefix <> ".available") $ length $ filter
      (\Item{..} ->
        itemLocality == Local
        && completenessStatus itemMeta == Thrift.DatabaseStatus_Complete)
      repoKeep
    void $ setCounter (prefix <> ".restoring") $
      length restores +
      length (filter (\Item{..} -> itemLocality == Restoring) dbs)
    void $ setCounter (prefix <> ".indexing") $ length $ filter
      (\Item{..} ->
        itemLocality == Local
        && completenessStatus itemMeta == Thrift.DatabaseStatus_Incomplete)
      dbs
    void $ setCounter (prefix <> ".backups") $ length $ filter
      (\Item{..} -> itemLocality == Cloud)
      dbs
    let pt = fromUTCTime t
        dbAges =
          [ timeSpanInSeconds $ pt `timeDiff`
              Time (fromIntegral (unPosixEpochTime (metaCreated itemMeta)))
            | Item{itemLocality=Local, ..} <- dbs ]
    unless (null dbAges) $ void $
      setCounter (prefix <> ".age") $ minimum dbAges


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
fetchBackups :: Env -> IO (Maybe [(Repo, Meta)])
fetchBackups env = do
  ServerConfig.Config{..} <- Observed.get (envServerConfig env)
  let syncPeriodSeconds = fromIntegral config_backup_list_sync_period
  maybeLastFetch <- readTVarIO (envLastBackupsSync env)
  now <- getCurrentTime
  case maybeLastFetch of
    Nothing -> fetch now
    Just lastFetch | now `timeDiffInSeconds` lastFetch > syncPeriodSeconds ->
        fetch now
    _ ->
      return Nothing
  where
    timeDiffInSeconds t1 t2 =
      timeSpanInSeconds $ fromUTCTime t1 `timeDiff` fromUTCTime t2
    fetch now = do
      logInfo "fetching restorable databases list"
      atomically $ writeTVar (envLastBackupsSync env) (Just now)
      loggingAction (runLogCmd "listRestorable" env) (const mempty) $
        Just . concatMap HashMap.toList <$>
          forRestoreSitesM env mempty listRestorable

-- Group databases by repository name
byRepoName :: [Item] -> [(Text, [Item])]
byRepoName dbs = HashMap.toList $ HashMap.fromListWith (++)
  [ (Thrift.repo_name $ itemRepo item, [item]) | item <- dbs ]

-- Most recent available DB for each repo name
newestDbs :: Env -> IO [Repo]
newestDbs env = fmap (map itemRepo) $ atomically $
  Catalog.list (envCatalog env) [Local] $
    groupF repoNameV $ do
      sortF createdV Descending
      limitF 1

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

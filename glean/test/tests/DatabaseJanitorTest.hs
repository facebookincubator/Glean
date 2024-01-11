{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module DatabaseJanitorTest
  ( main
  , dbConfig
  , serverConfig
  , makeFakeDB
  , withTest
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import GHC.Stack (HasCallStack)
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Time.Extra (sleep)
import System.Timeout
import Test.HUnit

import ServiceData.GlobalStats (getCounters)
import TestRunner
import Util.EventBase
import Util.TimeSec

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Data
import Glean.Database.Delete
import Glean.Database.Env
import Glean.Database.Janitor
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import qualified Glean.Database.Storage.RocksDB as RocksDB
import Glean.Database.Close
import Glean.Database.List
import Glean.Database.Open (isDatabaseClosed, withOpenDatabase)
import Glean.Database.Types
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Init
import Glean.RTS.Types (lowestFid)
import Glean.ServerConfig.Types as ServerTypes
import Glean.Internal.Types
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.ShardManager
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.Time (seconds)
import Glean.Database.Backup.Backend
import Glean.Database.Backup.Mock


withTest
  :: (FilePath -> IO ())
  -> (FilePath -> IO ())
  -> (EventBaseDataplane -> NullConfigProvider -> FilePath -> FilePath
       -> IO ())
  -> IO ()
withTest setup setupBackup action =
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \cfgAPI ->
  withSystemTempDirectory "glean-dbtest" $ \dbdir -> do
  withSystemTempDirectory "glean-dbtest-backup" $ \backupdir -> do
    setup dbdir
    setupBackup backupdir
    action evb cfgAPI dbdir backupdir

broken :: UTCTime -> Completeness
broken _ = Broken (DatabaseBroken "index" "TESTING")

complete :: Int64 -> UTCTime -> Completeness
complete size =
  Complete . (`DatabaseComplete` Just size) . utcTimeToPosixEpochTime

repo0001 :: Repo
repo0001 = Repo "test" "0001"

setupBasicDBs :: FilePath -> IO ()
setupBasicDBs dbdir = do
  now <- getCurrentTime
  let age t = addUTCTime (negate (fromIntegral (timeSpanInSeconds t))) now
  schema <- parseSchemaDir schemaSourceDir
  schema <- newDbSchema Nothing schema LatestSchemaAll readWriteContent
  -- populate a dir with various DBs
  makeFakeDB schema dbdir repo0001 (age (days 0)) (complete 1)
    (props [("bool","yes")])
  makeFakeDB schema dbdir (Repo "test" "0002") (age (days 2)) broken id
  makeFakeDB schema dbdir (Repo "test" "0003") (age (days 3)) (complete 3) $
    stacked (Stacked "test" "0004" Nothing)
  makeFakeDB schema dbdir (Repo "test" "0004") (age (days 4)) (complete 4) $
    stacked (Stacked "test" "0005" Nothing)
  makeFakeDB schema dbdir (Repo "test" "0005") (age (days 5)) (complete 5) $
    stacked (Stacked "test2" "0006" Nothing) . props [("bool","no")]
  makeFakeDB schema dbdir (Repo "test2" "0006") (age (days 6)) (complete 6) id


setupBasicCloudDBs :: FilePath -> IO ()
setupBasicCloudDBs backupDir = do
  now <- getCurrentTime
  let age t = addUTCTime (negate (fromIntegral (timeSpanInSeconds t))) now
  schema <- parseSchemaDir schemaSourceDir
  schema <- newDbSchema Nothing schema LatestSchemaAll readWriteContent
  makeFakeCloudDB schema backupDir (Repo "test" "0008")
    (age(days 8)) (complete 8) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0009")
    (age(days 7)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0010")
    (age(days 6)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0011")
    (age(days 5)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0012")
    (age(days 4)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0013")
    (age(days 3)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test2" "0014")
    (age(days 2)) (complete 9) id
  makeFakeCloudDB schema backupDir (Repo "test" "0015")
    (age(days 7)) (complete 7) (stacked (Stacked "test2" "0013" Nothing))

withFakeDBs
  :: (EventBaseDataplane -> NullConfigProvider -> FilePath -> FilePath
       -> IO ())
  -> IO ()
withFakeDBs action = withTest setupBasicDBs (const $ pure ()) action

withFakeCloudDBs
  :: (EventBaseDataplane -> NullConfigProvider -> FilePath -> FilePath
       -> IO ())
  -> IO ()
withFakeCloudDBs = withTest (const $ pure ()) setupBasicCloudDBs

stacked :: Stacked -> Meta -> Meta
stacked st meta = meta { metaDependencies = Just (Thrift.Dependencies_stacked st) }

props :: [(Text, Text)] -> Meta -> Meta
props list meta = meta { metaProperties = HashMap.fromList list }

makeFakeDB
  :: DbSchema
  -> FilePath
  -> Repo
  -> UTCTime
  -> (UTCTime -> Completeness)
  -> (Meta -> Meta)
  -> IO ()
makeFakeDB schema root repo dbtime completeness opts = do
  let
    meta = opts $ Meta
      { metaVersion = Storage.currentVersion
      , metaCreated = utcTimeToPosixEpochTime dbtime
      , metaRepoHashTime = Nothing
      , metaCompleteness = completeness dbtime
      , metaBackup = Nothing
      , metaProperties = HashMap.empty
      , metaDependencies = Nothing
      , metaCompletePredicates = mempty
      , metaAxiomComplete = False
      }
  let repoPath = databasePath root repo
  createDirectoryIfMissing True repoPath
  storage <- RocksDB.newStorage root def
  bracket
    (Storage.open
      storage
      repo
      (Storage.Create lowestFid Nothing Storage.UseDefaultSchema)
      Storage.currentVersion)
    Storage.close
    (\hdl -> storeSchema hdl $ toStoredSchema schema)
  LB.writeFile (repoPath </> "meta") (encode meta)

makeFakeCloudDB
  :: DbSchema
  -> FilePath
  -> Repo
  -> UTCTime
  -> (UTCTime -> Completeness)
  -> (Meta -> Meta)
  -> IO ()
makeFakeCloudDB schema backupDir repo dbtime completeness opts = do
  let repoPath = databasePath backupDir repo
  createDirectoryIfMissing True repoPath
  storage <- RocksDB.newStorage backupDir def
  bracket
    (Storage.open
      storage
      repo
      (Storage.Create lowestFid Nothing Storage.UseDefaultSchema)
      Storage.currentVersion)
    Storage.close
    (\hdl -> do
      storeSchema hdl $ toStoredSchema schema
      tmpDir <- getCanonicalTemporaryDirectory
      withTempDirectory tmpDir "scratch" $ \scratch ->
        Storage.backup hdl scratch $ \file _data ->
          void $ backup (mockSite backupDir) repo props Nothing file
    )
  where
    props = Map.fromList [
      ("meta"::String, LBS.unpack $ encode meta) ]
    meta = opts $ Meta
        { metaVersion = Storage.currentVersion
        , metaCreated = utcTimeToPosixEpochTime dbtime
        , metaRepoHashTime = Nothing
        , metaCompleteness = completeness dbtime
        , metaBackup = Nothing
        , metaProperties = HashMap.empty
        , metaDependencies = Nothing
        , metaCompletePredicates = mempty
        , metaAxiomComplete = False
        }

dbConfig :: FilePath -> ServerTypes.Config -> Glean.Database.Config.Config
dbConfig dbdir serverConfig = def
  { cfgDataStore = fileDataStore dbdir
  , cfgSchemaSource = schemaSourceFiles
  , cfgRecipeConfig = def
  , cfgServerConfig = ThriftSource.value serverConfig
  , cfgReadOnly = False
      -- If we set cfgReadOnly=True, then backing up a DB will fail
      -- because backup first opens the DB, and opening a DB tries to
      -- load the schema from the DB.  If we use read/write mode, then
      -- opening will commit the schema instead of failing.
  , cfgMockWrites = False
  , cfgListener = mempty
  }

serverConfig :: FilePath -> ServerTypes.Config
serverConfig backupdir = def
  { config_backup = def
    { databaseBackupPolicy_allowed = mempty
    , databaseBackupPolicy_location = "mock:" <> Text.pack backupdir
    }
  , config_restore = def
    { databaseRestorePolicy_enabled = False
    }
  , config_janitor_period = Nothing -- no auto janitor, we'll run it manually
  , config_db_rocksdb_cache_mb = 0
  }

listDBs :: Env -> IO [Thrift.Database]
listDBs env = filter hereDBs <$> listAllDBs env
  where
    hereDBs Database{..} = database_status /= DatabaseStatus_Available

listHereDBs :: Env -> IO [Database]
listHereDBs = listDBs

listAllDBs :: Env -> IO [Database]
listAllDBs env = listDatabasesResult_databases <$> listDatabases env def

waitDel :: Env -> IO ()
waitDel env = atomically $ do
  done <- HashMap.null <$> readTVar (envDeleting env)
  when (not done) retry

deleteOldDBsTest :: Test
deleteOldDBsTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 1 }
          }
        }
  withDatabases evb cfg cfgAPI $ \env -> do

  dbs <- listDBs env
  assertEqual "before"
    [ "0001", "0002", "0003", "0004", "0005", "0006"]
    (sort (map (repo_hash . database_repo) dbs))

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  assertEqual "after"
    [ "0001" ]
    (sort (map (repo_hash . database_repo) dbs))

  dbdirs1 <- listDirectory (dbdir </> "test")
  dbdirs2 <- listDirectory (dbdir </> "test2")
  assertEqual "directories deleted" (dbdirs1 ++ dbdirs2) [ "0001" ]

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  assertEqual "after-repeat"
    [ "0001" ]
    (sort (map (repo_hash . database_repo) dbs))

deleteIncompleteDBsTest :: Test
deleteIncompleteDBsTest = TestCase $
  withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 10
            , retention_delete_incomplete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 1 }
          }
        }
  withDatabases evb cfg cfgAPI $ \env -> do

  dbs <- listDBs env
  assertEqual "before"
    [ "0001", "0002", "0003", "0004", "0005", "0006"]
    (sort (map (repo_hash . database_repo) dbs))

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  print (map database_repo dbs)
  assertEqual "after"
    [ "0001", "0003", "0004", "0005", "0006"]
    (sort (map (repo_hash . database_repo) dbs))

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  assertEqual "after-repeat"
    [ "0001", "0003", "0004", "0005", "0006"]
    (sort $ map (repo_hash . database_repo) dbs)


retainAtMostTest :: Test
retainAtMostTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_retain_at_most = Just 2 }
          }
        }
  withDatabases evb cfg cfgAPI $ \env -> do

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  let repos = map database_repo dbs
  assertEqual "after"
    [ "0001", "0002", "0006"]
    (sort $ map repo_hash repos)

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  assertEqual "after-repeat"
    [ "0001", "0002", "0006"]
    (sort $ map (repo_hash . database_repo) dbs)


retainAtLeastTest :: Test
retainAtLeastTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 1
            , retention_retain_at_least = Just 2 }
          }
        }
  withDatabases evb cfg cfgAPI $ \env -> do

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  let repos = map database_repo dbs
  assertEqual "after"
    [ "0001", "0003", "0004", "0005", "0006"]
    (sort $ map repo_hash repos)
    -- should drop 0002, because its metaIndexing=Failed
    -- should retain 0003 even though it is older than delete_if_older,
    -- due to retain_at_least.

  runDatabaseJanitor env
  waitDel env
  dbs <- listDBs env
  let repos = map database_repo dbs
  assertEqual "after-repeat"
    [ "0001", "0003", "0004", "0005", "0006"]
    (sort $ map repo_hash repos)

requiredPropsTest :: Test
requiredPropsTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_required_properties =
                HashMap.fromList [("bool","yes")]
            }
          }
        }
  withDatabases evb cfg cfgAPI $ \env -> do
  runDatabaseJanitor env
  dbs <- listDBs env
  let repos = map database_repo dbs
  assertEqual "after" [ "0001" ] (map repo_hash repos)

multiRetentionTest :: Test
multiRetentionTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_by_repo =
            Map.fromList
              [ ("test",
                [ def
                  { retention_required_properties =
                      HashMap.fromList [("bool","yes")]
                  , retention_retain_at_least = Just 1
                  }
                , def
                  { retention_required_properties =
                      HashMap.fromList [("bool","no")]
                  , retention_retain_at_least = Just 1
                  , retention_keep_open = True
                  }
                ])
              ]
            }
          }
  withDatabases evb cfg cfgAPI $ \env -> do
  runDatabaseJanitor env
  dbs <- listDBs env
  let repos = sort $ map (repo_hash . database_repo) dbs
  assertEqual "after" [ "0001", "0005", "0006" ] repos

  -- 0005 should be open due to keep_open=True in the retention policy
  closed <- atomically $ isDatabaseClosed env (Repo "test" "0005")
  assertBool "open" (not closed)

-- | If we want to restore only one type of DB with a retention
-- policy, check that we don't restore additional instances of that DB
-- that are dependencies of other (non-restored) DBs.
retentionRestoreDepsTest :: Test
retentionRestoreDepsTest = TestCase $
  withFakeCloudDBs $ \evb cfgAPI dbdir backupdir -> do
    let cfg = dbConfig dbdir $ (serverConfig backupdir)
          { config_retention = def
            { databaseRetentionPolicy_default_retention = def
              { retention_retain_at_most = Just 1
              }
            },
            config_restore = def
            { databaseRestorePolicy_enabled = False
            , databaseRestorePolicy_override = Set.fromList ["test2"]
            }
          }
    withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    dbs <- listDBs env
    let repos = map database_repo dbs
    assertEqual "after" [ "0014" ] (map repo_hash repos)

backupRestoreTest :: Test
backupRestoreTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_backup = (config_backup (serverConfig backupdir))
          { databaseBackupPolicy_allowed = HashSet.fromList ["test", "test2"] }
        }
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    atomically $ do
      dbs <- Catalog.getLocalDatabases $ envCatalog env
      let should_wait Thrift.GetDatabaseResult
              {getDatabaseResult_database = Thrift.Database{..}} =
            database_status == DatabaseStatus_Complete
            && isNothing database_location
      when (any should_wait dbs) retry

  backups <- listDirectory backupdir
  let withProps filename = [filename, filename ++ ".props"]
  let expected = concatMap withProps
        [ "test.0001", "test.0003", "test.0004", "test.0005" , "test2.0006"]
  assertEqual "after"
    expected
    (sort backups)
  -- should not backup failed db test.0002

  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_restore = (config_restore (serverConfig backupdir))
          { databaseRestorePolicy_enabled = True }
        , config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 1
            , retention_retain_at_least = Just 2 }
          }
        }

  -- remove all the DBs, so we have to restore them:
  removeDirectoryRecursive dbdir
  createDirectoryIfMissing True (dbdir </> "test")
  createDirectoryIfMissing True (dbdir </> "test2")
  withDatabases evb cfg cfgAPI $ \env -> do
  runDatabaseJanitor env  -- this should kick off the restore

  let waitForRestore = do
        dbs <- listDBs env
        let
          available =
            [ db | db <- dbs
            , database_status db == Thrift.DatabaseStatus_Complete ]
        when (length dbs < 2 || length available < 2) waitForRestore

  r <- timeout (60*1000000) $ waitForRestore
  assertBool "timeout" $ isJust r

  -- we should have restored both 0001 and 0003, even though 0003 is
  -- older than delete_if_older, because retain_at_least = 2
  dbs <- listDBs env
  let
    repos = map database_repo dbs
    db1 = Repo "test" "0001"
    db3 = Repo "test" "0003"


  assertEqual "after"
    [ "0001", "0003", "0004", "0005", "0006"]
    (sort $ map repo_hash repos)

  areClosed <- atomically
    $ liftM2 (&&) (isDatabaseClosed env db1) (isDatabaseClosed env db3)
  assertBool "dbs available but closed after restore" areClosed


openNewestTest :: Test
openNewestTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ serverConfig backupdir

  withDatabases evb cfg cfgAPI $ \env -> do

  let
    newestDb = Repo "test" "0001"
    oldestDb = Repo "test" "0005"

  newestClosed <- atomically $ isDatabaseClosed env newestDb
  oldestClosed <- atomically $ isDatabaseClosed env oldestDb

  assertBool "newest closed before" newestClosed
  assertBool "oldest closed before" oldestClosed

  runDatabaseJanitor env

  newestOpen <- atomically $ not <$> isDatabaseClosed env newestDb
  oldestStillClosed <- atomically $ isDatabaseClosed env oldestDb

  assertBool "newest open after" newestOpen
  assertBool "oldest closed after" oldestStillClosed

  -- delete repo001, repo002 is the newest but is broken, so we
  -- should open repo003.
  deleteDatabase env newestDb
  runDatabaseJanitor env

  let nextBroken = Repo "test" "0002"
  let nextNotBroken = Repo "test" "0003"
  dontOpenBroken <- atomically $ isDatabaseClosed env nextBroken
  openNotBroken <- atomically $ not <$> isDatabaseClosed env nextNotBroken

  assertBool "don't open broken" dontOpenBroken
  assertBool "open not broken" openNotBroken

closeIdleDBsTest :: Test
closeIdleDBsTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ serverConfig backupdir

  withDatabases evb cfg cfgAPI $ \env -> do

  let
    normalDb = Repo "test" "0001"
    blackListedDb = Repo "test" "0003"

  withOpenDatabase env normalDb (\_ -> return ())
  withOpenDatabase env blackListedDb (\_ -> return ())

  normalDbOpen <- atomically $ not <$> isDatabaseClosed env normalDb
  blackListedDbOpen <- atomically $ not <$> isDatabaseClosed env blackListedDb

  assertBool "regular DB open before" normalDbOpen
  assertBool "blacklisted DB open before" blackListedDbOpen

  -- Close all available DBs unless blacklisted
  closeIdleDatabases env (seconds 0) [blackListedDb]

  normalDbClosed <- atomically $ isDatabaseClosed env normalDb
  blackListedDbStillOpen <- atomically
    $ not <$> isDatabaseClosed env blackListedDb

  assertBool "regular DB closed after" normalDbClosed
  assertBool "blacklisted DB open after" blackListedDbStillOpen

shardingTest :: Test
shardingTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  myShards <- newIORef ["0001"] -- initial shard assignment
  let cfg = (dbConfig dbdir (serverConfig backupdir))
        {cfgShardManager = \_ _ k -> k $ SomeShardManager $
          shardByRepoHash (Just <$> readIORef myShards)}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    waitDel env
    dbs <- listHereDBs env
    assertEqual "initial shard assignment"
      ["0001"]
      (sort $ map (repo_hash . database_repo) dbs)

    -- update the shard assignment and verify
    writeIORef myShards ["nil"]
    runDatabaseJanitor env
    waitDel env
    dbs <- listHereDBs env
    assertEqual "shard assignment: removed 0003"
      []
      (sort $ map (repo_hash . database_repo) dbs)

shardingStacksTest :: Test
shardingStacksTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  myShards <- newIORef ["0006"] -- initial shard assignment
  let cfg = (dbConfig dbdir (serverConfig backupdir))
        {cfgShardManager = \_ _ k -> k $ SomeShardManager $
          shardByBaseOfStackRepoHash (Just <$> readIORef myShards)}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    waitDel env
    dbs <- listHereDBs env
    assertEqual "all dbs in the stack belong to the shard"
      ["0003", "0004", "0005", "0006"]
      (sort $ map (repo_hash . database_repo) dbs)

shardingFallbackTest :: Test
shardingFallbackTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = (dbConfig dbdir (serverConfig backupdir))
        {cfgShardManager = \_ _ k ->
          k $ SomeShardManager $ shardByRepoHash (pure Nothing)}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    waitDel env
    dbs <- listHereDBs env
    assertEqual "falls back to no sharding"
      ["0001", "0002", "0003", "0004", "0005", "0006"]
      (sort $ map (repo_hash . database_repo) dbs)

shardingByRepoNameTest :: Test
shardingByRepoNameTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = (dbConfig dbdir (serverConfig backupdir))
        {cfgShardManager = \_ _ k -> k $ SomeShardManager $
          shardByRepo (pure $ Just ["test2"])}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    waitDel env
    dbs <- listHereDBs env
    assertEqual "only test2 dbs belong to the shard"
      ["0003", "0004", "0005", "0006"]
      (sort $ map (repo_hash . database_repo) dbs)


elsewhereTest :: Test
elsewhereTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let myShards = pure $ Just ["0001", "0003"]
  let cfg = (dbConfig dbdir (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 10
            , retention_retain_at_least = Just 10 }
          }
        })
        {cfgShardManager = \_ _ k -> k $ SomeShardManager $ shardByRepoHash myShards}
  withDatabases evb cfg cfgAPI $ \env -> do

    dbs <- listHereDBs env
    assertEqual "before"
      [ "0001", "0002", "0003", "0004", "0005", "0006"]
      (sort (map (repo_hash . database_repo) dbs))

    runDatabaseJanitor env
    waitDel env
    dbs <- listAllDBs env

    assertEqual "after: dbs available with the retention policy"
      [ "0001", "0002", "0003", "0004", "0005", "0006"]
      (sort $ map (repo_hash . database_repo) dbs)

makeFilterDBsWithInvariant :: ([Repo] -> IO [Repo]) -> IO ([Repo] -> IO [Repo])
makeFilterDBsWithInvariant pred = do
  seenAvailabeDBsRef <- newIORef mempty
  return $ \dbs -> do
    available <- pred dbs
    let availableSet = HashSet.fromList available
    beenAvailableBefore <- readIORef seenAvailabeDBsRef
    assertEqual "called cfgFilterAvailableDBs twice on the same DB"
      mempty (HashSet.intersection availableSet beenAvailableBefore)
    writeIORef seenAvailabeDBsRef (availableSet <> beenAvailableBefore)
    return available

elsewhereNotYetAvailableTest :: Test
elsewhereNotYetAvailableTest =
  TestCase $ withFakeCloudDBs $ \evb cfgAPI dbdir backupdir -> do
    let myShards = pure $ Just []
    filterDBs <- makeFilterDBsWithInvariant $
      return . filter ((`elem` ["0008","0009"]) . repo_hash)
    let cfg = (dbConfig dbdir (serverConfig backupdir)
          { config_retention = def
            { databaseRetentionPolicy_default_retention = def
              { retention_delete_if_older =
                  Just $ fromIntegral $ timeSpanInSeconds $ days 10
              , retention_retain_at_least = Just 2
              , retention_retain_at_most = Just 4
              }
            },
            config_restore = def {
              databaseRestorePolicy_enabled = True
            }
          })
          {cfgShardManager = \_ _ k ->
              k $ SomeShardManager $ shardByRepoHash myShards
          ,cfgFilterAvailableDBs = filterDBs
          }
    withDatabases evb cfg cfgAPI $ \env -> do
      runDatabaseJanitor env
      dbs <- listAllDBs env

      assertEqual
        "at least 2 dbs actually available + at most 4 more not yet available"
        ["0008", "0009", "0011", "0012", "0013", "0014", "0015"]
        (sort $ map (repo_hash . database_repo) dbs)

shardUnexpireTest :: Test
shardUnexpireTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  myShards <- newIORef ["nil"] -- initial incomplete shard assignment
  let cfg = (dbConfig dbdir (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            {retention_expire_delay = Just 30}
          }
        })
        {cfgShardManager = \_ _ k -> k $ SomeShardManager $
          shardByRepoHash (Just <$> readIORef myShards)}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    dbs <- listDBs env

    expiring <- atomically $
      mapM (Catalog.readExpiring (envCatalog env) . database_repo) dbs
    assertBool "All expiring" (all isJust expiring)

    -- update the shard assignment and verify
    writeIORef myShards [repo_hash repo0001]
    runDatabaseJanitor env

    expiring <- atomically $ Catalog.readExpiring (envCatalog env) repo0001
    assertBool "0001 not expiring now" (isNothing expiring)

expireTest :: Test
expireTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  let emptyShardAssignment = shardByRepoHash $ pure $ Just ["nil"]
  let cfg = (dbConfig dbdir (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            {retention_expire_delay = Just 1}
          }
        , config_janitor_period = Nothing
        })
        {cfgShardManager = \_ _ k -> k $ SomeShardManager emptyShardAssignment}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    dbs <- listDBs env

    expiring <- atomically $
      mapM (Catalog.readExpiring (envCatalog env) . database_repo) dbs
    assertBool "All expiring" (all isJust expiring)

    -- run the Janitor before the expire delay to exercise all code paths
    runDatabaseJanitor env
    sleep 1
    -- run the Janitor twice after the expire delay to exercise all code paths
    runDatabaseJanitor env
    runDatabaseJanitor env
    -- check that the DBs were really deleted
    res <- timeout 10000000 -- 10s
                   (waitDel env)
    waitingDeletion <- readTVarIO (envDeleting env)
    assertBool ("timeout: " <> show (HashMap.keys waitingDeletion)) (isJust res)
    assertEqual "envDeleting" [] (HashMap.keys waitingDeletion)
    localDBs <- listHereDBs env
    assertEqual "All deleted" [] (map database_repo localDBs)

ageCountersTestEx
  :: HasCallStack
  => SomeShardManager
  -> (Env -> [BS.ByteString] -> IO ())
  -> IO ()
ageCountersTestEx shardManager k =
  withTest setupBasicDBs setupBasicCloudDBs $ \evb cfgAPI dbdir backupdir -> do
    let cfg = (dbConfig dbdir (serverConfig backupdir)
          { config_restore = def {
              databaseRestorePolicy_enabled = True
            }, config_retention = def
          }) {cfgShardManager = \_ _ k -> k shardManager}
    withDatabases evb cfg cfgAPI $ \env -> do
      runDatabaseJanitor env
      waitDel env
      sideEffects <- runDatabaseJanitorPureish env
      let countersToPublish =
            [ c
            | PublishCounter c _ <- sideEffects
            , ".age" `BS.isSuffixOf` c || ".span" `BS.isSuffixOf` c
            ]
      k env $ sort countersToPublish

ageCountersCompleteTest :: Test
ageCountersCompleteTest = TestCase $ ageCountersTestEx
  (SomeShardManager $ shardByRepoHash (pure $ Just ["0001"]))
  $ \_ -> assertEqual
    "Should publish age counters for all newest DBs restored locally"
    ["glean.db.test.age", "glean.db.test.span"]

ageCountersOnlyNewestTest :: Test
ageCountersOnlyNewestTest = TestCase $ ageCountersTestEx
  (SomeShardManager $ shardByRepoHash (pure $ Just []))
  $ \_ -> assertEqual
      "Should not publish age for newest DBs not restored locally"
      []

ageCountersClearTest:: Test
ageCountersClearTest = TestCase $ do
  shardAssignmentRef <- newIORef ["0001"]
  ageCountersTestEx
    (SomeShardManager $ shardByRepoHash (Just <$> readIORef shardAssignmentRef))
    $ \env _ -> do
    writeIORef shardAssignmentRef []
    -- Run the Janitor to pick up the new assignment and delete unassigned DBs
    runDatabaseJanitor env
    waitDel env
    -- Run the Janitor again after DBs are deleted to clear the counters
    runDatabaseJanitor env
    counters <- getCounters
    assertBool "glean.db.test.age cleared"
      $ not (HashMap.member "glean.db.test.age" counters)

stuckTest :: Test
stuckTest = TestCase $ withFakeCloudDBs $ \evb cfgAPI dbdir backupdir -> do
  let cfg = dbConfig dbdir $ (serverConfig backupdir)
        { config_janitor_period = Just 0
        , config_restore = def {
              databaseRestorePolicy_enabled = True
            }
        , config_retention = def {
            databaseRetentionPolicy_default_retention = def {
            retention_retain_at_least = Just 2
          }
         }
        }
      cfg' = cfg {
         cfgFilterAvailableDBs  = \_ -> do
           uninterruptibleMask_ $ threadDelay maxBound
           return []
      }
  withDatabases evb cfg' cfgAPI $ \env -> do
    (_, janitorResult) <- atomically $ do
      res <- readTVar $ envDatabaseJanitor env
      maybe retry return res
    assertBool (show janitorResult) $ case janitorResult of
      JanitorStuck -> True
      _ -> False

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deleteOldDBs" deleteOldDBsTest
  , TestLabel "deleteIncompleteDBs" deleteIncompleteDBsTest
  , TestLabel "retainAtMost" retainAtMostTest
  , TestLabel "retainAtLeast" retainAtLeastTest
  , TestLabel "backupRestore" backupRestoreTest
  , TestLabel "openNewestDB" openNewestTest
  , TestLabel "closeIdleDBs" closeIdleDBsTest
  , TestLabel "expireDelay" expireTest
  , TestLabel "sharding" shardingTest
  , TestLabel "shardingStacks" shardingStacksTest
  , TestLabel "shardingFallback" shardingFallbackTest
  , TestLabel "shardingByRepoName" shardingByRepoNameTest
  , TestLabel "shardingExpiring" shardUnexpireTest
  , TestLabel "availableElsewhere" elsewhereTest
  , TestLabel "notAvailableElsewhere" elsewhereNotYetAvailableTest
  , TestLabel "ageCountersForAllNewestDBs" ageCountersCompleteTest
  , TestLabel "ageCountersForOnlyNewestDBs" ageCountersOnlyNewestTest
  , TestLabel "ageCountersClear" ageCountersClearTest
  , TestLabel "stuck" stuckTest
  , TestLabel "requiredPropsTest" requiredPropsTest
  , TestLabel "retentionRestoreDepsTest" retentionRestoreDepsTest
  , TestLabel "multiRetentionTest" multiRetentionTest
  ]

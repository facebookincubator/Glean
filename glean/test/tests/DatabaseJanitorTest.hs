{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module DatabaseJanitorTest (main) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Timeout
import Test.HUnit

import TestRunner
import Util.EventBase
import Util.TimeSec

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Data
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
import Glean.Impl.ConfigProvider
import Glean.Init
import Glean.RTS.Types (lowestFid)
import Glean.ServerConfig.Types as ServerTypes
import Glean.Internal.Types
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.ShardManager
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.Time (seconds)


withTest
  :: (FilePath -> IO ())
  -> (EventBaseDataplane -> ConfigAPI -> FilePath -> FilePath -> IO ())
  -> IO ()
withTest setup action =
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \cfgAPI ->
  withSystemTempDirectory "glean-dbtest" $ \dbdir -> do
  withSystemTempDirectory "glean-dbtest-backup" $ \backupdir -> do
    setup dbdir
    action evb cfgAPI dbdir backupdir

complete, broken :: UTCTime -> Completeness
complete = Complete . DatabaseComplete . utcTimeToPosixEpochTime
broken _ = Broken (DatabaseBroken "index" "TESTING")

setupBasicDBs :: FilePath -> IO ()
setupBasicDBs dbdir = do
  now <- getCurrentTime
  let age t = addUTCTime (negate (fromIntegral (timeSpanInSeconds t))) now
  schema <- parseSchemaDir schemaSourceDir
  schema <- newDbSchema schema readWriteContent
  -- populate a dir with various DBs
  makeFakeDB schema dbdir (Repo "test" "0001") (age (days 0)) complete Nothing
  makeFakeDB schema dbdir (Repo "test" "0002") (age (days 2)) broken Nothing
  makeFakeDB schema dbdir (Repo "test" "0003") (age (days 3)) complete $
    Just (Repo "test" "0004")
  makeFakeDB schema dbdir (Repo "test" "0004") (age (days 4)) complete $
    Just (Repo "test" "0005")
  makeFakeDB schema dbdir (Repo "test" "0005") (age (days 5)) complete $
    Just (Repo "test2" "0006")
  makeFakeDB schema dbdir (Repo "test2" "0006") (age (days 6)) complete Nothing

withFakeDBs
  :: (EventBaseDataplane -> ConfigAPI -> FilePath -> FilePath -> IO ())
  -> IO ()
withFakeDBs action = withTest setupBasicDBs action

makeFakeDB
  :: DbSchema
  -> FilePath
  -> Repo
  -> UTCTime
  -> (UTCTime -> Completeness)
  -> Maybe Repo
  -> IO ()
makeFakeDB schema root repo dbtime completeness stacked = do
  let
    meta = Meta
      { metaVersion = Storage.currentVersion
      , metaCreated = utcTimeToPosixEpochTime dbtime
      , metaRepoHashTime = Nothing
      , metaCompleteness = completeness dbtime
      , metaBackup = Nothing
      , metaProperties = HashMap.empty
      , metaDependencies = Thrift.Dependencies_stacked <$> stacked
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
      (Storage.Create lowestFid Nothing)
      Storage.currentVersion)
    Storage.close
    (\hdl -> storeSchema hdl $ toSchemaInfo schema)
  LB.writeFile (repoPath </> "meta") (encode meta)


dbConfig :: FilePath -> ServerTypes.Config -> Glean.Database.Config.Config
dbConfig dbdir serverConfig = def
  { cfgRoot = Just dbdir
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

-- | A shard manager that uses repo hashes as shards,
-- and a dynamic shard assignment
shardByRepo :: IORef [Text.Text] -> ShardManager Text.Text
shardByRepo refShardAssignment =
  ShardManager (readIORef refShardAssignment) (\repo _ -> repo_hash repo)

shardingTest :: Test
shardingTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  myShards <- newIORef ["0001", "0003"] -- initial shard assignment
  let cfg = (dbConfig dbdir (serverConfig backupdir))
        { cfgShardManager = SomeShardManager $ shardByRepo myShards}
  withDatabases evb cfg cfgAPI $ \env -> do
    runDatabaseJanitor env
    waitDel env
    dbs <- listDBs env
    assertEqual "initial shard assignment"
      ["0001", "0003"]
      (sort $ map (repo_hash . database_repo) dbs)

    -- update the shard assignment and verify
    writeIORef myShards ["0001"]
    runDatabaseJanitor env
    waitDel env
    dbs <- listDBs env
    assertEqual "shard assignment: removed 0003"
      ["0001"]
      (sort $ map (repo_hash . database_repo) dbs)

elsewhereTest :: Test
elsewhereTest = TestCase $ withFakeDBs $ \evb cfgAPI dbdir backupdir -> do
  myShards <- newIORef ["0001", "0003"] -- initial shard assignment
  let cfg = (dbConfig dbdir (serverConfig backupdir)
        { config_retention = def
          { databaseRetentionPolicy_default_retention = def
            { retention_delete_if_older =
                Just $ fromIntegral $ timeSpanInSeconds $ days 10
            , retention_retain_at_least = Just 10 }
          }
        })
        { cfgShardManager = SomeShardManager $ shardByRepo myShards}
  withDatabases evb cfg cfgAPI $ \env -> do

    dbs <- listDBs env
    assertEqual "before"
      [ "0001", "0002", "0003", "0004", "0005", "0006"]
      (sort (map (repo_hash . database_repo) dbs))

    runDatabaseJanitor env
    waitDel env
    dbs <- listAllDBs env

    assertEqual "after: dbs available with the retention policy"
      [ "0001", "0002", "0003", "0004", "0005", "0006"]
      (sort $ map (repo_hash . database_repo) dbs)

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deleteOldDBs" deleteOldDBsTest
  , TestLabel "deleteIncompleteDBs" deleteIncompleteDBsTest
  , TestLabel "retainAtMost" retainAtMostTest
  , TestLabel "retainAtLeast" retainAtLeastTest
  , TestLabel "backupRestore" backupRestoreTest
  , TestLabel "openNewestDB" openNewestTest
  , TestLabel "closeIdleDBs" closeIdleDBsTest
  , TestLabel "sharding" shardingTest
  , TestLabel "availableElsewhere" elsewhereTest
  ]

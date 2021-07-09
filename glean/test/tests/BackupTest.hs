-- Copyright (c) Facebook, Inc. and its affiliates.

module BackupTest (main) where

import Control.Concurrent.STM
import Control.Monad
import Data.Default
import qualified Data.HashSet as HashSet
import Data.List
import qualified Data.Text as Text
import Data.Time.Clock
import System.IO.Temp
import Test.HUnit

import TestRunner
import Util.EventBase

import Glean.Backend
import Glean.Database.Backup (Event(..))
import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Mock as Backup.Mock
import Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Janitor
import Glean.Database.Meta
import Glean.Database.Types
import Glean.Impl.ConfigProvider
import Glean.Init
import Glean.ServerConfig.Types as ServerTypes
import Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.Trace

withTest :: (EventBaseDataplane -> ConfigAPI -> FilePath -> IO ()) -> IO ()
withTest action =
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \cfgAPI ->
  withSystemTempDirectory "glean-dbtest-backup" $ \backupdir ->
    action evb cfgAPI backupdir

data TestEnv = forall site. Backup.Site site => TestEnv
  { testEnv :: Env
  , testBackup :: site
  , testUpdConfig :: (ServerTypes.Config -> ServerTypes.Config) -> IO ()
  , testEvents :: TQueue Event
  }

withTestEnv
  :: [(Repo, Bool, Int)]
  -> (ServerTypes.Config -> ServerTypes.Config)
  -> (TestEnv -> IO ())
  -> EventBaseDataplane
  -> ConfigAPI
  -> FilePath
  -> IO ()
withTestEnv dbs init_server_cfg action evb cfgAPI backupdir =
  withSystemTempDirectory "glean-dbtest" $ \dbdir -> do
  (server_cfg, update_server_cfg) <- ThriftSource.mutable $ init_server_cfg def
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
  (l, events) <- recorder
  let config = def
        { cfgRoot = dbdir
        , cfgSchemaSource = schemaSourceFiles
        , cfgRecipeConfig = def
        , cfgServerConfig = server_cfg
        , cfgReadOnly = False
        , cfgMockWrites = False
        , cfgListener = l
        }

  withDatabases evb config cfgAPI $ \env -> do
    now <- getCurrentTime
    mapM_ (makeDB env now) dbs
    action TestEnv
      { testEnv = env
      , testBackup = Backup.Mock.mockSite backupdir
      , testUpdConfig = update_server_cfg
      , testEvents = events
      }

expectBackups :: [Repo] -> Expect Event
expectBackups repos = parallel
  [ opt (want Waiting) <> wants [ BackupStarted repo, BackupFinished repo ]
  | repo <- repos ]

expectRestore :: [Repo] -> Expect Event
expectRestore repos = parallel
  [ opt (want Waiting) <> wants [ RestoreStarted repo, RestoreFinished repo ]
  | repo <- repos ]

expectFinalize :: [Repo] -> Expect Event
expectFinalize repos = parallel
  [ opt (want Waiting) <> wants [ FinalizeStarted repo, FinalizeFinished repo ]
  | repo <- repos ]

makeDB :: Env -> UTCTime -> (Repo, Bool, Int) -> IO ()
makeDB env now (repo, good, age) = do
  KickOffResponse False <- kickOffDatabase env def
    { kickOff_repo = repo
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
    }
  let created t = addUTCTime (negate (fromIntegral t)) now
  void $ atomically $ Catalog.modifyMeta (envCatalog env) repo $ \meta ->
    return meta { metaCreated = utcTimeToPosixEpochTime (created age) }
  workFinished env WorkFinished
    { workFinished_work = def
        { work_repo = repo
        , work_handle = ""
        }
    , workFinished_outcome = if good
        then Outcome_success Success
        else Outcome_failure $ Failure "failed"
    }

basicBackupTest :: Test
basicBackupTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | (repo, True, _) <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["test"] } }
    expect testEvents $ mconcat
      [ expectBackups [repo | (repo, True, _) <- repos]
      , want Waiting ]

    backups <- Backup.enumerate testBackup
    assertEqual "repos"
      (sort [repo | (repo, True, _) <- repos])
      (sort [repo | (repo, _) <- backups])
  where
    repos :: [(Repo, Bool, Int)]
    repos =
      [ (Repo "test" "1", True, 0)
      , (Repo "test" "2", False, 0)
      , (Repo "test" "3", True, 0) ]

allowedTest :: Test
allowedTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | (repo, True, _) <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["bar"] }}
    expect testEvents $ mconcat
      [ expectBackups [repo | (repo, True, _) <- repos, repo_name repo == "bar"]
      , want Waiting ]
  where
    repos :: [(Repo, Bool, Int)]
    repos =
      [ (Repo "foo" "1", True, 0)
      , (Repo "bar" "2", True, 0)
      , (Repo "foo" "3", True, 0)
      , (Repo "baz" "4", True, 0)
      , (Repo "bar" "5", True, 0) ]

restoreOrderTest :: Test
restoreOrderTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | (repo, True, _) <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed =
              HashSet.fromList ["bar","foo","baz"] }}
    expect testEvents $ mconcat
      [ expectBackups [repo | (repo, True, _) <- repos ]
      , want Waiting ]

    -- delete all the repos and run the janitor to start restoring
    forM_ repos $ \(repo, _, _) -> deleteDatabase testEnv repo
    testUpdConfig $ \scfg -> scfg
      { config_restore = def { databaseRestorePolicy_enabled = True } }
    runDatabaseJanitor testEnv

    -- restoring picks the newest DB of each repo to restore, and then
    -- picks the first of those ordered by staleness, and then most
    -- recent.

    -- local: []
    -- newest restorable for each repo:
    --    Repo "foo" "3", True, 1
    --    Repo "bar" "5", True, 0
    -- all are equally stale, so we pick the newest, bar/5
    expect testEvents $ expectRestore [ Repo "bar" "5" ]

    -- local: [bar/5]
    -- newest restorable for each repo:
    --    Repo "foo" "3", True, 1
    --    Repo "baz" "4", True, 3
    --    Repo "bar" "2", True, 1
    -- foo, baz are stale, foo/3 is the newest
    expect testEvents $ expectRestore [ Repo "foo" "3" ]

    -- local: [bar/5, foo/3]
    -- newest restorable for each repo:
    --    Repo "foo" "1", True, 2
    --    Repo "baz" "4", True, 3
    --    Repo "bar" "2", True, 1
    -- baz is stale
    expect testEvents $ expectRestore [ Repo "baz" "4" ]

    -- local: [bar/5, foo/3, baz/4]
    -- newest restorable for each repo:
    --    Repo "foo" "1", True, 2
    --    Repo "bar" "2", True, 1
    -- none stale, bar/2 is the newest
    expect testEvents $ expectRestore [ Repo "bar" "2" ]
  where
    repos :: [(Repo, Bool, Int)]
    repos =
      [ (Repo "foo" "1", True, 2)
      , (Repo "bar" "2", True, 1)
      , (Repo "foo" "3", True, 1)
      , (Repo "baz" "4", True, 3)
      , (Repo "bar" "5", True, 0) ]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "basicBackup" basicBackupTest
  , TestLabel "allowedTest" allowedTest
  , TestLabel "restoreOrderTest" restoreOrderTest
  ]

module BackupTest (main) where

import Control.Concurrent.STM
import Data.Default
import qualified Data.HashSet as HashSet
import Data.List
import qualified Data.Text as Text
import System.IO.Temp
import Test.HUnit

import TestRunner
import Util.EventBase

import Glean.Backend
import Glean.Database.Backup (Event(..))
import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Mock as Backup.Mock
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Repo
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
  :: [(Repo, Bool)]
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
    mapM_ (uncurry $ makeDB env) dbs
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

expectFinalize :: [Repo] -> Expect Event
expectFinalize repos = parallel
  [ opt (want Waiting) <> wants [ FinalizeStarted repo, FinalizeFinished repo ]
  | repo <- repos ]

makeDB :: Env -> Repo -> Bool -> IO ()
makeDB env repo good = do
  KickOffResponse False <- kickOffDatabase env def
    { kickOff_repo = repo
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
    }
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
      [ expectFinalize [repo | (repo, True) <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["test"] } }
    expect testEvents $ mconcat
      [ expectBackups [repo | (repo, True) <- repos]
      , want Waiting ]

    backups <- Backup.enumerate testBackup
    assertEqual "repos"
      (sort [repo | (repo, True) <- repos])
      (sort [repo | (repo,_) <- backups])
  where
    repos =
      [ (Repo "test" "1", True)
      , (Repo "test" "2", False)
      , (Repo "test" "3", True) ]

allowedTest :: Test
allowedTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | (repo, True) <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["bar"] }}
    expect testEvents $ mconcat
      [ expectBackups [repo | (repo, True) <- repos, repo_name repo == "bar"]
      , want Waiting ]
  where
    repos =
      [ (Repo "foo" "1", True)
      , (Repo "bar" "2", True)
      , (Repo "foo" "3", True)
      , (Repo "baz" "4", True)
      , (Repo "bar" "5", True) ]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "basicBackup" basicBackupTest
  , TestLabel "allowedTest" allowedTest
  ]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module BackupTest (main) where

import Control.Concurrent.STM
import Control.Monad
import Data.Default
import qualified Data.HashSet as HashSet
import Data.List
import Data.Text (Text)
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
import Glean.Database.Work (finalizeWait)
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

data TestDbSpec = TestDbSpec
  { _testDbRepo :: Repo
  , _testDbGood :: Bool
  , _testDbAge :: Int
  , _testDbDeps :: Maybe Dependencies
  }

goodDb :: Text -> Text -> TestDbSpec
goodDb name hash = TestDbSpec (Repo name hash) True 0 Nothing

badDb :: Text -> Text -> TestDbSpec
badDb name hash = TestDbSpec (Repo name hash) False 0 Nothing

goodDbAge :: Text -> Text -> Int -> TestDbSpec
goodDbAge name hash age = TestDbSpec (Repo name hash) True age Nothing

goodDbAgeDeps :: Text -> Text -> Int -> Dependencies -> TestDbSpec
goodDbAgeDeps name hash age deps =
  TestDbSpec (Repo name hash) True age (Just deps)

withTestEnv
  :: [TestDbSpec]
  -> (ServerTypes.Config -> ServerTypes.Config)
  -> (TestEnv -> IO ())
  -> EventBaseDataplane
  -> ConfigAPI
  -> FilePath
  -> IO ()
withTestEnv dbs init_server_cfg action evb cfgAPI backupdir = do
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
        { cfgRoot = Nothing
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

makeDB :: Env -> UTCTime -> TestDbSpec -> IO ()
makeDB env now (TestDbSpec repo good age deps) = do
  KickOffResponse False <- kickOffDatabase env def
    { kickOff_repo = repo
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
    , kickOff_dependencies = deps
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
  when good $ finalizeWait env repo

basicBackupTest :: Test
basicBackupTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | TestDbSpec repo True _ _ <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["test"] } }
    expect testEvents $ mconcat
      [ expectBackups [repo | TestDbSpec repo True _ _ <- repos]
      , want Waiting ]

    backups <- Backup.enumerate testBackup
    assertEqual "repos"
      (sort [repo | TestDbSpec repo True _ _ <- repos])
      (sort [repo | (repo, _) <- backups])
  where
    repos :: [TestDbSpec]
    repos =
      [ goodDb "test" "1"
      , badDb "test" "2"
      , goodDb "test" "3" ]

allowedTest :: Test
allowedTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | TestDbSpec repo True _ _ <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed = HashSet.fromList ["bar"] }}
    expect testEvents $ mconcat
      [ expectBackups [repo | TestDbSpec repo True _ _ <- repos,
          repo_name repo == "bar"]
      , want Waiting ]
  where
    repos :: [TestDbSpec]
    repos =
      [ goodDb "foo" "1"
      , goodDb "bar" "2"
      , goodDb "foo" "3"
      , goodDb "baz" "4"
      , goodDb "bar" "5" ]

restoreOrderTest :: Test
restoreOrderTest = TestCase $ withTest $ withTestEnv
  repos
  id
  $ \TestEnv{..} -> do
    expect testEvents $ mconcat
      [ expectFinalize [repo | TestDbSpec repo True _ _ <- repos] ]
    testUpdConfig $ \scfg -> scfg
      { config_backup = (config_backup scfg)
          { databaseBackupPolicy_allowed =
              HashSet.fromList ["bar","foo","baz"] }}
    expect testEvents $ mconcat
      [ expectBackups [repo | TestDbSpec repo True _ _ <- repos ]
      , want Waiting ]

    -- delete all the repos and run the janitor to start restoring
    forM_ repos $ \(TestDbSpec repo _ _ _) -> deleteDatabase testEnv repo
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
    -- none stale, bar/2 is the newest, but it depends on foo/1, so
    -- we should restore that first.
    expect testEvents $ expectRestore [ Repo "foo" "1" ]
    expect testEvents $ expectRestore [ Repo "bar" "2" ]
  where
    repos :: [TestDbSpec]
    repos =
      [ goodDbAge "foo" "1" 2
      , goodDbAge "foo" "3" 1
      , goodDbAgeDeps "bar" "2" 1 (Dependencies_stacked (Repo "foo" "1"))
      , goodDbAge "bar" "5" 0
      , goodDbAge "baz" "4" 3
      ]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "basicBackup" basicBackupTest
  , TestLabel "allowedTest" allowedTest
  , TestLabel "restoreOrderTest" restoreOrderTest
  ]

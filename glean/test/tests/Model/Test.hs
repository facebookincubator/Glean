{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{- This module implements state machine / lockstep testing
   for the Glean server, in particular the Janitor.

  - https://well-typed.com/blog/2022/09/lockstep-with-quickcheck-dynamic/
  - https://well-typed.com/blog/2019/01/qsm-in-depth/

   The basic idea is to define a pure 'Model' of the janitor that can be
   updated with a 'Command' representing an external event. Examples:
    - A new DB is produced
    - A DB download completes
    - The sharding assignment changes
    - Time elapses

  The test generates a random `[Command]` and applies them in sequence to both
  the `Model` and the Glean server, comparing the observable state in every step

  The observable state is currently limited to:
    - the Catalog
    - the restore queue
    - (to come next) the active DBs set
    - (to come later) the advertised shards
    - (to come later) the ShardManager DB counters

  Observing this state in the Glean server can be tricky because some side
  effects happen asynchronously, e.g. deletes and restores, and we need to
  synchronise before observing the state. This requires us to mock certain
  parts of the server 'Env':
    - envGetCurrentTime: in order to control time elapsed
    - envShardManager: in order to control the shard assignment
    - envStorage (just restore): in order to control async DB restores
 -}
module Model.Test (main) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (
  atomically,
  modifyTVar,
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
  writeTVar,
 )
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (guard, unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (def)
import Data.Either.Combinators (whenLeft)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import qualified Data.Text.Lazy.IO as Text
import GHC.Stack (HasCallStack)
import Glean.Database.Backup.Backend (Backend (fromPath))
import qualified Glean.Database.Backup.Backend as Site
import Glean.Database.Backup.Mock (mockSite)
import qualified Glean.Database.Backup.Mock as Backup.Mock
import Glean.Database.Catalog (entriesEphemeral, entriesRestoring, getEntries)
import Glean.Database.Config (
  Config (
    cfgDataStore,
    cfgSchemaSource,
    cfgServerConfig
  ),
  parseSchemaDir,
  schemaSourceDir,
 )
import Glean.Database.Data (storeSchema)
import Glean.Database.Env (withDatabases)
import Glean.Database.Janitor (
  runDatabaseJanitor,
 )
import Glean.Database.List (listDatabases, listRestorable)
import Glean.Database.Meta (posixEpochTimeToUTCTime)
import Glean.Database.Restore (forRestoreSitesM)
import Glean.Database.Schema (
  newDbSchema,
  readWriteContent,
  toStoredSchema,
 )
import Glean.Database.Schema.Types
import Glean.Database.Storage.Memory (Database (..), Memory (Memory))
import Glean.Database.Types (Env (..))
import Glean.Init (withUnitTest)
import Glean.RTS (lowestFid)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.ServerConfig.Types (
  Config (
    config_backup,
    config_janitor_period,
    config_restore,
    config_retention
  ),
  DatabaseBackupPolicy (
    databaseBackupPolicy_location
  ),
  DatabaseRestorePolicy (databaseRestorePolicy_enabled),
  DatabaseRetentionPolicy (databaseRetentionPolicy_default_retention),
  Retention (
    retention_delete_if_older,
    retention_expire_delay,
    retention_retain_at_least
  ),
 )
import qualified Glean.ServerConfig.Types as ServerTypes
import Glean.Test.Mock (Mock, augment, call, implement, reimplement)
import Glean.Types (
  Database (Database),
  DatabaseStatus (DatabaseStatus_Restoring),
  ListDatabasesResult (listDatabasesResult_databases),
  PosixEpochTime,
  Repo (Repo),
  database_repo,
  database_status,
 )
import Glean.Util.ConfigProvider (
  ConfigProvider (defaultConfigOptions, withConfigProvider),
  NullConfigProvider,
 )
import qualified Glean.Util.Observed as Observed
import Glean.Util.ShardManager (
  SomeShardManager (SomeShardManager),
  shardByRepoHash,
 )
import Glean.Util.Some (Some (Some))
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Util.IO (withTempFileContents)
import Model.Command (
  Command (..),
  ShardingAssignmentChange (ShardAdded, ShardRemoved),
  commandType,
  defMeta,
  fixCreationTimes,
  good,
  insertTimeLapses,
 )
import Model.Mock
import Model.Model (Model, addTime, initialModel, numberOfShards, zeroTime)
import Model.System (SystemState, modelState, readSystemState)
import Model.Update (stepModel)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (
  BufferMode (LineBuffering),
  hPutStr,
  hPutStrLn,
  hSetBuffering,
  stderr,
 )
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Time.Extra (Seconds, timeout)
import Test.HUnit (
  Test (TestCase, TestLabel, TestList),
  assertFailure,
  (@?=),
 )
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  forAllShrink,
  ioProperty,
  suchThat,
  tabulate,
  whenFail,
 )
import TestRunner (testRunner)
import Text.Pretty.Simple (pShow)
import Util.EventBase (withEventBaseDataplane)
import Util.Log (logError, logInfo)
import Util.Testing (assertProperty)

main :: IO ()
main = do
  withUnitTest $
    testRunner $
      TestList
        [ modelTest "modelTest"
        , TestLabel "Test Assumptions" $
            TestList
              [ TestLabel "fetch backups" $ TestCase fetchBackupsTest
              , TestLabel "Mock site" $ TestCase mockSiteTest
              , TestLabel "Backup restore" $ TestCase backupRestoreTest
              , TestLabel "Open DB" $ TestCase openTest
              ]
        ]

--------------------------------------------------------------------------------

dbConfig ::
  IO (FilePath -> (Glean.Database.Config.Config, MockDataStore Memory))
dbConfig = do
  dataStore <- memoryDataStoreForSchema
  schemaSources <- parseSchemaDir schemaSourceDir
  return $ \backupDir ->
    ( def
        { cfgDataStore = mockedDataStore dataStore
        , cfgSchemaSource = ThriftSource.value schemaSources
        , cfgServerConfig = ThriftSource.value (serverConfig backupDir)
        }
    , dataStore
    )

serverConfig :: String -> ServerTypes.Config
serverConfig backupdir =
  def
    { config_backup =
        def {databaseBackupPolicy_location = "mock:" <> pack backupdir}
    , config_janitor_period = Nothing -- no auto janitor, we'll run it manually
    , config_restore = def {databaseRestorePolicy_enabled = True}
    , config_retention =
        def
          { databaseRetentionPolicy_default_retention =
              def
                { retention_delete_if_older = Just $ fromIntegral numberOfShards
                , retention_retain_at_least = Just 1
                , retention_expire_delay = Just 0
                }
          }
    }

--------------------------------------------------------------------------------

newtype TEnv = TEnv
  { tEnv :: MockedEnv
  }

withTEnv ::
  FilePath ->
  (Glean.Database.Config.Config, MockDataStore Memory) ->
  (TEnv -> IO b) ->
  IO b
withTEnv backupDir (dbConfig, mDataStore) f = withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \(cfgAPI :: NullConfigProvider) ->
    withDatabases evb dbConfig cfgAPI $ \env -> do
      tEnv <- mockEnv backupDir env mDataStore
      f TEnv {..}

--------------------------------------------------------------------------------

data MockedEnv = MockedEnv
  { mockedEnv :: Env
  , mockedTime :: Mock (IO PosixEpochTime)
  , mockedShardAssignment :: IORef (HashSet Text)
  , mockedCompleteRestore :: IO ()
  , backupDir :: FilePath
  }

mockEnv :: FilePath -> Env -> MockDataStore Memory -> IO MockedEnv
mockEnv backupDir Env {..} MockDataStore {..} = do
  schemaSource <- Observed.get envSchemaSource
  schema <- newDbSchema (Just envDbSchemaCache) schemaSource
    LatestSchemaAll readWriteContent
  let storedSchema = toStoredSchema schema
  mockedTime <- implement "getCurrentTime" (pure zeroTime)
  mockedShardAssignment <- newIORef mempty
  restoreSemaphore <- newEmptyMVar
  reimplement
    (mockRestore mockDataStoreStorage)
    ( \dbRepo _ _ -> do
        takeMVar restoreSemaphore
        dbFacts <- FactSet.new lowestFid
        dbData <- newTVarIO mempty
        let db = Glean.Database.Storage.Memory.Database {..}
        case mockedStorage mockDataStoreStorage of
          Memory v -> do
            storeSchema db storedSchema
            atomically $ modifyTVar v $ HM.insert dbRepo db
    )

  let envShardManager =
        -- the choice of shard manager needs to match the model sharding logic
        shardByRepoHash
          (Just . Set.toList <$> readIORef mockedShardAssignment)
  let mockedEnv =
        Env
          { envGetCurrentTime = posixEpochTimeToUTCTime <$> call mockedTime
          , envShardManager = SomeShardManager envShardManager
          , ..
          }
      mockedCompleteRestore = do
        logInfo "restore: go ahead"
        putMVar restoreSemaphore ()
  return MockedEnv {..}

--------------------------------------------------------------------------------

modelTest :: String -> Test
modelTest name = TestLabel name $
  TestCase $ do
    dbConf <- dbConfig
    outputRef <- newIORef mempty
    assertProperty "model" $
      -- generate many random sequences of valid commands
      forAllShrink
        (arbitrary `suchThat` good)
        (filter good . shrink)
        $ \rawCmds ->
          whenFail (hPutStr stderr . ("\n" <>) =<< readIORef outputRef) $
            tabulate "Commands" (map commandType rawCmds) $
              tabulate "Sequence length" [show (length rawCmds)] $
                -- for each sequence
                ioProperty $ do
                  -- manipulate the raw command sequence to make it realistic
                  let realCommands =
                        fixCreationTimes zeroTime $
                          insertTimeLapses rawCmds
                  (output, res) <-
                    hCapture [stderr] $ do
                      try @SomeException $
                        runModelTest dbConf realCommands
                  whenLeft res $ \_ -> writeIORef outputRef output
                  either throwIO pure res

runModelTest ::
  (FilePath -> (Glean.Database.Config.Config, MockDataStore Memory)) ->
  [Command] ->
  IO ()
runModelTest dbConf commands =
  withSystemTempDirectory "glean-dbtest-backup" $ \backupDir ->
    -- start a server
    withTEnv backupDir (dbConf backupDir) $ \TEnv {..} -> do
      -- a helper to traverse the commands list applying each one
      -- to both the server and a pure model,
      -- checking that the intermediate states match in every step
      let loop :: Wait -> Model -> [Command] -> IO ()
          loop _ _ [] = return ()
          loop prevSideEffects model (command : cc) = do
            -- apply the command to both the model and the system
            hPutStrLn stderr $ "Applying " <> show command
            let model' = stepModel model command
            sideEffects <- liftIO $ mutateSystem tEnv command
            -- run the system
            runDatabaseJanitor (mockedEnv tEnv)
            -- wait for side effects to settle down
            moreSideEffects <-
              liftIO $ wait $ sideEffects <> prevSideEffects
            -- assert that the system and model change in lockstep
            let expected = modelState model'
            obtained <- readSystemState (mockedEnv tEnv)
            diffSystemState command expected obtained
            -- continue with the updated model
            loop moreSideEffects model' cc

      hSetBuffering stderr LineBuffering
      loop noWait (initialModel backupDir) commands

--------------------------------------------------------------------------------

diffSystemState :: Command -> SystemState -> SystemState -> IO ()
diffSystemState cmd expect obtain = withSystemTempDirectory "diff" $ \dir -> do
  let expectPath = dir </> "expect"
      obtainPath = dir </> "obtain"
  Text.writeFile expectPath (pShow expect)
  Text.writeFile obtainPath (pShow obtain)
  let cp = proc "diff" ["-U", "1000", expectPath, obtainPath]
  (ec, out, _err) <- readCreateProcessWithExitCode cp ""
  unless (ExitSuccess == ec) $ do
    errorWithoutStackTrace $
      "The observed state of the system and the model don't match after "
        <> show cmd
        <> ":\n"
        <> out

--------------------------------------------------------------------------------

-- | A sequence of waits
newtype Wait = Wait [IO ()]

-- | Wait for the next side effect and return the future side effect waits
wait :: Wait -> IO Wait
wait (Wait (x : xx)) = do
  x
  return (Wait xx)
wait it@(Wait []) = return it

waitAll :: Wait -> IO ()
waitAll (Wait xx) = sequence_ xx

instance Semigroup Wait where
  Wait a <> Wait b = Wait $ loop a b
    where
      loop [] bb = bb
      loop aa [] = aa
      loop (a : aa) (b : bb) = a <> b : loop aa bb

waitOne :: IO () -> Wait
waitOne act = Wait [act]

noWait :: Wait
noWait = Wait []

--------------------------------------------------------------------------------

{- | Side effect the system to simulate or trigger the effect of a command.
   Returns a sequence of waits, one for each subsequent run of the janitor
-}
mutateSystem :: HasCallStack => MockedEnv -> Command -> IO Wait
mutateSystem env (NewRemoteDB repo meta) =
  withTempFileContents ("" :: String) $ \path -> do
  void $ Site.backup (mockSite $ backupDir env) repo props Nothing path
  -- Force fetch backups to run again next time
  atomically $ writeTVar (envCachedRestorableDBs $ mockedEnv env) Nothing
  return $ waitOne $ waitForDeletions (mockedEnv env)
  where
    props = Map.fromList [("meta" :: String, LBS.unpack $ encode meta)]
mutateSystem MockedEnv {..} (TimeElapsed x) = do
  augment mockedTime (addTime (x * 1000000) <$>)
  return $ waitOne $ waitForDeletions mockedEnv
mutateSystem MockedEnv {..} (ShardingAssignmentChange (ShardAdded x)) = do
  modifyIORef mockedShardAssignment (Set.insert x)
  return noWait
mutateSystem MockedEnv {..} (ShardingAssignmentChange (ShardRemoved x)) = do
  modifyIORef mockedShardAssignment (Set.delete x)
  return $ waitOne $ waitForDeletions mockedEnv
mutateSystem MockedEnv {..} DBDownloaded = do
  restorePending <-
    atomically $ entriesRestoring <$> getEntries (envCatalog mockedEnv)
  if null restorePending
    then return noWait
    else do
      mockedCompleteRestore
      return $
        Wait
          [ timeoutWithError "waitForDownloadCompleted" 1 $
              atomically $ do
                entries <- getEntries (envCatalog mockedEnv)
                let restorePending' = entriesRestoring entries
                guard (length restorePending' == length restorePending - 1)
                guard (null $ entriesEphemeral entries)
          , -- the download is only processed *after* the next Janitor run
            -- it can still lead to a deletion in the follow-up Janitor run
            waitForDeletions mockedEnv
          ]

waitForDeletions :: HasCallStack => Env -> IO ()
waitForDeletions env =
  timeoutWithError "waitForDeletions" 1 $
    atomically $ do
      done <- null <$> readTVar (envDeleting env)
      when (not done) retry

timeoutWithError :: HasCallStack => String -> Seconds -> IO () -> IO ()
timeoutWithError msg seconds action = do
  res <- timeout seconds action
  case res of
    Nothing -> do
      logError $ "timeout: " <> msg
      error $ "timeout: " <> msg
    _ -> return ()

--------------------------------------------------------------------------------
-- Test assumptions

fetchBackupsTest :: IO ()
fetchBackupsTest = withSystemTempDirectory "backupdir" $ \backupDir -> do
  dbConf <- dbConfig
  withTEnv backupDir (dbConf backupDir) $ \TEnv {..} -> do
    let r = Repo "repo" "0001"
        m = defMeta
    _ <- mutateSystem tEnv (NewRemoteDB r m) >>= wait
    result <- forRestoreSitesM (mockedEnv tEnv) mempty listRestorable
    fmap HM.keys result @?= [[r]]

mockSiteTest :: IO ()
mockSiteTest = withSystemTempDirectory "backupdir" $ \backupDir -> do
  dbConf <- dbConfig
  withTEnv backupDir (dbConf backupDir) $ \TEnv {..} -> do
    let r = Repo "repo" "0001"
        m = defMeta
    void $ mutateSystem tEnv (NewRemoteDB r m)
    case fromPath Backup.Mock.mock (pack backupDir) of
      Nothing -> assertFailure "fromPath Mock backupDir"
      Just (Some site) -> do
        result <- listRestorable "" site
        HM.keys result @?= [r]

backupRestoreTest :: IO ()
backupRestoreTest = withSystemTempDirectory "backupdir" $ \backupDir -> do
  dbConf <- dbConfig
  withTEnv backupDir (dbConf backupDir) $ \TEnv {..} -> do
    let r = Repo "repo" "1"
        m = defMeta
    w1 <- mutateSystem tEnv (NewRemoteDB r m)
    w2 <- mutateSystem tEnv (ShardingAssignmentChange $ ShardAdded "1")
    runDatabaseJanitor (mockedEnv tEnv)
    w3 <- wait $ w1 <> w2
    w4 <- mutateSystem tEnv DBDownloaded
    waitAll $ w3 <> w4
    dbs <-
      filter hereDBs . listDatabasesResult_databases
        <$> listDatabases (mockedEnv tEnv) def
    map database_repo dbs @?= [r]
  where
    hereDBs Glean.Types.Database {..} =
      database_status /= DatabaseStatus_Restoring

openTest :: IO ()
openTest = withSystemTempDirectory "backupdir" $ \backupDir -> do
  dbConf <- dbConfig
  withTEnv backupDir (dbConf backupDir) $ \TEnv {..} -> do
    let r = Repo "repo" "1"
        m = defMeta
    w1 <- mutateSystem tEnv (NewRemoteDB r m)
    w2 <- mutateSystem tEnv (ShardingAssignmentChange $ ShardAdded "1")
    runDatabaseJanitor (mockedEnv tEnv)
    w3 <- wait $ w1 <> w2
    w4 <- mutateSystem tEnv DBDownloaded
    runDatabaseJanitor (mockedEnv tEnv)
    waitAll $ w3 <> w4
    runDatabaseJanitor (mockedEnv tEnv)
    openDBs <- readTVarIO $ envActive (mockedEnv tEnv)
    HM.keys openDBs @?= [r]

-- (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

module PublishShardsTest (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Foldable (toList)
import Data.Time
import DatabaseJanitorTest (
  dbConfig,
  makeFakeDB,
  serverConfig,
  withTest,
 )
import Glean.Database.Config (parseSchemaDir, schemaSourceDir)
import Glean.Database.Env
import Glean.Database.Schema (newDbSchema, readWriteContent)
import Glean.Database.Schema.Types (
  SchemaSelector (LatestSchemaAll),
 )
import Glean.Init (withUnitTest)
import Glean.Internal.Types
import Glean.Server.Sharding
import Glean.Types (Repo (Repo), DatabaseComplete (DatabaseComplete))
import Test.HUnit
import TestRunner
import Glean.Database.Meta (utcTimeToPosixEpochTime)

main :: IO ()
main =
  withUnitTest $
    testRunner $
      TestList
        [ TestLabel "publish incomplete DBs" publishIncompleteDBs
        , TestLabel "publish complete DBs" publishCompleteDBs
        ]

runTest :: (IO [Repo] -> IO ()) -> IO ()
runTest test = do
  published <- newTVarIO mempty
  let callback = atomically . writeTVar published
  withTest setupFakeDBs $ \evb cfgAPI dbdir backupdir ->
    withDatabases evb (dbConfig dbdir $ serverConfig backupdir) cfgAPI $ \env ->
      withAsync (dbUpdateNotifierThread env 0.0000001 callback) $ \_ ->
        test $
          atomically $ do
            repos <- readTVar published
            if null repos
              then retry
              else return $ toList repos

setupFakeDBs :: FilePath -> IO ()
setupFakeDBs dbdir = do
  now <- getCurrentTime
  schema <- parseSchemaDir schemaSourceDir
  schema <- newDbSchema schema LatestSchemaAll readWriteContent
  -- populate a dir with various DBs
  makeFakeDB schema dbdir incompleteRepo now incomplete Nothing
  makeFakeDB schema dbdir completeRepo now complete Nothing

incomplete :: p -> Completeness
incomplete _ = Incomplete DatabaseIncomplete_EMPTY

complete :: UTCTime -> Completeness
complete = Complete . (`DatabaseComplete` Nothing) . utcTimeToPosixEpochTime

incompleteRepo :: Repo
incompleteRepo = Repo "test" "incomplete"

completeRepo :: Repo
completeRepo = Repo "test" "complete"

publishIncompleteDBs :: Test
publishIncompleteDBs = TestCase $
  runTest $ \readShards -> do
    repos <- readShards
    assertBool "Incomplete DBs are published" (incompleteRepo `elem` repos)

publishCompleteDBs :: Test
publishCompleteDBs = TestCase $
  runTest $ \readShards -> do
    repos <- readShards
    assertBool "Complete DBs are published" (completeRepo `elem` repos)

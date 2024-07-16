{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module PublishShardsTest (main) where

import Control.Concurrent.Async
import Util.STM
import Data.Default
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
        , TestLabel "stop publishing after timeout" stopPublishingCompletedDBs
        ]

runTest :: Bool -> (IO [Repo] -> IO ()) -> IO ()
runTest term test = do
  published <- newTVarIO mempty
  let callback = atomically . writeTVar published
  terminating <- newTVarIO term
  withTest setupFakeDBs setupCloudDBs $ \evb cfgAPI dbdir backupdir ->
    withDatabases evb (dbConfig dbdir $ serverConfig backupdir) cfgAPI $ \env ->
      withAsync (
          dbUpdateNotifierThread
          env
          0.0000001
          (readTVar terminating)
          callback
        ) $ \_ ->
            test $
              atomically $ do
                repos <- readTVar published
                if null repos
                  then retry
                  else return $ toList repos

setupCloudDBs :: FilePath -> IO ()
setupCloudDBs _ = return ()

setupFakeDBs :: FilePath -> IO ()
setupFakeDBs dbdir = do
  now <- getCurrentTime
  schema <- parseSchemaDir schemaSourceDir
  schema <- newDbSchema Nothing schema LatestSchemaAll readWriteContent def
  -- populate a dir with various DBs
  makeFakeDB schema dbdir incompleteRepo now incomplete id
  makeFakeDB schema dbdir completeRepo now complete id

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
  runTest False $ \readShards -> do
    repos <- readShards
    assertBool "Incomplete DBs are published" (incompleteRepo `elem` repos)

publishCompleteDBs :: Test
publishCompleteDBs = TestCase $
  runTest False $ \readShards -> do
    repos <- readShards
    assertBool "Complete DBs are published" (completeRepo `elem` repos)

stopPublishingCompletedDBs :: Test
stopPublishingCompletedDBs = TestCase $
  runTest True $ \readShards -> do
    repos <- readShards
    assertBool
      "Still publishing incomplete DBs after timeout"
      (incompleteRepo `elem` repos)
    assertBool
      "Stopped publishing completed DBs after timeout"
      (completeRepo `notElem` repos)

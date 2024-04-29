{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

-- | Library for running an indexer and running unit tests on the
-- resulting Repo.
module Glean.Regression.Test
  ( -- * your test code
    TestIndex
    -- * make your main
  , mainTestIndex
  , mainTestIndexExternal
  , mainTestIndexGeneric
  , mainTestIndexXlang
  ) where

import Foreign.Marshal.Utils
import qualified Data.Text as Text
import Options.Applicative
import System.FilePath
import System.IO.Temp
import Test.HUnit
import TestRunner
import Util.IO

import Glean
import Glean.LocalOrRemote
import Glean.Indexer
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot.Options
import Glean.Util.Some (Some(..))

type TestIndex = IO (Some LocalOrRemote, Repo) -> Test

withOutputDir :: String -> Maybe FilePath -> (FilePath -> IO a) -> IO a
withOutputDir _dir (Just output) act = act output
withOutputDir dir Nothing act = withSystemTempDirectory dir act

-- | Run a test with an arbitrary indexer
mainTestIndex
  :: String -- ^ just a string to identify this test
  -> Indexer opts
  -> TestIndex
  -> IO ()
mainTestIndex dir indexer testIndex =
  mainTestIndexGeneric
    (driverFromIndexer indexer)
    (pure ())
    dir
    (\_ _ _ _ -> testIndex)

-- | Run a test with an external indexer
mainTestIndexExternal
  :: String -- ^ just a string to identify this test
  -> TestIndex
  -> IO ()
mainTestIndexExternal dir testIndex =
  mainTestIndexGeneric externalDriver (pure ()) dir (\_ _ _ _ -> testIndex)

createTestConfig :: Text.Text -> String -> String -> Config -> TestConfig
createTestConfig repo group outDir cfg =
   TestConfig
      { testRepo = Repo repo (Text.pack group)
      , testOutput = if null group
          then outDir
          else outDir </> group
      , testRoot = cfgRoot cfg
      , testProjectRoot = cfgProjectRoot cfg
      , testGroup = group
      , testSchemaVersion = cfgSchemaVersion cfg
      }

-- | Run a test with an arbitrary indexer
mainTestIndexGeneric
  :: Driver driverOpts
  -> Parser extraOpts -- ^ parser for extra options to recognise
  -> String -- ^ just a string to identify this test
  -> (extraOpts -> driverOpts -> Config -> TestConfig -> TestIndex)
  -> IO ()
mainTestIndexGeneric driver extraOptParser dir testIndex = do
  let
    indexer = driverIndexer driver
    parse = (,) <$> indexerOptParser indexer <*> extraOptParser
  withUnitTestOptions (optionsWith parse) $
          \ action (mkcfg, (driverOpts, extraOpts)) -> do
    -- TODO: we're using the options for snapshot tests which have a
    -- couple of flags that don't make sense here: --replace for
    -- example.
    cfg <- mkcfg
    let
      theGroups = driverGroups driver driverOpts
      (platforms, mkLabel) = if null theGroups
          then (["testhash"], const dir)
          else (theGroups, \group -> dir <> " : " <> group)
    withOutputDir dir (cfgOutput cfg) $ \ outDir -> do
      let
        withPlatformTest :: String -> (Test -> IO a) -> IO a
        withPlatformTest platform fn = do
          let testConfig = createTestConfig "test" platform outDir cfg
              withSetup :: ((Some LocalOrRemote, Repo) -> IO a) -> IO a
              withSetup f =
                withTestBackend testConfig $ \backend -> do
                  driverCreateDatabase driver driverOpts backend
                    (indexerRun indexer) testConfig
                  f (backend, testRepo testConfig)

          withLazy withSetup $ \get ->
            fn $ TestLabel (mkLabel platform) $
              testIndex extraOpts driverOpts cfg testConfig get

      withMany withPlatformTest platforms $ \tests ->
        testRunnerAction action (TestList tests)

-- | Run a test which runs two indexers, the one in the driver
--   plus an extra one
mainTestIndexXlang
  :: (Driver driverOpts, Text.Text) -- ^ Driver and name of db to be created
  -> (Indexer opts, Text.Text) -- ^ Indexer and name of db to be created
  -> String -- ^ just a string to identify this test
  -> TestIndex
  -> IO ()
mainTestIndexXlang
  (driver, repoName) (indexer', repoName') dir testIndex = do
  let
    indexer = driverIndexer driver
    parse = (,) <$> indexerOptParser indexer <*> indexerOptParser indexer'
  withUnitTestOptions (optionsWith parse) $
          \ action (mkcfg, (driverOpts, driverOpts')) -> do
    -- TODO: we're using the options for snapshot tests which have a
    -- couple of flags that don't make sense here: --replace for
    -- example.
    cfg <- mkcfg
    let
      -- We ignore groups of second driver
      theGroups = driverGroups driver driverOpts
      (platforms, mkLabel) = if null theGroups
          then (["testhash"], const dir)
          else (theGroups, \group -> dir <> " : " <> group)
    withOutputDir dir (cfgOutput cfg) $ \ outDir -> do
      let
        withPlatformTest :: String -> (Test -> IO a) -> IO a
        withPlatformTest platform fn = do
          let testConfig = createTestConfig repoName platform outDir cfg
              testConfig' = testConfig {
                testRepo = Repo repoName' (Text.pack platform)
              }
              withSetup :: ((Some LocalOrRemote, Repo) -> IO a) -> IO a
              withSetup f =
                withTestBackend testConfig $ \backend -> do
                  driverCreateDatabase driver driverOpts backend
                    (indexerRun indexer) testConfig
                  defaultCreateDatabase driverOpts' backend
                    (indexerRun indexer') testConfig'
                  f (backend, testRepo testConfig')

          withLazy withSetup $ \get ->
            fn $ TestLabel (mkLabel platform) $ testIndex get

      withMany withPlatformTest platforms $ \tests ->
        testRunnerAction action (TestList tests)

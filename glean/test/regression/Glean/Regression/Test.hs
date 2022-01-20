-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

{-# LANGUAGE ApplicativeDo #-}

-- | Library for running an indexer and running unit tests on the
-- resulting Repo.
module Glean.Regression.Test
  ( -- * your test code
    TestIndex
    -- * make your main
  , mainTestIndexExternal
  , mainTestIndexGeneric
  ) where

import Foreign.Marshal.Utils
import qualified Data.Text as Text
import System.FilePath
import System.IO.Temp
import Test.HUnit
import TestRunner
import Util.IO

import Glean
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot.Options
import Glean.Util.Some (Some(..))

type TestIndex = IO (Some Backend, Repo) -> Test

withOutputDir :: String -> Maybe FilePath -> (FilePath -> IO a) -> IO a
withOutputDir _dir (Just output) act = act output
withOutputDir dir Nothing act = withSystemTempDirectory dir act

-- | Run a test with an external indexer
mainTestIndexExternal
  :: String -- ^ just a string to identify this test
  -> TestIndex
  -> IO ()
mainTestIndexExternal dir testIndex =
  mainTestIndexGeneric externalDriver dir (\_ _ _ -> testIndex)

-- | Run a test with an arbitrary indexer
mainTestIndexGeneric
  :: Driver opts
  -> String -- ^ just a string to identify this test
  -> (opts -> String -> TestConfig -> TestIndex)
  -> IO ()
mainTestIndexGeneric driver dir testIndex = do
  let
    indexer = driverIndexer driver
    parse = indexerOptParser indexer
  withUnitTestOptions (optionsWith parse) $ \ action (mkcfg, opts) -> do
    -- TODO: we're using the options for snapshot tests which have a
    -- couple of flags that don't make sense here: --replace for
    -- example.
    cfg <- mkcfg
    let
      theGroups = driverGroups driver opts
      (platforms, mkLabel) = if null theGroups
          then (["testhash"], const dir)
          else (theGroups, \group -> dir <> " : " <> group)
    withOutputDir dir (cfgOutput cfg) $ \ outDir -> do
      let
        withPlatformTest :: String -> (Test -> IO a) -> IO a
        withPlatformTest platform fn = do
          let testConfig = TestConfig
                { testRepo = Repo "test" (Text.pack platform)
                , testOutput = if null platform
                    then outDir
                    else outDir </> platform
                , testRoot = cfgRoot cfg
                , testProjectRoot = cfgProjectRoot cfg
                , testGroup = platform
                , testSchemaVersion = cfgSchemaVersion cfg
                }
          withLazy (withTestDatabase (indexerRun indexer opts) testConfig) $
            \get -> fn $ TestLabel (mkLabel platform) $
              testIndex opts platform testConfig get

      withMany withPlatformTest platforms $ \tests ->
        testRunnerAction action (TestList tests)

-- Copyright 2004-present Facebook. All Rights Reserved.
{-# LANGUAGE ApplicativeDo, NamedFieldPuns #-}
-- | Library for running indexer (using ExternalLib)
-- and running unit tests on the resulting Repo
module Glean.Regression.Driver.TestIndex
  ( -- * your test code
    TestIndex
    -- * make your main
  , mainTestIndex
  ) where

import Control.Monad
import System.FilePath
import System.Directory
import System.IO.Temp
import Test.HUnit
import TestRunner

import Glean hiding (Config(..))
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Driver.ExternalLib
import Glean.Regression.Test
import Glean.Util.Some (Some(..))

type TestIndex = Some Backend -> Repo -> Test

withOutputDir :: String -> Maybe FilePath -> (FilePath -> IO a) -> IO a
withOutputDir _dir (Just output) act = act output
withOutputDir dir Nothing act = withSystemTempDirectory dir act

mainTestIndex :: String -> TestIndex -> IO ()
mainTestIndex dir testIndex =
  withUnitTestOptions (optionsWith extOptions) $ \ (mkcfg, ext) -> do
    cfg <- mkcfg
    testProjectRoot <- makeAbsolute (cfgProjectRoot cfg)
    testRoot <- makeAbsolute (cfgRoot cfg)
    let platforms = if null (extGroups ext)
          then ["testhash"]
          else extGroups ext
    withOutputDir dir (cfgOutput cfg) $ \ outDir ->
      forM_ platforms $ \ platform -> do
        let testConfig = TestConfig
              { testRepoName = "test"
              , testRepoHash = platform
              , testOutput = if null platform
                  then outDir
                  else outDir </> platform
              , testRoot
              , testProjectRoot
              , testGroup = platform
              , testSchemaVersion = cfgSchemaVersion cfg
              }
        withTestDatabase (execExternal ext) testConfig $ \ backend repo ->
          testRunner $
            TestLabel (dir <> " : " <> testGroup testConfig) $
              testIndex (Some backend) repo

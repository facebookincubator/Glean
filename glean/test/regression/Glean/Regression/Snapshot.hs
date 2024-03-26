{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

-- | Make a snapshot regression test out of a 'Driver'.
--
-- A snapshot regression test runs queries specified in @.query@ files
-- against some indexed sample source code and compares the output
-- against the snapshot saved in the correspond @.out@ file.

module Glean.Regression.Snapshot
  ( testMain
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import qualified Test.HUnit as HUnit

import TestRunner
import Util.JSON.Pretty ()
import Util.Log

import Glean (Backend)
import Glean.Indexer
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot.Options
import Glean.Regression.Snapshot.Query
import Glean.Regression.Snapshot.Result
import Glean.Regression.Snapshot.Transform
import Glean.Types hiding (Success, Failure)

-- | From 'testRoot' this locates all subdirectories below the root
-- that contain at least one ".out" file.
discoverTests :: FilePath -> IO [FilePath]
discoverTests root = go ""
  where
  go dir = do
    xs <- listDirectory (root </> dir)
    dirs <- filterM (doesDirectoryExist . ((root </> dir) </>)) xs
    subdirTests <- concat <$> mapM (go . (dir </>)) dirs
    return $
      if any (".out" `isSuffixOf`) xs
        then dir : subdirTests
        else subdirTests

-- | Run one test and its *.query files, return (*.out, *.perf) 'FilePath'.
runTest
  :: Driver opts
  -> opts
  -> FilePath    -- ^ test root, canonicalized
  -> TestConfig
  -> IO [FilePath]
runTest driver@Driver{..} driverOpts root testIn =
  withTestBackend testIn $ \backend -> do
    let index = indexerRun driverIndexer
    driverCreateDatabase driverOpts backend index testIn
    runQueries backend driver root testIn

-- | Run the queries
runQueries
  :: Backend b
  => b
  -> Driver opts
  -> FilePath  -- ^ test root, canonicalized
  -> TestConfig
  -> IO [FilePath]
runQueries backend Driver{..} root test  = do
  queries <- get_queries root mempty (testRoot test)
  when (null queries) $
    throwIO $ ErrorCall $ "no .query files found; root=" <> show root
  fmap concat $ forM (Map.elems queries) $ \query -> do
    (result, perf) <- runQuery
      backend
      (testRepo test)
      (defaultTransforms <> driverTransforms)
      query
    let base = testOutput test </> dropExtension (takeFileName query)
        out = base <.> "out"
        perfOut = base <.> "perf"
    writeFile out result
    mapM_ (writeFile perfOut) perf
    return $ if isJust perf then [out,perfOut] else [out]
  where
  get_queries root qs path = do
    files <- listDirectory path
    let qs' = Map.union qs $ Map.fromList
          [ (file, path </> file)
          | file <- files, ".query" `isExtensionOf` file ]
    if equalFilePath path root
      then return qs'
      else get_queries root qs' $ takeDirectory path

-- | Outputs to compare/regenerate.
--
-- When (re)generating the golden outputs, we designate one group (the first
-- in the Driver's list) as the base group and generate the base golden output
-- from it. For any other group, if the output differs from the base one we
-- generate a group-specific output file (`xxx.<group>.out`). During testing
-- we prefer those group-specific outputs to the base ones.
data Outputs = Outputs
  { outGenerated :: FilePath
      -- ^ generated output
  , outGoldenBase :: FilePath
      -- ^ base golden output
  , outGoldenGroup :: FilePath
      -- ^ golden output for this group (can be same as base)
  }

-- | Run one test and check the *.out files against the golden *.out files.
executeTest
  :: Config
  -> Driver opts
  -> opts
  -> String  -- ^ group which produces the base golden output ('outGoldenBase')
  -> String  -- ^ current group
  -> (Outputs -> IO Result)  -- ^ compare or overwrite golden outputs
  -> FilePath
  -> IO Result
executeTest cfg driver driverOpts base_group group diff subdir =
  with_outdir $ \outdir -> do
  let test = TestConfig
        { testRepo =
            let hash = map (\c -> if c == '/' then '_' else c) subdir
            in Repo "test" (Text.pack hash)
        , testOutput =
            outdir </> (if null group then id else (group </>)) subdir
        , testRoot = cfgRoot cfg </> subdir
        , testProjectRoot = cfgProjectRoot cfg
        , testGroup = group
        , testSchemaVersion = cfgSchemaVersion cfg
        }
  createDirectoryIfMissing True $ testOutput test
  outputs <- runTest driver driverOpts (cfgRoot cfg) test
  compareOutputs test diff base_group group outputs
  where
    with_outdir f = case cfgOutput cfg of
      Just dir -> f dir
      Nothing -> withSystemTempDirectory "glean-regression" f

compareOutputs
  :: TestConfig
  -> (Outputs -> IO Result)  -- ^ compare or overwrite golden outputs
  -> String
  -> String
  -> [FilePath]
  -> IO Result
compareOutputs test diff base_group group outputs = do
  fmap mconcat $ forM outputs $ \output -> do
    let base = testRoot test </> takeFileName output
        specific
          | group == base_group = base
          | otherwise = outputFileForGroup base group
    diff Outputs
      { outGenerated = output
      , outGoldenBase = base
      , outGoldenGroup = specific
      }

outputFileForGroup :: FilePath -> String -> FilePath
outputFileForGroup base group = addExtension (stem <.> group) ext
  where (stem,ext) = splitExtension base

-- | Regenerate golden outputs. Do nothing if 'outGoldenBase' exists and is the
-- same as 'outGenerated'. Otherwise, copy 'outGenerated' to 'outGoldenGroup'
-- (which might be the same as 'outGoldenBase').
regenerate :: Outputs -> IO Result
regenerate Outputs{..} = do
  base <- do
    ex <- doesFileExist outGoldenBase
    if ex
      then Just <$> BS.readFile outGoldenBase
      else return Nothing
  generated <- BS.readFile outGenerated
  -- this will either overwrite base or generate a group-specific output
  when (base /= Just generated) $ BS.writeFile outGoldenGroup generated
  return (Success [outGoldenGroup])

-- | Compare the generated output with the appropriate golden output via `diff`.
-- This uses 'outGoldenGroup' if it exists and 'outGoldenBase' otherwise.
diff :: Outputs -> IO Result
diff Outputs{..} = do
  spec <- doesFileExist outGoldenGroup
  (e, sout, serr) <- readProcessWithExitCode
    "diff"
    [outGenerated, if spec then outGoldenGroup else outGoldenBase]
    ""
  return $ case e of
    ExitSuccess -> Success []
    ExitFailure n -> failure
      $ takeFileName outGenerated ++
        if n == 1
          then ": unexpected result\n" ++ sout
          else ": fatal error\n" ++ serr

-- | Convert a 'Driver' into a regression test over --root parameter.
--
--  Normal mode: find all /testRoot/*/*/ directories and run all tests.
--
-- With --replace : find all /testRoot/*/*/ directories and update all golden
-- *.out files.
testMain :: Driver opts -> IO ()
testMain driver = do
  let parse = indexerOptParser (driverIndexer driver)
  withUnitTestOptions (optionsWith parse) $ \act (mk_cfg, indexerOpts) -> do
    cfg <- mk_cfg
    testAll act cfg driver indexerOpts


testAll :: TestAction -> Config -> Driver opts -> opts -> IO ()
testAll act cfg driver opts = do
  tests' <- if null $ cfgTests cfg
    then discoverTests $ cfgRoot cfg
    else return $ cfgTests cfg

  when (null tests') $
    die $ "No .out files found under " <> cfgRoot cfg

  let tests = filter (`notElem` cfgOmitTests cfg) tests'

  let groups
        | null fromDriver = [""]
        | otherwise = fromDriver
        where fromDriver = driverGroups driver opts

  case cfgReplace cfg of
    Just root ->
      let cfg' = cfg { cfgRoot = root }
      in
      testRunnerAction act $
        HUnit.TestList $ flip map tests $ \subdir ->
          HUnit.TestLabel subdir $ HUnit.TestCase $ do
            -- With --replace, we have to run all groups serially,
            -- because if we run them in parallel then it would be
            -- non-deterministic whether we overwrite the output file
            -- for the base group or a specific group.  Also we
            -- wouldn't know which files we can remove in
            -- removeNonRegenerated below.
            result <- mconcat $ flip map groups $ \g ->
              executeTest cfg' driver opts (head groups) g regenerate subdir
            removeNonRegenerated root subdir result
            toHUnit result

    Nothing ->
      testRunnerAction act $
        HUnit.TestList $ flip map groups $ \g ->
          (if null g then id else HUnit.TestLabel g) $
            HUnit.TestList $ flip map tests $ \subdir ->
              HUnit.TestLabel subdir $ HUnit.TestCase $
                executeTest cfg driver opts (head groups) g diff subdir
                  >>= toHUnit

  where
      -- clean-up .out or .perf files which weren't regenerated
      -- for instance, if a .query file was removed.
      removeNonRegenerated _ _ Failure{} = return ()
      removeNonRegenerated root test (Success regenerated) = do
          let path = root </> test
          allFiles <- listDirectory path
          let allOutFiles = filter
                (\x -> takeExtension x == ".out" || takeExtension x == ".perf")
                ((path </>) <$> allFiles)
          let toDelete = filter (`notElem` regenerated) allOutFiles
          when (not (null toDelete)) $
            logInfo $ "Removing output files that were not regenerated: " <>
              intercalate "," toDelete
          mapM_ removePathForcibly toDelete

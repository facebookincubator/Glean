{-# LANGUAGE ApplicativeDo #-}
-- | Make a regression test out of a 'Driver'
module Glean.Regression.Test
  ( -- * Primary operation
    testMain
  , testAll
  -- * helpers
  , discoverTests
  , withTestDatabase
  , executeTest
  )
where

import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import qualified Test.HUnit as HUnit

import TestRunner (testRunner)
import Util.JSON.Pretty ()

import Glean.Backend (Backend)
import qualified Glean.Backend as Backend
import Glean.Database.Test
import Glean.Database.Types (Env)
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Query
import Glean.Regression.Result
import Glean.Regression.Transform
import qualified Glean.Types as Thrift

-- | From 'testRoot' this locates all /testRoot/*/*/ directories, those
-- that are exactly two levels below 'testRoot'
discoverTests :: FilePath -> IO [FilePath]
discoverTests path = do
  dirs <- list_subdirs path
  fmap concat $ forM dirs $ \dir ->
    map (dir </>) <$> list_subdirs (path </> dir)
  where
    list_subdirs path = do
      xs <- listDirectory path
      filterM (\x -> doesDirectoryExist $ path </> x) xs

-- | Execute a 'Driver' against a temporary db and execute provided 'action'.
withTestDatabase
  :: Generator
  -> TestConfig
  -> (forall e. Backend e => e -> Thrift.Repo -> IO a)
  -> IO a
withTestDatabase = withTestEnvDatabase

withTestEnvDatabase
  :: Generator
  -> TestConfig
  -> (Env -> Thrift.Repo -> IO a)
  -> IO a
withTestEnvDatabase generator test action =
  let
    settings = [setRoot $ testOutput test </> "db"] <>
      maybeToList (setSchemaVersion . fromIntegral <$> testSchemaVersion test)
  in
  withTestEnv settings $ \backend -> do
  Backend.fillDatabase backend repo "" (die "repo already exists") $ do
    generator test backend repo
  action backend repo
  where
    repo = Thrift.Repo
      (Text.pack $ testRepoName test)
      (Text.pack $ testRepoHash test)

-- | Run one test and its *.query files, return (*.out, *.perf) 'FilePath'.
runTest :: Driver -> TestConfig -> IO [FilePath]
runTest Driver{..} testIn =
  withTestEnvDatabase driverGenerator testIn $
    fromMaybe queryMakeOuts driverMakeOuts testIn
  where
    queryMakeOuts test backend repo = do
      queries <- get_queries (testProjectRoot test) mempty (testRoot test)
      fmap concat $ forM (Map.elems queries) $ \query -> do
        (result, perf) <- runQuery
          backend
          repo
          (defaultTransforms <> driverTransforms)
          query
        let base = testOutput test </> dropExtension (takeFileName query)
            out = base <.> "out"
            perfOut = base <.> "perf"
        writeFile out result
        mapM_ (writeFile perfOut) perf
        return $ if isJust perf then [out,perfOut] else [out]

    get_queries root qs path = do
      files <- listDirectory path
      let qs' = Map.union qs $ Map.fromList
            [ (file, path </> file)
            | file <- files, ".query" `isExtensionOf` file ]
      if path == root
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
  -> Driver
  -> String  -- ^ group which produces the base golden output ('outGoldenBase')
  -> String  -- ^ current group
  -> (Outputs -> IO Result)  -- ^ compare or overwrite golden outputs
  -> FilePath
  -> IO Result
executeTest cfg driver base_group group diff subdir =
  with_outdir $ \outdir -> do
  let test = TestConfig
        { testRepoName = "test"
        , testRepoHash = map (\c -> if c == '/' then '_' else c) subdir
        , testOutput =
            outdir </> (if null group then id else (group </>)) subdir
        , testRoot = cfgRoot cfg </> subdir
        , testProjectRoot = cfgProjectRoot cfg
        , testGroup = group
        , testSchemaVersion = cfgSchemaVersion cfg
        }
  createDirectoryIfMissing True $ testOutput test
  outputs <- runTest driver test
  fmap mconcat $ forM outputs $ \output -> do
    let base = testRoot test </> takeFileName output
        specific
          | group == base_group = base
          | otherwise =
              let (stem,ext) = splitExtension base
              in
              addExtension (stem <.> group) ext
    diff Outputs
      { outGenerated = output
      , outGoldenBase = base
      , outGoldenGroup = specific
      }
  where
    with_outdir f = case cfgOutput cfg of
      Just dir -> f dir
      Nothing -> withSystemTempDirectory "glean-regression" f

-- | Regenerate golden outputs. Do nothing if 'outGoldenBase' exists and is the
-- same as 'outGenerated'. Otherwise, copy 'outGenerated' to 'outGoldenGroup'
-- (which might be the same as 'outGoldenBase').
regenerate :: Outputs -> IO Result
regenerate Outputs{..} = do
  base <- do
    ex <- doesFileExist outGoldenBase
    if ex
      then Just <$> BSL.readFile outGoldenBase
      else return Nothing
  generated <- BSL.readFile outGenerated
  -- this will either overwrite base or generate a group-specific output
  when (base /= Just generated) $ BSL.writeFile outGoldenGroup generated
  return Success

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
    ExitSuccess -> Success
    ExitFailure n -> failure
      $ takeFileName outGenerated ++
        if n == 1
          then ": unexpected result\n" ++ sout
          else ": fatal error\n" ++ serr

-- | Wrap 'executeTest' into an 'HUnit.Test'
toHUnit :: Config -> Driver -> String -> String -> FilePath -> HUnit.Test
toHUnit cfg driver base_group group subdir =
  HUnit.TestLabel subdir $ HUnit.TestCase $ do
    r <- executeTest cfg driver base_group group diff subdir
    case r of
      Success -> return ()
      Failure msg -> HUnit.assertFailure $ unlines $ msg []

-- | Convert a 'Driver' into a regression test over --root parameter.
--
--  Normal mode: find all /testRoot/*/*/ directories and run all tests.
--
-- With --replace : find all /testRoot/*/*/ directories and update all golden
-- *.out files.
testMain :: Driver -> IO ()
testMain driver = withUnitTestOptions options $ \mk_cfg -> do
  cfg <- mk_cfg
  testAll cfg driver


testAll :: Config -> Driver -> IO ()
testAll cfg driver = do
  tests <- if null $ cfgTests cfg
    then discoverTests $ cfgRoot cfg
    else return $ cfgTests cfg
  let groups
        | null $ driverGroups driver = [""]
        | otherwise = driverGroups driver

  if cfgReplace cfg
    then do
      forM_ tests $ \test -> do
        -- remove all .out files
        let path = cfgRoot cfg </> test
        xs <- listDirectory path
        forM_ xs $ \x ->
          when (takeExtension x == ".out" || takeExtension x == ".perf") $
            removePathForcibly $ path </> x
        -- regenerate outputs - use the first group as the base
        forM_ groups $ \group ->
          executeTest cfg driver (head groups) group regenerate test
    else do
      testRunner $ HUnit.TestList
        [ (if null g then id else HUnit.TestLabel g)
            $ HUnit.TestList
            $ map (toHUnit cfg driver (head groups) g) tests
          | g <- groups ]

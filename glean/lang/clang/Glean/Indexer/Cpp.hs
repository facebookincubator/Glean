{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, ApplicativeDo #-}
module Glean.Indexer.Cpp
  ( indexerWith, indexer, indexerNoDeriv, Clang(..)
  , findExecutableRecursive ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Proxy
import Options.Applicative
import qualified System.Console.ANSI as ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Thrift.Protocol (deserializeGen)
import Thrift.Protocol.Compact (Compact)
import Util.List (chunk)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text

import Facebook.Fb303
import Facebook.Service

#ifdef FBTHRIFT
import qualified Thrift.Server.CppServer as ThriftServer
#else
import qualified Thrift.Server.HTTP as ThriftServer
#endif

import Glean
  ( sendBatchAndWait
  , clientConfig_serv
  , showRepo
  , completePredicates
  , CompletePredicates (CompletePredicates_axiom), CompleteAxiomPredicates(..)
  )
import Glean.Remote (thriftBackendClientConfig)
import Glean.Indexer
import Glean.LocalOrRemote ( BackendKind(..),
  LocalOrRemote(..), serializeInventory )
import Glean.Util.Service
import qualified Glean.Interprocess.Worklist as Worklist
import qualified Glean.Handler as GleanHandler
import Data.Aeson (decode, Object, Value (String))
import Data.Foldable (toList)

data Clang = Clang
  { clangIndexBin     :: Maybe FilePath -- ^ path to @clang-index@ binary
  , clangDeriveBin    :: Maybe FilePath -- ^ path to @clang-derive@ binary
  , clangCompileDBDir :: Maybe FilePath
      -- ^ (optional) path to pre-existing @compile_commands.json@
  , clangTarget       :: Maybe String -- ^ (optional) target to index
  , clangJobs         :: Int -- ^ number of indexers to run concurrently
  , clangBatch        :: Int -- ^ dump indexer output every N files
  , clangVerbose      :: Bool -- ^ display debugging information
  , clangProgress     :: Bool -- ^ display indexing progress
  , clangCmakeOpts    :: [String] -- ^ extra flags to cmake
  , clangIncremental  :: Bool -- ^ use incremental derivation
  , clangOutDir       :: Maybe FilePath -- ^ where to put output files
  , clangSkipIndexing :: Bool -- ^ use existing indexer output files
  } deriving Show

options :: Parser Clang
options = do
  clangIndexBin <- optional $ strOption $
    long "indexer" <>
    help "path to the clang-index binary"
  clangDeriveBin <- optional $ strOption $
    long "deriver" <>
    help "path to the clang-derive binary"
  clangCompileDBDir <- optional $ strOption $
    long "cdb-dir" <>
    help "path to a directory containing an existing compile_commands.json file"
  clangTarget <- optional $ strOption $
    long "c-target" <>
    help "target to index (e.g. //path/to:target)"
  clangJobs <- option auto $
    short 'j' <>
    long "jobs" <>
    value 1 <>
    help "run N indexers in parallel"
  clangBatch <- option auto $
    short 'n' <>
    long "batch" <>
    value 100 <>
    help "index files in batches of N"
  clangVerbose <- switch $
    short 'v' <>
    long "verbose" <>
    help "Enable verbose logging from subprocesses"
  clangProgress <- switch $
    long "progress" <>
    help "Display indexing progress even in verbose mode"
  clangCmakeOpts <- many $ strOption $
    long "cmake-opt" <>
    help "Extra flag to pass to cmake"
  clangOutDir <- optional $ strOption $
    long "out" <>
    metavar "DIR" <>
    help "Directory to save indexer output"
  clangSkipIndexing <- switch $
    long "skip-indexing" <>
    help ("If indexing has already been done, " <>
      "use the existing indexer output files. Requires --out DIR")
  clangIncremental <- pure False -- internal, not a CLI flag
  return Clang{..}

-- | Standard indexer, that also runs the deriver
indexer :: Indexer Clang
indexer = indexerWith True

-- | Indexing only, no deriving
indexerNoDeriv :: Indexer Clang
indexerNoDeriv = indexerWith False

-- | C++ indexer. The 'Bool' specifies whether the indexer
--   also runs the deriver. It creates a compilation database, either
--   using CMake or taking the one provided as param, and then creates
--   glean facts from it.
indexerWith :: Bool -> Indexer Clang
indexerWith deriveToo = Indexer {
  indexerShortName = "cpp-cmake",
  indexerDescription = "Index C++ code with CMake (via Clang)",
  indexerOptParser = options,
  indexerRun = \clang@Clang{..} backend repo IndexerParams{..} -> do
    -- indexing
    let tmpDir        = indexerOutput
        buildDir      = indexerOutput </> "build"
        outDir        = fromMaybe (indexerOutput </> "indexer") clangOutDir
        inventoryFile = tmpDir </> "inventory.data"
    createDirectoryIfMissing True buildDir
    createDirectoryIfMissing True outDir
    generateInventory backend repo inventoryFile
    compileDBDir <-
      case clangCompileDBDir of
        Nothing  ->
          case clangTarget of
            Nothing -> cmake clang indexerRoot buildDir >> return buildDir
            Just target -> do
              cdb <- runBuckFullCompilationDatabase target
              return $ takeDirectory cdb
        Just dir -> return dir

    index clang inventoryFile indexerRoot compileDBDir outDir

    files <- map (outDir </>) . filter ("indexer-" `isPrefixOf`) <$>
      listDirectory outDir
    writeToDB backend repo files

    -- deriving
    when deriveToo $ do
      completePredicates backend repo $
        CompletePredicates_axiom CompleteAxiomPredicates
      derive clangVerbose clangDeriveBin backend repo
  }

  where
    runBuckFullCompilationDatabase :: String -> IO String
    runBuckFullCompilationDatabase target = do
      let args =
            [ "--isolation-dir=glean-indexer"
            , "build"
            , target <> "[full-compilation-database]"
            , "--show-full-json-output"
            ]
      (exit, out, err) <- readProcessWithExitCode "buck" args ""
      case exit of
        ExitSuccess -> do
          let json :: Maybe Object = decode $ BL.pack $ head $ lines out
          case json of
            Nothing -> invalidJson out
            Just obj -> do
              case toList obj of
                [cdb] -> case cdb of
                  String cdb -> return $ Text.unpack cdb
                  _ -> invalidJson out
                _ -> invalidJson out
        ExitFailure i -> error $ unwords (args ++
          [ "returned exit code", show i
          , "with output", out
          , "and error", err])
        where
          invalidJson out = error $ "buck returned invalid JSON: " ++ out

    generateInventory backend repo outFile =
      serializeInventory backend repo >>= BS.writeFile outFile

    cmake Clang{..} srcDir tmpDir = withExe "cmake" Nothing $ \cmakeBin ->
      spawnAndConcurrentLog clangVerbose cmakeBin $
        [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
        , "-S", srcDir
        , "-B", tmpDir
        ] <> clangCmakeOpts

    index Clang{..} inventory srcDir buildDir tmpDir =
      withExe "clang-index" clangIndexBin $ \clangIndex -> do
      let args =
            [ "-cdb_dir", buildDir
            , "-cdb_target", "all"
            , "-root", srcDir
            , "--inventory", inventory
            , "-logtostderr"
            ]

      -- get the total number of source files
      sourceCount <- do
        let pargs = args ++ ["--print_sources_count"]
        s <- readProcess clangIndex pargs ""
        case reads s of
          [(sources,"")] -> return sources
          _ -> error $ unwords (clangIndex:pargs)
            ++ " produced unexpect output \"" ++ s ++ "\""

      case sourceCount of
        0 -> do
          -- TODO: should this be an error?
          putStrLn "No source files to index"
          return []
        _ ->
          -- set up worklist
          let ranges = map (\(i,n) -> Worklist.Range i (i+n)) $
                  chunk clangJobs sourceCount
              !workers = length ranges
          in
          Worklist.withTemp ranges $ \wfile worklist ->

          -- progress and logging
          (if clangProgress || not clangVerbose
            then withProgress worklist clangJobs sourceCount
            else id) $
          withLog clangVerbose (void . evaluate . length) $ \stream -> do

          -- run workers
          let dataFile i = tmpDir </> "indexer-" <> show i <> ".data"
              workerargs i = args ++
                [ "-dump", dataFile i
                , "-dump_every", show clangBatch
                , "--work_file", wfile
                , "--worker_index", show i
                , "--worker_count", show workers
                ]
          currentDir <- getCurrentDirectory
          let cdUp = not $ isPathPrefixOf currentDir buildDir
          unless clangSkipIndexing $
            forConcurrently_ [0 .. workers-1] $ \i -> bracket
            -- createProcess_ because we don't want the stdout/stderr handles
            -- to be closed
            (createProcess_
                "Cpp.index"
                (proc clangIndex $ workerargs i)
                  {std_out = stream,
                   std_err = stream,
                   cwd = if cdUp then Just (takeDirectory currentDir) else Nothing})
            cleanupProcess
            $ \(_, _, _, ph) -> do
              ex <- waitForProcess ph
              case ex of
                ExitSuccess -> return ()
                ExitFailure i -> error $ unwords (clangIndex:workerargs i)
                  ++ " returned exit code " ++ show i

          -- return data file names
          return $ map dataFile [0 .. workers-1]

    isPathPrefixOf :: FilePath -> FilePath -> Bool
    isPathPrefixOf prefix path = prefix == take (length prefix) path


    writeToDB backend repo = mapM_ $ \dataFile -> do
      dat <- BS.readFile dataFile
      case deserializeGen (Proxy :: Proxy Compact) dat of
        Left parseError -> error parseError
        Right batch     -> sendBatchAndWait backend repo batch

    derive verbose deriveBin backend repo =
      withExe "clang-derive" deriveBin $ \clangDerive -> do
        let go service = spawnAndConcurrentLog verbose clangDerive
              [ "--repo", showRepo repo
              , "--service", service
              ]
        case backendKind backend of
          BackendEnv env -> do
            fb303 <- newFb303 "gleandriver"
            let state = GleanHandler.State fb303 env
            withBackgroundFacebookService
              (GleanHandler.fb303State state)
              (GleanHandler.handler state)
              ThriftServer.defaultOptions
              $ \server ->
                go ("localhost:" <> show (ThriftServer.serverPort server))
          BackendThrift thrift -> do
            let clientConfig = thriftBackendClientConfig thrift
            go $ serviceToString (clientConfig_serv clientConfig)

withProgress ::  Worklist.Worklist -> Int -> Int -> IO a -> IO a
withProgress worklist jobs total action = do
  terminal <- ANSI.hSupportsANSI stdout
  flush $ start terminal
  x <- withAsync (showProgress terminal 0) $ const action
  flush $ finish terminal
  return x
  where
    flush f = f >> hFlush stdout

    tmsg n =
      unwords ["Indexed", show (total - n), "of", show total, "source files"]

    start True = putStrLn $ tmsg total
    start False = putStr $ unwords ["Indexing", show total, "source files: 0%"]

    finish True = do
      ANSI.cursorUpLine 1
      putStrLn $ tmsg 0
    finish False =
      putStrLn $ " ... 100%\nIndexed " ++ show total ++ "source files"

    message True before now = when (before /= now) $ do
      ANSI.cursorUpLine 1
      putStrLn $ tmsg now
    message False before now = do
      let tenth n = ((total - n) * 10) `div` total
          t = tenth now
      when (t /= 0 && t /= 10 && t /= tenth before) $
        putStr $ " ... " <> show (tenth now) <> "0%"

    showProgress terminal before = do
      ranges <- mapM (Worklist.get worklist) [0 .. jobs-1]
      let remaining = max 0 $ sum $ map (\(Worklist.Range i k) -> k-i) ranges
      flush $ message terminal before remaining
      threadDelay 1000000
      showProgress terminal remaining

withLog :: Bool -> (String -> IO ()) -> (StdStream -> IO a) -> IO a
withLog verbose log act
  | verbose = act Inherit
  | otherwise = bracket createPipe (\(r,w) -> hClose r >> hClose w)
      $ \(outRead, outWrite) ->
          withAsync (log =<< hGetContents outRead)
          $ const $ act $ UseHandle outWrite

-- | Simple concurrent logger. Spawn the process and asynchronously log
-- concise or full contents to stdout. Should use a fancy progress bar really
spawnAndConcurrentLog :: Bool -> FilePath -> [String] -> IO ()
spawnAndConcurrentLog verbose exe args = withLog verbose log $ \stream -> do
  (_, _, _, ph) <- createProcess (proc exe args)
    { std_out = stream, std_err = stream }
  ex <- waitForProcess ph
  case ex of
    ExitSuccess -> return ()
    ExitFailure i -> error $
      unwords (exe:args) ++ " returned exit code " ++ show i
  where
    log s = mapM_ (const $ putChar '.' >> hFlush stdout) (lines s)
      `finally` do
        putStr "\n"
        hFlush stdout

--
-- We need to find clang-index and clang-derive in $PATH or in-tree
--
withExe :: FilePath -> Maybe FilePath -> (FilePath -> IO a) -> IO a
withExe _ (Just exePath) f = do
  exeExists <- doesFileExist exePath
  if exeExists
    then f exePath
    else error $ exePath ++ " does not exist"
withExe exeName Nothing  f = do
  -- check $PATH
  mPath <- findExecutable exeName
  case mPath of
    Just exe -> f exe
    Nothing -> do -- well maybe we are in-tree, check local build
      wrapperExePath <- getExecutablePath
      case inTreeSearchPath wrapperExePath of
        Just path -> do
          mPath <- findExecutableRecursive exeName path
          case mPath of
            [] -> error $ "Could not find " <> exeName <>
                     " in $PATH or in " <> path
            exe:_ -> f exe
        Nothing -> error $ "Could not find " <> exeName <> " in $PATH"

-- determine if we are invoking glean in-tree, to find the clang-* binaries
inTreeSearchPath :: FilePath -> Maybe FilePath
inTreeSearchPath exePath = do
  case reverse (splitDirectories  exePath) of
    -- definitely running in tree:
    (_:_:"build":_:ty:_:xs)
      | ty `elem` ["x", "t"] -> Just $ joinPath (reverse xs)
    _ -> Nothing

-- do a silly recursive search in the dist-newstyle under the ghc dirs
-- > findExecutableRecursive "clang-index" ..path
--
findExecutableRecursive :: String -> FilePath -> IO [FilePath]
findExecutableRecursive exeName dirPath = do
  mFound <- findExecutablesInDirectories [dirPath] exeName
  case mFound of
    exe:_ -> return [exe]
    [] -> do
      dirs <- listDirectory dirPath
      let subDirs = map (dirPath </>) dirs
      subdirs <- filterM doesDirectoryExist subDirs
      concat <$> mapM (findExecutableRecursive exeName) subdirs

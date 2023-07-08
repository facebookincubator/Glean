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

import Facebook.Fb303
import Facebook.Service

#ifdef FBTHRIFT
import qualified Thrift.Server.CppServer as ThriftServer
#else
import qualified Thrift.Server.HTTP as ThriftServer
#endif

import Glean (sendBatch, clientConfig_serv, showRepo)
import Glean.Remote (thriftBackendClientConfig)
import Glean.Indexer
import Glean.LocalOrRemote ( BackendKind(..),
  LocalOrRemote(..), serializeInventory )
import Glean.Util.Service
import qualified Glean.Interprocess.Worklist as Worklist

import qualified Data.ByteString as BS
import qualified Glean.Handler as GleanHandler

data Clang = Clang
  { clangIndexBin     :: Maybe FilePath -- ^ path to @clang-index@ binary
  , clangDeriveBin    :: Maybe FilePath -- ^ path to @clang-derive@ binary
  , clangCompileDBDir :: Maybe FilePath
      -- ^ (optional) path to pre-existing @compile_commands.json@
  , clangJobs         :: Int -- ^ number of indexers to run concurrently
  , clangVerbose      :: Bool -- ^ display debugging information
  , clangProgress     :: Bool -- ^ display indexing progress
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
    long "cdb" <>
    help "path to a directory containing an existing compile_commands.json file"
  clangJobs <- option auto $
    short 'j' <>
    long "jobs" <>
    value 1 <>
    help "run N indexers in parallel"
  clangVerbose <- switch $
    short 'v' <>
    long "verbose" <>
    help "Enable verbose logging from subprocesses"
  clangProgress <- switch $
    long "progress" <>
    help "Display indexing progress even in verbose mode"
  return Clang{..}

-- | Standard indexer, that also runs the deriver
indexer :: Indexer Clang
indexer = indexerWith True

-- | Indexing only, no deriving
indexerNoDeriv :: Indexer Clang
indexerNoDeriv = indexerWith False

-- | C++ indexer. The 'Bool' specifies whether the indexer
--   also runs the deriver.
indexerWith :: Bool -> Indexer Clang
indexerWith deriveToo = Indexer {
  indexerShortName = "cpp-cmake",
  indexerDescription = "Index C++ code with CMake (via Clang)",
  indexerOptParser = options,
  indexerRun = \clang@Clang{..} backend repo IndexerParams{..} -> do
    -- indexing
    let tmpDir        = indexerOutput
        inventoryFile = tmpDir </> "inventory.data"
    generateInventory backend repo inventoryFile
    compileDBDir <-
      case clangCompileDBDir of
        Nothing  -> cmake clangVerbose indexerRoot tmpDir >> return tmpDir
        Just dir -> return dir
    indexerData <-
      index clang inventoryFile indexerRoot compileDBDir indexerOutput
    writeToDB backend repo indexerData

    -- deriving
    when deriveToo $
      derive clangVerbose clangDeriveBin backend repo
  }

  where
    generateInventory backend repo outFile =
      serializeInventory backend repo >>= BS.writeFile outFile

    cmake verbose srcDir tmpDir = withExe "cmake" Nothing $ \cmakeBin ->
      spawnAndConcurrentLog verbose cmakeBin
        [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
        , "-S", srcDir
        , "-B", tmpDir
        ]

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
      sources <- do
        let pargs = args ++ ["--print_sources_count"]
        s <- readProcess clangIndex pargs ""
        case reads s of
          [(sources,"")] -> return sources
          _ -> error $ unwords (clangIndex:pargs)
            ++ " produced unexpect output \"" ++ s ++ "\""

      case sources of
        0 -> do
          -- TODO: should this be an error?
          putStrLn "No source files to index"
          return []
        _ ->
          -- set up worklist
          let ranges =
                map (\(i,n) -> Worklist.Range i (i+n)) $ chunk clangJobs sources
              !workers = length ranges
          in
          Worklist.withTemp ranges $ \wfile worklist ->

          -- progress and logging
          (if clangProgress || not clangVerbose
            then withProgress worklist clangJobs sources
            else id) $
          withLog clangVerbose (void . evaluate . length) $ \stream -> do

          -- run workers
          let dataFile i = tmpDir </> "indexer-" <> show i <> ".data"
              workerargs i = args ++
                [ "-dump", dataFile i
                , "--work_file", wfile
                , "--worker_index", show i
                , "--worker_count", show workers
                ]
          forConcurrently_ [0 .. workers-1] $ \i -> bracket
            -- createProcess_ because we don't want the stdout/stderr handles
            -- to be closed
            (createProcess_
              "Cpp.index"
              (proc clangIndex $ workerargs i)
                {std_out = stream, std_err = stream})
            cleanupProcess
            $ \(_, _, _, ph) -> do
              ex <- waitForProcess ph
              case ex of
                ExitSuccess -> return ()
                ExitFailure i -> error $ unwords (clangIndex:workerargs i)
                  ++ " returned exit code " ++ show i

          -- return data file names
          return $ map dataFile [0 .. workers-1]

    writeToDB backend repo = mapM_ $ \dataFile -> do
      dat <- BS.readFile dataFile
      case deserializeGen (Proxy :: Proxy Compact) dat of
        Left parseError -> error parseError
        Right batch     -> sendBatch backend repo batch

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

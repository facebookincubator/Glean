{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.CMake (CppIndexerOpts (..), indexCMake) where

import Control.Monad.Except
import System.Directory
import System.Exit
import System.FilePath
import System.Environment
import System.IO.Temp
import System.Process
import Util.Log
import Control.Concurrent.Async
import System.IO

data CppIndexerOpts = CppIndexerOpts
  { -- | directory with the root CMakeLists.txt
    cfgCppSrcDir :: FilePath
  , -- | where to dump indexed data (thrift compact format)
    cfgDumpFile :: FilePath
  , -- | path to inventory file for the glean schema to be used
    cfgInventory :: FilePath
  , -- | cmake target to focus on ('Nothing' corresponds to 'all')
    cfgCMakeTarget :: String
  , -- | extra clang arguments
    cfgClangArgs :: String
    -- | verbose output
  , cfgVerbose :: Bool
  }

data IndexError
  = -- | cmake exit code, stdout, stderr
    CMakeError Int String String
  | -- | @compile_commands.json@ missing
    CMakeCommandsFileMissing FilePath
  | -- | indexer exit code, stdout, stderr
    IndexerError Int String String
  | -- | path to glean-clang-index not found
    ClangIndexNotFound String
  deriving (Show)

type IndexM = ExceptT IndexError IO

-- | Generate @compile_commands.json@ file for a CMake project
-- Takes path to (temporary) cmake build dir and path to @compile_commands.json@
-- file, when successful
generateBuildCommands :: CppIndexerOpts -> FilePath -> IndexM FilePath
generateBuildCommands opts@CppIndexerOpts{..} buildDir = do
  ex <- liftIO $ spawnAndConcurrentLog opts "cmake" args
  case ex of
    ExitSuccess -> return (buildDir </> "compile_commands.json")
    ExitFailure i -> throwError $ CMakeError i [] []
  where
    args = words cfgClangArgs++
      [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
      , "-S"
      , cfgCppSrcDir
      , "-B"
      , buildDir
      ]

-- | buildDir is path to (temporary) cmake build dir
runIndexer :: CppIndexerOpts -> FilePath -> IndexM ()
runIndexer opts@CppIndexerOpts{..} buildDir = withClangIndex $ \exe -> do
  liftIO $ logInfo $ "Indexing with: " ++ unwords (exe : args)
  ex <- liftIO $ spawnAndConcurrentLog opts exe args
  case ex of
    ExitSuccess -> return ()
    ExitFailure i -> throwError $ IndexerError i [] []
  where
    args =
      [ "-cdb_dir"
      , buildDir
      , "-cdb_target"
      , cfgCMakeTarget
      , "-root"
      , cfgCppSrcDir
      , "-dump"
      , cfgDumpFile
      , "--inventory"
      , cfgInventory
      , "-logtostderr"
      ]

-- | Simple concurrent logger. Spawn the process and asynchronously log
-- concise or full contents to stdout. Should use a fancy progress bar really
spawnAndConcurrentLog :: CppIndexerOpts -> FilePath -> [String] -> IO ExitCode
spawnAndConcurrentLog CppIndexerOpts{..} exe args = do
  (_, Just hout, Just herr, ph) <- createProcess (proc exe args)
      { std_out = CreatePipe, std_err = CreatePipe }
  withAsync (log hout) $ \asyncOut ->
    withAsync (log herr) $ \asyncErr -> do
      status <- waitForProcess ph
      cancel asyncOut
      cancel asyncErr
      putStr "\n" >> hFlush stdout
      return status
  where
    log h = mapM_ draw . lines =<< hGetContents h
    draw s
      | cfgVerbose = putStrLn s
      | otherwise = putChar '.' >> hFlush stdout

withClangIndex :: (FilePath -> IndexM ()) -> IndexM ()
withClangIndex f = do
  -- check $PATH
  mPath <- liftIO $ findExecutable clangIndexExe
  case mPath of
    Just exe -> f exe
    Nothing -> do -- well maybe we are in-tree, check local build
      wrapperExePath <- liftIO $getExecutablePath
      let searchPath = takeDirectory (takeDirectory wrapperExePath) </>
            clangIndexExe
      mPath <- liftIO $ findExecutablesInDirectories [searchPath] clangIndexExe
      case mPath of
        [] -> throwError $ ClangIndexNotFound $
                "Could not find " <> clangIndexExe <>
                   " in $PATH or " <> searchPath
        exe:_ -> f exe

-- Canonical name for the clang-linked indexer executable
clangIndexExe :: String
clangIndexExe = "glean-clang-index"

indexCMake :: CppIndexerOpts -> IndexM ()
indexCMake indexOpts = withSystemTempDirectory "glean-cmake" $ \tmpDir -> do
  compileCommandsPath <- generateBuildCommands indexOpts tmpDir
  logInfo $ "Using cmake build commands from " <> compileCommandsPath
  exists <- liftIO (doesFileExist compileCommandsPath)
  when (not exists) $
    throwError (CMakeCommandsFileMissing compileCommandsPath)
  runIndexer indexOpts tmpDir

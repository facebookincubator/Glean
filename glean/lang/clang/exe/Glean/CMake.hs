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
generateBuildCommands ::
  CppIndexerOpts ->
  -- | path to (temporary) cmake build dir
  FilePath ->
  -- | path to @compile_commands.jsonà file, when successful
  IndexM FilePath
generateBuildCommands indexOpts buildDir = do
  (ex, out, err) <- liftIO (readProcessWithExitCode "cmake" args "")
  case ex of
    ExitSuccess -> return (buildDir </> "compile_commands.json")
    ExitFailure i -> throwError $ CMakeError i out err
  where
    args =
      words (cfgClangArgs indexOpts)
        ++ [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
           , "-S"
           , cfgCppSrcDir indexOpts
           , "-B"
           , buildDir
           ]

-- | buildDir is path to (temporary) cmake build dir
runIndexer :: CppIndexerOpts -> FilePath -> IndexM ()
runIndexer indexOpts buildDir = withClangIndex $ \exe -> do
  liftIO $ putStrLn $ "Indexing with: " ++ unwords (exe : args)
  (ex, out, err) <- liftIO $ readProcessWithExitCode exe args ""
  -- Uncomment to see C++ indexer output. TODO: hide behind a --verbose flag?
  -- liftIO $ do
  --   print ex
  --   putStrLn "---"
  --   putStrLn out
  --   putStrLn "---"
  --   putStrLn err
  --   putStrLn "---"
  case ex of
    ExitSuccess -> return ()
    ExitFailure i -> throwError $ IndexerError i out err
  where
    args =
      [ "-cdb_dir"
      , buildDir
      , "-cdb_target"
      , cfgCMakeTarget indexOpts
      , "-root"
      , cfgCppSrcDir indexOpts
      , "-dump"
      , cfgDumpFile indexOpts
      , "--inventory"
      , cfgInventory indexOpts
      ]

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
indexCMake indexOpts =
  withSystemTempDirectory "glean-cmake" $ \tmpDir -> do
    compileCommandsPath <- generateBuildCommands indexOpts tmpDir
    -- Uncomment the 5 lines below to see what happens if we additionally try to
    -- build the project.
    -- (ex, out, err) <- liftIO $
    --   readProcessWithExitCode "cmake" ["--build", tmpDir] ""
    -- liftIO $ do
    --   putStrLn ("stdout:\n" ++ out ++ "\n---")
    --   putStrLn ("stderr:\n" ++ err ++ "\n---")

    exists <- liftIO (doesFileExist compileCommandsPath)
    when (not exists) $
      throwError (CMakeCommandsFileMissing compileCommandsPath)
    runIndexer indexOpts tmpDir

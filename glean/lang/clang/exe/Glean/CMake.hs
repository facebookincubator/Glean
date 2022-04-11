module Glean.CMake where

import Control.Monad.Except
import Control.Monad.IO.Class
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

data CppIndexerOpts = CppIndexerOpts
  { cfgCppSrcDir   :: FilePath     -- ^ directory with the root CMakeLists.txt
  , cfgDumpFile    :: FilePath     -- ^ where to dump indexed data (thrift compact format)
  , cfgInventory   :: FilePath     -- ^ path to inventory file for the glean schema to be used
  , cfgCMakeTarget :: String       -- ^ cmake target to focus on ('Nothing' corresponds to 'all')
  , cfgClangArgs   :: String       -- ^ extra clang arguments
  }


data IndexError
  = CMakeError Int String String -- ^ cmake exit code, stdout, stderr
  | CMakeCommandsFileMissing FilePath -- ^ @compile_commands.json@ missing
  | IndexerError Int String String -- ^ indexer exit code, stdout, stderr
  deriving Show

type IndexM = ExceptT IndexError IO

-- | Generate @compile_commands.json@ file for a CMake project
generateBuildCommands
  :: CppIndexerOpts
  -> FilePath        -- ^ path to (temporary) cmake build dir
  -> IndexM FilePath -- ^ path to @compile_commands.jsonà file, when successful
generateBuildCommands indexOpts buildDir = do
  (ex, out, err) <- liftIO (readProcessWithExitCode "cmake" args "")
  case ex of
    ExitSuccess   -> return (buildDir </> "compile_commands.json")
    ExitFailure i -> throwError $ CMakeError i out err

  where args = words (cfgClangArgs indexOpts)
            ++ [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
               , "-S", cfgCppSrcDir indexOpts
               , "-B", buildDir
               ]

runIndexer
  :: CppIndexerOpts
  -> FilePath     -- ^ path to (temporary) cmake build dir
  -> IndexM ()
runIndexer indexOpts buildDir = do
  liftIO . putStrLn $
    "calling:\t " ++ unwords ("glean-clang-index":args)
  (ex, out, err) <- liftIO $ readProcessWithExitCode "glean-clang-index" args ""
  -- Uncomment to see C++ indexer output. TODO: hide behind a --verbose flag?
  -- liftIO $ do
  --   print ex
  --   putStrLn "---"
  --   putStrLn out
  --   putStrLn "---"
  --   putStrLn err
  --   putStrLn "---"
  case ex of
    ExitSuccess   -> return ()
    ExitFailure i -> throwError $ IndexerError i out err

  where args = [ "-cdb_dir", buildDir
               , "-cdb_target", cfgCMakeTarget indexOpts
               , "-root", cfgCppSrcDir indexOpts
               , "-dump", cfgDumpFile indexOpts
               , "--inventory", cfgInventory indexOpts
               ]

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

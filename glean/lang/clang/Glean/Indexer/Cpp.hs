{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Cpp ( indexer ) where

import Control.Concurrent.Async
import Data.Proxy
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Thrift.Protocol (deserializeGen)
import Thrift.Protocol.Compact (Compact)
import Util.Log

import Facebook.Fb303
import Facebook.Service
import Glean (sendBatch, clientConfig_serv, showRepo)
import Glean.Backend (thriftBackendClientConfig)
import Glean.Indexer
import Glean.LocalOrRemote ( BackendKind(..), LocalOrRemote(..), serializeInventory )
import Glean.Util.Service

import qualified Data.ByteString as BS
import qualified Glean.Handler as GleanHandler
import qualified Thrift.Server.CppServer as CppServer

data Clang = Clang
  { clangIndexBin     :: Maybe FilePath -- ^ path to @clang-index@ binary
  , clangDeriveBin    :: Maybe FilePath -- ^ path to @clang-derive@ binary
  , clangCompileDBDir :: Maybe FilePath -- ^ (optional) path to pre-existing @compile_commands.json@
  , clangVerbose      :: Bool -- ^ display debugging information
  }

options :: Parser Clang
options = do
  clangIndexBin <- optional $ strOption $
    long "indexer" <>
    help "path to the glean-clang-index binary"
  clangDeriveBin <- optional $ strOption $
    long "deriver" <>
    help "path to the clang-derive binary"
  clangCompileDBDir <- optional $ strOption $
    long "cdb" <>
    help "(Optional) path to a directory containing an existing compile_commands.json file"
  clangVerbose <- switch $
    short 'v' <>
    long "verbose" <>
    help "Enable verbose logging from subprocesses"
  return Clang{..}

indexer :: Indexer Clang
indexer = Indexer {
  indexerShortName = "cpp",
  indexerDescription = "Index C++ code (through Clang)",
  indexerOptParser = options,
  indexerRun = \Clang{..} backend repo IndexerParams{..} -> do
    -- indexing
    let tmpDir        = indexerOutput
        inventoryFile = tmpDir </> "inventory.data"
        indexerData   = tmpDir </> "indexer.data"
    generateInventory backend repo inventoryFile
    compileDBDir <-
      case clangCompileDBDir of
        Nothing  -> cmake clangVerbose indexerRoot tmpDir >> return tmpDir
        Just dir -> return dir
    index clangVerbose clangIndexBin inventoryFile indexerRoot compileDBDir indexerData
    writeToDB backend repo indexerData

    -- deriving
    derive clangVerbose clangDeriveBin backend repo
  }

  where generateInventory backend repo outFile =
          serializeInventory backend repo >>= BS.writeFile outFile

        cmake verbose srcDir tmpDir = withExe "cmake" Nothing $ \cmakeBin ->
          spawnAndConcurrentLog verbose cmakeBin
            [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
            , "-S", srcDir
            , "-B", tmpDir
            ]

        index verbose indexBin inventory srcDir buildDir outFile =
          withExe "clang-index" indexBin $ \clangIndex -> do
            let args = [ "-cdb_dir", buildDir
                       , "-cdb_target", "all"
                       , "-root", srcDir
                       , "-dump", outFile
                       , "--inventory", inventory
                       , "-logtostderr"
                       ]
            logInfo $ "Indexing with: " ++ unwords (clangIndex : args)
            spawnAndConcurrentLog verbose clangIndex args

        writeToDB backend repo dataFile = do
          dat <- BS.readFile dataFile
          case deserializeGen (Proxy :: Proxy Compact) dat of
            Left parseError -> error parseError
            Right batch     -> sendBatch backend repo batch

        derive verbose deriveBin backend repo =
          withExe "clang-derive" deriveBin $ \clangDerive -> do
            let go service = spawnAndConcurrentLog verbose clangDerive $
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
                  CppServer.defaultOptions
                  $ \server -> go ("localhost:" <> show (CppServer.serverPort server))
              BackendThrift thrift -> do
                let clientConfig = thriftBackendClientConfig thrift
                go $ serviceToString (clientConfig_serv clientConfig)

-- | Simple concurrent logger. Spawn the process and asynchronously log
-- concise or full contents to stdout. Should use a fancy progress bar really
spawnAndConcurrentLog :: Bool -> FilePath -> [String] -> IO ()
spawnAndConcurrentLog verbose exe args = do
  (_, Just hout, Just herr, ph) <- createProcess (proc exe args)
      { std_out = CreatePipe, std_err = CreatePipe }
  ex <- withAsync (log hout) $ \asyncOut ->
    withAsync (log herr) $ \asyncErr -> do
      status <- waitForProcess ph
      cancel asyncOut
      cancel asyncErr
      putStr "\n" >> hFlush stdout
      return status
  case ex of
    ExitSuccess -> return ()
    ExitFailure i -> error $
      unwords (exe:args) ++ " returned exit code " ++ show i
  where
    log h = mapM_ draw . lines =<< hGetContents h
    draw s
      | verbose = putStrLn s
      | otherwise = putChar '.' >> hFlush stdout

withExe :: FilePath -> Maybe FilePath -> (FilePath -> IO ()) -> IO ()
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
      let searchPath = takeDirectory (takeDirectory wrapperExePath) </>
            exeName
      mPath <- findExecutablesInDirectories [searchPath] exeName
      case mPath of
        [] -> error $
                "Could not find " <> exeName <>
                   " in $PATH or " <> searchPath
        exe:_ -> f exe

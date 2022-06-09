{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

-- | An 'Indexer' that uses an external process, as opposed to a
-- library. The indexer might involve running an executable to
-- generate JSON files and then writing them to the repo using the
-- 'glean' CLI tool, or it might involve running a server.
--
module Glean.Indexer.External
  ( externalIndexer
  , Ext(..)
  , Flavour(..)
  , sendJsonBatches
  , withExecutable
  ) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Char(isAlphaNum)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Extra as L
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as O
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import System.Environment

import Control.Concurrent.Stream (stream)
import Facebook.Fb303
import Facebook.Service
import qualified Thrift.Server.CppServer as CppServer
import qualified Thrift.Server.Types as Thrift.Server

import qualified Glean
import qualified Glean.LocalOrRemote as LocalOrRemote
import Glean.Backend (BackendKind(..), LocalOrRemote(..), ThriftBackend(..))
import Glean.Derive
import qualified Glean.Handler as GleanHandler
import Glean.Indexer
import Glean.Write
import Glean.Util.Service

externalIndexer :: Indexer Ext
externalIndexer = Indexer
  { indexerShortName = "external"
  , indexerDescription = "A generic indexer that runs an external binary"
  , indexerOptParser = extOptions
  , indexerRun = execExternal
  }

data Flavour = Json | Server

data Ext = Ext
  { extRunScript :: FilePath
  , extArgs :: [String]
  , extFlavour :: Flavour
  , extDerivePredicates :: [Text]
  }

extOptions :: O.Parser Ext
extOptions = do
  extRunScript <- O.strOption $
    O.long "binary" <> O.metavar "PATH" <>
    O.help "script to run to produce facts, arguments supplied by --arg/--args"
  extArgs <- fmap concat $ O.many $
    fmap (:[]) (O.strOption $ O.long "arg" <> O.metavar "ARG")
    O.<|>
    fmap words (O.strOption $ O.long "args" <> O.metavar "ARGS")
  extFlavour <-
    O.flag' Json (
      O.long "json" <>
      O.help "the binary should put JSON files in ${JSON_BATCH_DIR}") O.<|>
    O.flag' Server (
      O.long "server" <>
      O.help ("the binary should connect to a Glean server at " <>
        "${GLEAN_SERVER} to write facts"))
  extDerivePredicates <-
    fmap (maybe [] (Text.splitOn "," . Text.pack)) $
    O.optional $
    O.strOption $
    O.long "derive" <> O.metavar "PREDICATE,PREDICATE,..." <>
    O.help "predicates to derive after writing facts (ordered)"
  return Ext{..}

-- | Finish decoding parameters from @glean_test@ by
-- performing substitutions on @TEST_*@ variables, and return the
-- 'RunIndexer'.
execExternal :: Ext -> RunIndexer
execExternal Ext{..} env repo IndexerParams{..} = do index; derive
  where
  derive = forM_ extDerivePredicates $ \pred ->
    derivePredicate env repo Nothing Nothing
      (parseRef pred) Nothing

  repoName = Text.unpack (Glean.repo_name repo)
  repoHash = Text.unpack (Glean.repo_hash repo)

  vars = HashMap.fromList
    [ ("TEST_REPO", Glean.showRepo repo)
    , ("TEST_REPO_NAME", repoName)
    , ("TEST_REPO_HASH", repoHash)
    , ("TEST_ROOT", indexerRoot)
    ]

  -- We could make this configurable, but there's not a lot to be
  -- gained and it would have to be plumbed through all the separate
  -- language indexers that build on External.
  maxConcurrency = 20 :: Int

  index = case extFlavour of
    Json -> do
      jsonBatchDir <- createTempDirectory indexerOutput "glean-json"
      let jsonVars = HashMap.insert "JSON_BATCH_DIR" jsonBatchDir vars
      withExecutable extRunScript Nothing $ \exeRunBin -> do
        callCommand
          (unwords (exeRunBin : map (quoteArg . subst jsonVars) extArgs))
        files <- listDirectory jsonBatchDir
        stream maxConcurrency (forM_ files) $ \file -> do
          batches <- fileToBatches (jsonBatchDir </> file)
          void $ LocalOrRemote.sendJsonBatch env repo batches Nothing

    Server -> do
      let
        go service = do
          let serverVars = HashMap.insert "GLEAN_SERVER" service vars
          withExecutable extRunScript Nothing $ \exeRunBin -> callCommand
            (unwords (exeRunBin : map (quoteArg . subst serverVars) extArgs))
      case backendKind env of
        BackendEnv env -> do
          fb303 <- newFb303 "gleandriver"
          let state = GleanHandler.State fb303 env
          withBackgroundFacebookService
            (GleanHandler.fb303State state)
            (GleanHandler.handler state)
            Thrift.Server.defaultOptions
            $ \server -> go ("localhost:" <> show (CppServer.serverPort server))
        BackendThrift thrift -> do
          let clientConfig = thriftBackendClientConfig thrift
          go (serviceToString (Glean.clientConfig_serv clientConfig))

  subst vars ('$':'{':s)
    | (var,'}':rest) <- break (=='}') s
    , all (\c -> isAlphaNum c || c == '_') var =
        HashMap.lookupDefault ("${" <> var <> "}") var vars ++ subst vars rest
  subst vars (c:s) = c : subst vars s
  subst _ "" = ""

  -- Quotes a value to allow it to be safely exposed to the shell
  -- The method used is to replace ' with '"'"' and wrap the value inside
  -- single quotes. This works for POSIX shells.
  quoteArg t =  q <> L.intercalate "'\"'\"'" (L.splitOn q t) <> q
    where
      q = "'"

sendJsonBatches
  :: LocalOrRemote.LocalOrRemote b
  => b
  -> Glean.Repo
  -> String
  -> Aeson.Value
  -> IO ()
sendJsonBatches backend repo msg val = do
  batches <- case Aeson.parse parseJsonFactBatches val of
    Aeson.Error s -> throwIO $ ErrorCall $ msg <> ": " <> s
    Aeson.Success x -> return x
  void $ LocalOrRemote.sendJsonBatch backend repo batches Nothing

--
-- We need to find clang-index,clang-derive, hiedb-indexer etc in $PATH or
-- in-tree.
--
withExecutable :: String -> Maybe FilePath -> (FilePath -> IO ()) -> IO ()
withExecutable _exeName (Just exePath) f = do
  exeExists <- doesFileExist exePath
  if exeExists
    then f exePath
    else error $ exePath ++ " does not exist"
withExecutable exeName Nothing f = do
  mPath <- findExecutable exeName -- check $PATH
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
    ("glean":"glean":"build":"glean":"x":_:xs) -> Just $ joinPath (reverse xs)
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

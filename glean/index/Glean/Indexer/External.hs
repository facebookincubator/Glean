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
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import Data.Char(isAlphaNum)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as O
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process

import Facebook.Fb303
import Facebook.Service
import Foreign.CPP.Dynamic (parseJSON)
import qualified Thrift.Server.CppServer as CppServer
import qualified Thrift.Server.Types as Thrift.Server

import qualified Glean
import Glean.Backend (BackendKind(..), LocalOrRemote(..), ThriftBackend(..))
import Glean.Derive
import qualified Glean.Handler as GleanHandler
import Glean.Indexer
import Glean.Write
import qualified Glean.LSIF.Driver as LSIF
import Glean.Util.Service

externalIndexer :: Indexer Ext
externalIndexer = Indexer
  { indexerOptParser = extOptions
  , indexerRun = execExternal
  }

data Flavour = Json | Server | Lsif

data Ext = Ext
  { extBinary :: FilePath
  , extArgs :: [String]
  , extFlavour :: Flavour
  , extDerivePredicates :: [Text]
  }

extOptions :: O.Parser Ext
extOptions = do
  extBinary <- O.strOption $
    O.long "binary" <> O.metavar "PATH" <>
    O.help "binary to run to produce facts, arguments supplied by --arg/--args"
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
        "${GLEAN_SERVER} to write facts")) O.<|>
    O.flag' Lsif (
      O.long "lsif" <>
      O.help "run glean-lsif on the language specified by --binary")
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

  index = case extFlavour of
    Json -> do
      withSystemTempDirectory "glean-json" $ \jsonBatchDir -> do
      let jsonVars = HashMap.insert "JSON_BATCH_DIR" jsonBatchDir vars
      callProcess extBinary (map (subst jsonVars) extArgs)
      files <- listDirectory jsonBatchDir
      forM_ files $ \file -> do
        str <- B.readFile (jsonBatchDir </> file)
        r <- Foreign.CPP.Dynamic.parseJSON str
        val <- either (throwIO  . ErrorCall . ((file ++ ": ") ++) .
          Text.unpack) return r
        batches <- case Aeson.parse parseJsonFactBatches val of
          Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
          Aeson.Success x -> return x
        void $ Glean.sendJsonBatch env repo batches Nothing

    Lsif -> case LSIF.indexerLang extBinary of
      Nothing -> fail ("Unrecognized LSIF language indexer: " <> extBinary)
      Just lang -> do
        val <- LSIF.runIndexer lang indexerRoot
        batches <- case Aeson.parse parseJsonFactBatches val of
          Aeson.Error s -> throwIO $ ErrorCall $ extBinary <> "/lsif: " ++ s
          Aeson.Success x -> return x
        void $ Glean.sendJsonBatch env repo batches Nothing

    Server -> do
      let
        go service = do
          let serverVars = HashMap.insert "GLEAN_SERVER" service vars
          callProcess extBinary (map (subst serverVars) extArgs)
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

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
module Glean.Regression.Indexer.External
  ( externalIndexer
  , Ext(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import Data.Char(isAlphaNum)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (wordsBy)
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
import Glean.Database.Types (envRoot)
import Glean.Derive
import qualified Glean.Handler as GleanHandler
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Types
import Glean.Write

externalIndexer :: Indexer Ext
externalIndexer = Indexer
  { indexerOptParser = extOptions
  , indexerRun = execExternal
  }

data Flavour = Json | Server

data Ext = Ext
  { extBinary :: FilePath
  , extArgs :: [String]
  , extGroups :: [String]
  , extSchemaVersion :: Maybe Int
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
  extGroups <- fmap list $ O.strOption $
    O.long "groups" <> O.metavar "NAME,NAME,..." <> O.value "" <>
    O.help "all tests are run for each group"
  extSchemaVersion <- O.optional $ O.option O.auto $
    O.long "schema-version" <> O.metavar "INT" <>
    O.help "version of 'all' schema to use for unversioned queries"
  extFlavour <-
    O.flag' Json (
      O.long "json" <>
      O.help "the binary should put JSON files in ${JSON_BATCH_DIR}") O.<|>
    O.flag' Server (
      O.long "server" <>
      O.help ("the binary should connect to a Glean server on " <>
        "port ${SERVER_PORT} to write facts"))
  extDerivePredicates <-
    fmap (maybe [] (Text.splitOn "," . Text.pack)) $
    O.optional $
    O.strOption $
    O.long "derive" <> O.metavar "PREDICATE,PREDICATE,..." <>
    O.help "predicates to derive after writing facts (ordered)"
  return Ext{..}
  where
    list = wordsBy (==',')

-- | Finish decoding parameters from @glean_test@ by
-- performing substitutions on @TEST_*@ variables, and return the
-- 'RunIndexer'.
execExternal :: Ext -> RunIndexer
execExternal Ext{..} TestConfig{..} env = do index; derive
  where
  derive = forM_ extDerivePredicates $ \pred ->
    derivePredicate env testRepo Nothing Nothing
      (parseRef pred) Nothing

  repoName = Text.unpack (repo_name testRepo)
  repoHash = Text.unpack (repo_hash testRepo)

  vars = HashMap.fromList
    [ ("TEST_DB_ROOT", envRoot env)
    , ("TEST_REPO", repoName ++ '/' : repoHash)
    , ("TEST_REPO_NAME", repoName)
    , ("TEST_REPO_HASH", repoHash)
    , ("TEST_ROOT", testRoot)
    , ("TEST_PROJECT_ROOT", testProjectRoot)
    , ("TEST_OUTPUT", testOutput)
    , ("TEST_GROUP", testGroup)
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
        void $ Glean.sendJsonBatch env testRepo batches Nothing

    Server -> do
      fb303 <- newFb303 "gleandriver"
      let state = GleanHandler.State fb303 env
      withBackgroundFacebookService
        (GleanHandler.fb303State state)
        (GleanHandler.handler state)
        Thrift.Server.defaultOptions
        $ \server -> do
          let port = CppServer.serverPort server
              serverVars = HashMap.insert "SERVER_PORT" (show port) vars
          callProcess extBinary (map (subst serverVars) extArgs)

  subst vars ('$':'{':s)
    | (var,'}':rest) <- break (=='}') s
    , all (\c -> isAlphaNum c || c == '_') var =
        HashMap.lookupDefault ("${" <> var <> "}") var vars ++ subst vars rest
  subst vars (c:s) = c : subst vars s
  subst _ "" = ""

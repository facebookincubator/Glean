{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Shell.Index
  ( indexCmd
  , pickHash
  , load
  , create
  ) where

import Options.Applicative as OptParse
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.CPP.Dynamic
import System.Directory
import System.FilePath
import System.IO.Temp
import TextShow

import qualified Glean
import Glean.Shell.Types
import Glean.Util.Some
import Glean.Write

import Glean.Indexer
import Glean.Indexer.List

indexCmd :: String -> Eval ()
indexCmd str = do
  let parser = (,)
        <$> cmdLineParser
        <*> (strArgument (metavar "ROOT"))

      result = execParserPure defaultPrefs (info parser fullDesc)
        (words str {- TODO: quoting -})

  (runIndexer, root) <- case result of
    OptParse.Success a -> return a
    OptParse.Failure f -> throwM $ ErrorCall $ fst $ renderFailure f ":index"
    OptParse.CompletionInvoked{} -> throwM $ ErrorCall "CompletionInvoked"

  projectRoot <- liftIO $ getCurrentDirectory
  withSystemTempDirectory' "glean-shell" $ \tmp -> do
    let name = Text.pack (takeBaseName (dropTrailingPathSeparator root))
    hash <- pickHash name
    let repo = Glean.Repo name hash
    withBackend $ \backend -> do
      let exists = throwIO (ErrorCall (show repo <> ": already exists"))
      liftIO $ Glean.fillDatabase backend repo "" Nothing exists $
        runIndexer (Some backend) repo
          IndexerParams {
            indexerRoot = root,
            indexerProjectRoot = projectRoot,
            indexerOutput = tmp,
            indexerGroup = ""
          }
    setRepo repo

withSystemTempDirectory' :: String -> (FilePath -> Eval a) -> Eval a
withSystemTempDirectory' str action = Eval $ do
  a <- liftWith $ \run -> withSystemTempDirectory str (run . unEval . action)
  restoreT $ return a

pickHash :: Text -> Eval Text
pickHash name = withBackend $ \be -> do
  r <- liftIO $ Glean.listDatabases be def
  let
    hashes =
      [ repo_hash
      | Glean.Database{..} <- Glean.listDatabasesResult_databases r
      , let Glean.Repo{..} = database_repo
      , repo_name == name
      ]
  return $ head $ filter (`notElem` hashes) $ map showt [0::Int ..]

-- | Load a set of JSON files as a DB
load :: Glean.Repo -> [FilePath] -> Eval ()
load repo files = withBackend $ \be ->  liftIO $ do
  let onExisting  = throwIO $ ErrorCall "database already exists"
  void $ Glean.fillDatabase be repo "" Nothing onExisting $
    forM_ files $ \file -> do
      r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
      val <- either (throwIO  . ErrorCall . Text.unpack) return r
      (batches, schema_id) <- case Aeson.parse parseJsonFactBatches val of
        Error str -> throwIO $ ErrorCall str
        Aeson.Success x -> return x
      let opts = schemaIdToOpts schema_id
      Glean.sendJsonBatch be repo batches opts

create :: Glean.Repo -> Eval ()
create repo = withBackend $ \be ->  liftIO $ do
  exists <- Glean.create be repo "" Nothing
  when exists $ throwIO $ ErrorCall "database already exists"

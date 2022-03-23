{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Shell.Index
  ( indexCmd
  , indexerTable
  , load
  , pickHash
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.CPP.Dynamic (parseJSON)
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import TextShow
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Glean
import Glean.Shell.Types
import Glean.Write

import qualified Glean.LSIF.Driver as LSIF ( runIndexer, Language(..) )

-- | Run an indexer, returning either a list of JSON files or a JSON value
runIndexer :: Language -> String -> String -> IO (Either [FilePath] Value)
runIndexer lang dir outputDir = case lang of
  Flow -> Left <$> call "flow"
    [ "glean", dir , "--output-dir", outputDir , "--write-root", "." ]
  Hack -> Left <$> call "hh_server" (
    [ dir , "--write-symbol-info", outputDir ] <> hackConfig)
  LSIF lang -> Right <$> LSIF.runIndexer lang dir
  where
    call :: String -> [String] -> IO [FilePath]
    call procName args = do
      callProcess procName args
      map (outputDir </>) <$> listDirectory outputDir

    hackConfig = concatMap (\flag -> ["--config", flag])
        [ "symbol_write_include_hhi=false"
        , "symbolindex_search_provider=NoIndex"
        , "use_mini_state=true"
        , "lazy_decl=true"
        , "lazy_parse=true"
        , "lazy_init2=true"
        , "enable_enum_classes=true"
        , "enable_enum_supertyping=true"
        ]

data Language
   = Flow
   | Hack
   | LSIF LSIF.Language

-- ^ Indexers that can be invoked from glean shell
indexerTable :: Map.Map String Language
indexerTable = Map.fromList
  [ ("flow", Flow)
  , ("hack", Hack)
  , ("lsif/typescript", LSIF LSIF.TypeScript)
  , ("lsif/go", LSIF LSIF.Go)
  ]

-- Note, tab completions in Glean.Shell
indexCmd :: String -> Eval ()
indexCmd str = case words str of
  [indexer, dir] | Just lang <- Map.lookup indexer indexerTable
    -> index lang dir
  _ -> liftIO $ throwIO $ ErrorCall
    "syntax: :index flow|hack|lsif/typescript <dir>"
  where
    index lang dir =
      withSystemTempDirectory' "glean-shell" $ \tmp -> do
        fileValues <- liftIO $ runIndexer lang dir tmp
        let name = Text.pack (takeBaseName (dropTrailingPathSeparator dir))
        hash <- pickHash name
        let repo = Glean.Repo name hash
        case fileValues of
          Left files -> load repo files
          Right val -> loadValue repo val
        setRepo repo

withSystemTempDirectory' :: String -> (FilePath -> Eval a) -> Eval a
withSystemTempDirectory' str action = Eval $ do
  a <- liftWith $ \run -> withSystemTempDirectory str (run . unEval . action)
  restoreT $ return a

pickHash :: Text -> Eval Text
pickHash name = withBackend $ \be -> do
  r <- liftIO $ listDatabases be def
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
  void $ fillDatabase be Nothing repo "" onExisting $ forM_ files $ \file -> do
    r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
    val <- either (throwIO  . ErrorCall . Text.unpack) return r
    batches <- case Aeson.parse parseJsonFactBatches val of
      Error str -> throwIO $ ErrorCall str
      Aeson.Success x -> return x
    Glean.sendJsonBatch be repo batches Nothing

-- | Load a specific JSON value as a db
loadValue :: Glean.Repo -> Value -> Eval ()
loadValue repo val = withBackend $ \be ->  liftIO $ do
  let onExisting  = throwIO $ ErrorCall "database already exists"
  void $ fillDatabase be Nothing repo "" onExisting $ do
    batches <- case Aeson.parse parseJsonFactBatches val of
      Error str -> throwIO $ ErrorCall str
      Aeson.Success x -> return x
    void $ Glean.sendJsonBatch be repo batches Nothing

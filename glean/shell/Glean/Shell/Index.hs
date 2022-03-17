{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Shell.Index
  ( indexCmd
  , load
  , pickHash
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.CPP.Dynamic (parseJSON)
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import TextShow

import Glean
import Glean.Shell.Types
import Glean.Write

data Language = Flow | Hack

runIndexer :: Language -> String -> String -> IO ()
runIndexer lang dir outputDir = case lang of
  Flow -> callProcess "flow"
    [ "glean", dir , "--output-dir", outputDir , "--write-root", "." ]
  Hack -> callProcess "hh_server" $
    [ dir , "--write-symbol-info", outputDir ] <> hackConfig
  where
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

indexCmd :: String -> Eval ()
indexCmd str
  | ["flow", dir] <- words str = index Flow dir
  | ["hack", dir] <- words str = index Hack dir
  | otherwise = liftIO $ throwIO $ ErrorCall
    "syntax:  :index flow|hack <dir>"
  where
    index lang dir =
      withSystemTempDirectory' "glean-shell" $ \tmp -> do
        liftIO $ runIndexer lang dir tmp
        files <- liftIO $ listDirectory tmp
        let name = Text.pack (takeBaseName (dropTrailingPathSeparator dir))
        hash <- pickHash name
        let repo = Glean.Repo name hash
        load repo (map (tmp </>) files)
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

load :: Glean.Repo -> [FilePath] -> Eval ()
load repo files = withBackend $ \be ->  liftIO $ do
  let onExisting  = throwIO $ ErrorCall "database already exists"
  void $ fillDatabase be Nothing repo "" onExisting $
    forM_ files $ \file -> do
      r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
      val <- either (throwIO  . ErrorCall . Text.unpack) return r
      batches <- case Aeson.parse parseJsonFactBatches val of
        Error str -> throwIO $ ErrorCall str
        Aeson.Success x -> return x
      Glean.sendJsonBatch be repo batches Nothing

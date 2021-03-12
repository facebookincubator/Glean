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

indexCmd :: String -> Eval ()
indexCmd str
  | ["flow", dir] <- words str = indexFlow dir
  | otherwise = liftIO $ throwIO $ ErrorCall
    "syntax:  :index flow <dir>"
  where
  indexFlow dir = do
      withSystemTempDirectory' "glean-shell" $ \tmp -> do
        liftIO $ callProcess "flow"
          [ "glean"
          , dir
          , "--output-dir", tmp
          , "--write-root", "." ]
        files <- liftIO $ listDirectory tmp
        let name = Text.pack (takeBaseName dir)
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
  void $ fillDatabase
      be
      repo
      ""
      (throwIO $ ErrorCall "database already exists") $
    forM_ files $ \file -> do
      r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
      val <- either (throwIO  . ErrorCall . Text.unpack) return r
      batches <- case Aeson.parse parseJsonFactBatches val of
        Error str -> throwIO $ ErrorCall str
        Aeson.Success x -> return x
      Glean.sendJsonBatch be repo batches Nothing

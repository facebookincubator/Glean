module Glean.Shell.Index
  ( indexCmd
  , load
  , pickHash
  ) where


import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
  indexFlow dir =
    withBackend $ \be -> do
      repo <- liftIO $ withSystemTempDirectory "glean-shell" $ \tmp -> do
        callProcess "flow"
          [ "glean"
          , dir
          , "--output-dir", tmp
          , "--write-root", "." ]
        files <- listDirectory tmp
        let name = Text.pack (takeBaseName dir)
        hash <- pickHash be name
        let repo = Glean.Repo name hash
        load be repo (map (tmp </>) files)
        return repo
      setRepo repo

pickHash :: Backend b => b -> Text -> IO Text
pickHash be name = do
  r <- liftIO $ listDatabases be def
  let
    hashes =
      [ repo_hash
      | Glean.Database{..} <- Glean.listDatabasesResult_databases r
      , let Glean.Repo{..} = database_repo
      , repo_name == name
      ]
  return $ head $ filter (`notElem` hashes) $ map showt [0::Int ..]

load :: Backend b => b -> Glean.Repo -> [FilePath] -> IO ()
load be repo files =
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

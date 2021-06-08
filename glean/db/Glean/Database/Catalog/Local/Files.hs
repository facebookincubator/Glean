module Glean.Database.Catalog.Local.Files
  ( Files
  , local
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import System.Directory
import System.FilePath

import qualified Thrift.Protocol.JSON as Thrift
import Util.Control.Exception (catchAll)
import Util.IO
import Util.Log

import Glean.Database.Catalog.Store
import Glean.Database.Exception
import Glean.Database.Meta (Meta)
import Glean.Database.Repo (databasePath)
import Glean.Types (Repo(..))
import Glean.Util.IO (readFileContents)

newtype Files = Files { _filesRoot :: FilePath }

metaPath :: FilePath -> Repo -> FilePath
metaPath root repo = databasePath root repo </> "meta"

local :: FilePath -> Files
local = Files

instance Store Files where
  list (Files root) = do
    ex <- doesDirectoryExist root
    if not ex then return mempty else do
    dirs <- filter (not . isPrefixOf ".") <$> getDirectoryContents root
    fmap (HashMap.fromList . catMaybes . concat) $ forM dirs $ \subdir -> do
      let dir = root </> subdir
          repo_name = Text.pack subdir
      hashes <- filter (not . isPrefixOf ".") <$> getDirectoryContents dir
        `catchAll` \exc -> do
          logError $ concat ["couldn't read directory ", dir, ": ", show exc]
          return []
      forM hashes $ \hash ->
        let path = dir </> hash
            skip msg = do
              logError $ concat [path, ": ", msg]
              return Nothing
        in
        do
          s <- readFileContents $ path </> "meta"
          case Thrift.deserializeJSON s of
            Right meta -> do
              return $ Just (Repo repo_name (Text.pack hash), meta)
            Left err -> skip err
        `catchAll` (skip . show)

  create (Files root) repo meta = do
    -- TODO: make this atomic if necessary (cf. renameat2)
    new <- not <$> doesFileExist (metaPath root repo)
    when new $ do
      createDirectoryIfMissing True $ databasePath root repo
      writeMeta root repo meta
    return new

  delete (Files root) repo = do
    -- TODO: make this atomic if necessary
    exists <- doesFileExist (metaPath root repo)
    when exists $ safeRemovePathForcibly $ metaPath root repo
    return exists

  put (Files root) repo meta = do
    -- TODO: make this atomic if necessary
    exists <- doesFileExist (metaPath root repo)
    when exists $ writeMeta root repo meta
    return exists

  get (Files root) repo = do
    let path = metaPath root repo
    exists <- doesFileExist (metaPath root repo)
    if exists
      then do
        s <- readFileContents $ path </> "meta"
        case Thrift.deserializeJSON s of
          Right meta -> return $ Just meta
          Left err -> dbError repo $  "can't parse '" ++ path ++ "': " ++ err
      else
        return Nothing

writeMeta :: FilePath -> Repo -> Meta -> IO ()
writeMeta root = writeFileAtomically
  (\h -> BS.hPut h . Thrift.serializeJSON)
  . metaPath root

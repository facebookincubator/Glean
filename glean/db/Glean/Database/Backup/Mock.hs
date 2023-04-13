{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- | A Glean Backup provider that just copies files to somewhere in
-- the filesystem, for testing purposes.
--
module Glean.Database.Backup.Mock
  ( mock
  , mockSite
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import qualified System.Posix.Files as Posix
import Text.Read

import Glean.Database.Backup.Backend
import Glean.Repo.Text
import Glean.Types
import Glean.Util.Some

data MockBackend = MockBackend

newtype MockSite = MockSite FilePath

mock :: Some Backend
mock = Some MockBackend

mockSite :: FilePath -> Some Site
mockSite = Some . MockSite

instance Backend MockBackend where
  fromPath _ = Just . mockSite . Text.unpack

instance Site MockSite where
  backup (MockSite path) repo props _ttl file = do
    let repo_path = repoPath path repo
    createDirectoryIfMissing True (takeDirectory repo_path)
    copyFile file repo_path
    size <- Posix.fileSize <$> Posix.getFileStatus repo_path
    writeFile (repo_path <.> "props") (show props)
    return Data { dataSize = fromIntegral size }

  inspect (MockSite path) repo = do
    s <- readFile (repoPath path repo <.> "props")
    case readEither s of
      Left err -> throwIO $ ErrorCall $ "can't parse props: " <> err
      Right r -> return r

  restore (MockSite path) repo file = do
    copyFile (repoPath path repo) file
    inspect (MockSite path) repo

  delete (MockSite path) repo =
    removeFile $ repoPath path repo

  enumerate (MockSite path) = do
    fs <- listDirectory path
    fmap catMaybes $ forM fs $ \f -> do
      case splitExtension f of
        (x, ".props") -> case splitExtension x of
          (name,'.':hash) -> do
              let repo = Repo (Text.pack name) (Text.pack hash)
              props <- inspect (MockSite path) repo
              return (Just (repo, props))
          _other -> return Nothing
        _other -> return Nothing

  toPath (MockSite path) = Text.pack path

repoPath :: FilePath -> Repo -> FilePath
repoPath path repo = path </> showRepoSep "." repo

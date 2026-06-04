{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Backup.Backend
  ( Backend(..)
  , Site(..)
  , Data(..)
  , Backends
  , RestoreSource(..)
  , restoreViaFile
  ) where

import Control.Exception (finally)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Unique (newUnique, hashUnique)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO (Handle)

import Glean.Database.Meta
import Glean.Types (Repo)
import Glean.Util.Some

newtype Data = Data { dataSize :: Int }

-- | The source of a serialized database during a restore: either a file
-- already on disk or a readable stream handle.
data RestoreSource
  = SourceFile FilePath
  | SourceStream Handle

-- | A backup backend
class Backend a where
  -- | Parse the textual representation of a 'Site'
  fromPath :: a -> Text -> Maybe (Some Site)

instance Backend (Some Backend) where
  fromPath (Some backend) = fromPath backend

-- | A backup site
class Site a where
  backup
    :: a -- ^ site to back up to
    -> Repo -- ^ repo to back up
    -> Meta -- ^ DB properties
    -> Maybe Int -- ^ Backup TTL in seconds (if supported by Site)
    -> FilePath  -- ^ serialized DB
    -> IO Data
  inspect :: a -> Repo -> IO Meta
  -- | Restore a database, passing a 'RestoreSource' (either a file on
  -- disk or a readable stream) to the given continuation. The site
  -- yields whatever it naturally has: a stream when it can stream, or a
  -- file otherwise. Returns the continuation's result together with the
  -- backup 'Meta'.
  --
  -- The 'FilePath' is a scratch directory which implementations may use
  -- for intermediate files.
  restore :: a -> Repo -> FilePath -> (RestoreSource -> IO b) -> IO (b, Meta)
  delete :: a -> Repo -> IO ()
  enumerate :: a -> IO [(Repo, Meta)]
  toPath :: a -> Text

-- | File-based helper for 'restore': download the backup into a unique
-- file under the scratch directory, then hand a 'SourceFile' to the
-- continuation. The intermediate file is deleted once we are done,
-- including if 'download' fails partway. If the consumer moved or
-- deleted it already, 'removePathForcibly' ignores the missing file.
restoreViaFile
  :: FilePath  -- ^ scratch directory
  -> (FilePath -> IO Meta)  -- ^ download into this path, returning 'Meta'
  -> (RestoreSource -> IO b)
  -> IO (b, Meta)
restoreViaFile scratch download action = do
  u <- newUnique
  let tmpFile = scratch </> "glean-restore" <> show (hashUnique u)
  (`finally` removePathForcibly tmpFile) $ do
    meta <- download tmpFile
    result <- action (SourceFile tmpFile)
    return (result, meta)

instance Site (Some Site) where
  backup (Some site) = backup site
  inspect (Some site) = inspect site
  restore (Some site) = restore site
  delete (Some site) = delete site
  enumerate (Some site) = enumerate site
  toPath (Some site) = toPath site

type Backends = HashMap Text (Some Backend)

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
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Glean.Database.Meta
import Glean.Types (Repo)
import Glean.Util.Some

newtype Data = Data { dataSize :: Int }

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
  restore :: a -> Repo -> FilePath -> IO Meta
  delete :: a -> Repo -> IO ()
  enumerate :: a -> IO [(Repo, Meta)]
  toPath :: a -> Text

instance Site (Some Site) where
  backup (Some site) = backup site
  inspect (Some site) = inspect site
  restore (Some site) = restore site
  delete (Some site) = delete site
  enumerate (Some site) = enumerate site
  toPath (Some site) = toPath site

type Backends = HashMap Text (Some Backend)

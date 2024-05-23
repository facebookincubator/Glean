{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SourceControl (
    SourceControl(..),
    ScmGeneration(..),
    NilSourceControl(..),
    ContentHash(..),
  ) where

import Data.ByteString (ByteString)
import Data.Hashable
import Data.Int

import Glean.Glass.Types
import Glean.Util.Some
import Glean.Haxl.Repos

-- | Source control generation, used for ordering revisions
newtype ScmGeneration = ScmGeneration Int64
  deriving (Eq, Hashable, Ord, Show)

newtype ContentHash = ContentHash ByteString
  deriving (Eq, Hashable, Ord, Show)

-- | Interface to source control operations
class SourceControl scm where
  -- | Retrieve the generation number for a revision, i.e. the number
  -- of commits between the given revision and the root of the repository.
  getGeneration :: scm -> RepoName -> Revision -> IO (Maybe ScmGeneration)

  -- | Retrieve a content hash for the given file, if the file exists
  -- in the repository at the given revision.
  getFileContentHash
    :: scm -> RepoName -> Path -> Revision -> RepoHaxl u w (Maybe ContentHash)

data NilSourceControl = NilSourceControl

instance SourceControl NilSourceControl where
  getGeneration _ _ _ = return Nothing
  getFileContentHash _ _ _ _ = return Nothing

instance SourceControl (Some SourceControl) where
  getGeneration (Some scm) = getGeneration scm
  getFileContentHash (Some scm) = getFileContentHash scm

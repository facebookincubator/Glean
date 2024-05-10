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
  ) where

import Data.Hashable
import Data.Int

import Glean.Glass.Types
import Glean.Util.Some

-- | Source control generation, used for ordering revisions
newtype ScmGeneration = ScmGeneration Int64
  deriving (Eq, Hashable, Ord, Show)

-- | Interface to source control operations
class SourceControl scm where
  getGeneration :: scm -> RepoName -> Revision -> IO (Maybe ScmGeneration)
  -- | @checkMatchingRevisions repo file rev0 revs@ answers the question
  --   "Which of revs satisfy that the contents of filepath match rev0?"
  checkMatchingRevisions
    :: scm -> RepoName -> Path -> Revision -> [Revision] -> IO [Bool]

data NilSourceControl = NilSourceControl

instance SourceControl NilSourceControl where
  getGeneration _ _ _ = return Nothing
  checkMatchingRevisions _ _ _ _ revs = return $ map (const False) revs

instance SourceControl (Some SourceControl) where
  getGeneration (Some scm) = getGeneration scm
  checkMatchingRevisions (Some scm) = checkMatchingRevisions scm

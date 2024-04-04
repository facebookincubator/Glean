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
  deriving (Hashable, Show)

-- | Interface to source control operations
class SourceControl scm where
  getGeneration :: scm -> RepoName -> Revision -> IO (Maybe ScmGeneration)

data NilSourceControl = NilSourceControl

instance SourceControl NilSourceControl where
  getGeneration _ _ _ = return Nothing

instance SourceControl (Some SourceControl) where
  getGeneration (Some scm) = getGeneration scm

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.RepoMapping
  ( getRepoMapping
  , fixedRepoMapping
  , gleanRequiredIndices
  , allGleanRepos
  , supportsCxxDeclarationSources
  , mirrorConfig
  , Mirror(..)
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text(Text)

import Glean.Glass.Base
  ( GleanDBName(..)
  , RepoMapping(..)
  )
import Glean.Glass.Types ( RepoName(..) )

getRepoMapping :: IO RepoMapping
getRepoMapping = return AutoRepoMapping

fixedRepoMapping :: RepoMapping
fixedRepoMapping = AutoRepoMapping

-- | All the Glean db repo names we're aware of
-- We will only be able to query members of this set.
-- 'Nothing' means all existing Glean DBs can be used.
allGleanRepos :: Maybe (Set GleanDBName)
allGleanRepos = Nothing

-- repos that are required
gleanRequiredIndices :: Set.Set GleanDBName
gleanRequiredIndices = Set.empty

supportsCxxDeclarationSources :: GleanDBName -> Bool
supportsCxxDeclarationSources = const True

-- Some repos can be mirrored
data Mirror = Mirror
  { mirrorRepo :: RepoName
  , mirrorPath :: Text
  , sourceRepo :: RepoName
  }

mirrorConfig :: [Mirror]
mirrorConfig = []

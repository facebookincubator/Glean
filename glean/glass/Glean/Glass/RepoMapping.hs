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
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Glean.Glass.Base (
  GleanDBAttrName(..), GleanDBName(..), RepoMapping(..) )
import Glean.Glass.Types ( Language(..), RepoName(..) )

getRepoMapping :: IO RepoMapping
getRepoMapping = return RepoMapping
  { gleanIndices = gleanIndices_
  , gleanAttrIndices = gleanAttrIndices_
  }

fixedRepoMapping :: RepoMapping
fixedRepoMapping = RepoMapping
  { gleanIndices = gleanIndices_
  , gleanAttrIndices = gleanAttrIndices_
  }

-- example: the open source react repo.
gleanIndices_ :: Map.Map RepoName [(GleanDBName, Language)]
gleanIndices_ = Map.fromList
  -- demo
  [ (RepoName "react", [ ("react", Language_JavaScript) ])
  -- for running tests with locally-indexed repos:
  , (RepoName "test",
      [("test", Language_JavaScript)
      ,("test", Language_Hack)
      ,("test", Language_Cpp)
      ,("test", Language_PreProcessor)
      ,("test", Language_Python)
      ,("test", Language_Thrift)
      ,("test", Language_Buck)
      ,("test", Language_Go)
      ,("test", Language_TypeScript)
      ,("test", Language_Rust)
      ,("test", Language_Java)
      ])
  ]

-- repos that contain symbol attributes
gleanAttrIndices_ :: Map.Map GleanDBName [GleanDBAttrName]
gleanAttrIndices_ = Map.empty

-- | All the Glean db repo names we're aware of
-- We will only be able to query members of this set
allGleanRepos :: Set GleanDBName
allGleanRepos = Set.fromList $
  map fst (concat (Map.elems gleanIndices_)) ++
  concatMap (map gleanAttrDBName) (Map.elems gleanAttrIndices_)

-- repos that are required
gleanRequiredIndices :: Set.Set GleanDBName
gleanRequiredIndices = Set.empty

supportsCxxDeclarationSources :: GleanDBName -> Bool
supportsCxxDeclarationSources = const True

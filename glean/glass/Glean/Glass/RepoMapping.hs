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
  , Mirror(Mirror)
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Glean.Glass.Base
  ( GleanDBName(..)
  , RepoMapping(..)
  , GleanDBSelector(..)
  )
import Glean.Glass.Types ( Language(..), RepoName(..) )
import Data.Text(Text)

getRepoMapping :: IO RepoMapping
getRepoMapping = return RepoMapping
  { gleanIndices = gleanIndices_
  , gleanAttrIndices = Map.empty
  }

fixedRepoMapping :: RepoMapping
fixedRepoMapping = RepoMapping
  { gleanIndices = gleanIndices_
  , gleanAttrIndices = Map.empty
  }

-- example: the open source react repo.
gleanIndices_ :: Map.Map RepoName [GleanDBSelector]
gleanIndices_ = Map.fromList
  -- demo
  [ ( RepoName "react",
      [ GleanDBSelector
        { dbName = "react"
        , language = Language_JavaScript
        , branchName = Nothing
        }
      ]
    )
  -- for running tests with locally-indexed repos:
  , ( RepoName "test",
      [ testSelector Language_JavaScript
      , testSelector Language_Hack
      , testSelector Language_Haskell
      , testSelector Language_Cpp
      , testSelector Language_PreProcessor
      , testSelector Language_Python
      , testSelector Language_Thrift
      , testSelector Language_Buck
      , testSelector Language_Go
      , testSelector Language_TypeScript
      , testSelector Language_Rust
      , testSelector Language_Java
      , testSelector Language_Swift
      ]
    )
  ]
  where
    testSelector language =
      GleanDBSelector
        { dbName = "test"
        , language = language
        , branchName = Nothing
        }

-- | All the Glean db repo names we're aware of
-- We will only be able to query members of this set
allGleanRepos :: Set GleanDBName
allGleanRepos = Set.fromList $
  map dbName (concat (Map.elems gleanIndices_))

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

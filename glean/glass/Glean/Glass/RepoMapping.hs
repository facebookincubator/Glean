{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedRecordDot #-}
module Glean.Glass.RepoMapping
  ( getRepoMapping
  , fixedRepoMapping
  , gleanRequiredIndices
  , allGleanRepos
  , supportsCxxDeclarationSources
  , mirrorConfig
  , Mirror(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text(Text)

import Thrift.Protocol.JSON (deserializeJSON)

import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Util.ConfigProvider

import Glean.Glass.Base
  ( GleanDBName(..)
  , RepoMapping(..)
  , GleanDBSelector(..)
  )
import qualified Glean.Glass.Repomapping.Types as Config
import Glean.Glass.SymbolId ( fromShortCode )
import Glean.Glass.Types

getRepoMapping :: ConfigProvider cfg => cfg -> IO RepoMapping
getRepoMapping cfg = ThriftSource.load cfg repoMappingSource

toRepoMapping :: Config.RepoMapping -> RepoMapping
toRepoMapping rm =
  RepoMapping
    { gleanIndices = Map.fromList
        [ (RepoName repo, mapMaybe toSelector selectors)
        | (repo, selectors) <- Map.toList rm.indices
        ]
    , gleanAttrIndices = Map.empty
        -- can do this later if necessary. We would have to pattern-match
        -- on a string and map to the ToAttributes key type.
    }
  where
  toSelector :: Config.DbSelector -> Maybe GleanDBSelector
  toSelector dbsel = do
    lang <- fromShortCode dbsel.language
    return GleanDBSelector
      { dbName = GleanDBName dbsel.name
      , language = lang
      , branchName = dbsel.branch
      }

repoMappingSource :: ThriftSource.ThriftSource RepoMapping
repoMappingSource =
  ThriftSource.configWithDeserializerDefault "glass/repomapping" $
    \bytes -> toRepoMapping <$> deserializeJSON bytes
    -- doing the translation to RepoMapping in the deserializer ensures
    -- that this is cached, we don't have to worry about the translation
    -- happening on every request.

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

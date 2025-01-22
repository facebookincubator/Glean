{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Base
  ( GleanDBName(..)
  , GleanPath(..)
  , SymbolRepoPath(..)
  , GleanDBAttrName(..)
  , RepoMapping(..)
  ) where

import Data.Function
import Data.Hashable
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Logger.GleanGlass as Logger

import Glean ( Repo(..) )
import qualified Glean.Glass.Types as Glass
import Glean.Glass.Logging
import Glean.Glass.Attributes.Class as Attributes

-- | Type of glean dbs
newtype GleanDBName = GleanDBName { unGleanDBName :: Text }
  deriving (Eq, Ord, Show, Hashable)

instance IsString GleanDBName where fromString = GleanDBName . fromString

instance LogResult (NonEmpty (GleanDBName, Glean.Repo)) where
  logResult ((_, repo) :| []) =
    Logger.setRepoName (Glean.repo_name repo) <>
    Logger.setRepoHash (Glean.repo_hash repo)
  logResult rs0@(_ :| _) =
    Logger.setRepoName (commas repo_name rs) <>
    Logger.setRepoHash (commas (Text.take 12 . repo_hash) rs)
    where
      rs = NE.sortBy (compare `on` Glean.repo_name) (fmap snd rs0)

commas :: (Glean.Repo -> Text) -> NonEmpty Glean.Repo -> Text
commas f = Text.intercalate "," . map f . NE.toList

--
-- | A glean path for www is prefixed with www/
-- For all other repos, it is relative to the repo root.
--
newtype GleanPath = GleanPath { gleanPath :: Text }
  deriving (Show, Eq)

--
-- | A path relative to the given repo root
--
data SymbolRepoPath = SymbolRepoPath
  { symbolRepo :: Glass.RepoName
  , symbolPath :: Glass.Path
  }
  deriving (Show, Eq)

-- | A little existential type key for attributes
data GleanDBAttrName =
  forall attr . (Attributes.ToAttributes attr) =>
  GleanDBAttrName {
    gleanAttrDBName :: GleanDBName,
    attributeKey :: attr
  }

data RepoMapping = RepoMapping
  { gleanIndices :: Map.Map Glass.RepoName [(GleanDBName, Glass.Language)]
    -- ^ Glean indexes and the language they index. This should be in a config
    --
    -- This is the set of Glean dbs that should implement codemarkup.*
    --
    -- Note: the order here determines the order of search/lookup
    -- if you have overlapping dbs , for file contents, the first in the order
    -- with a src.File fact will win.
    --
    -- If you add/remove a db, consider if it needs to be present in
    -- gleanRequiredIndices as well

  , gleanAttrIndices :: Map.Map Glass.RepoName [GleanDBAttrName]
    -- ^ Map of source db to attr db names & attribute key types
    --
    -- This pairs attribute Glean dbs with a key type to index the ToAttribute
    -- class, that in turns knowns how to query and marshal the attributes
  }

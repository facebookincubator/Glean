{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
-- | To help configure and resolve the 'Glean.Repo' for queries about
-- a well-known schema.
module Glean.Util.SchemaRepos
  ( SchemaRepos(..), Repos, RepoName, FromSchemaRepos(..)
  , defaultSchemaRepos
  , schemaRepoNames
  , resolveLatestSchemaRepos
  ) where

import qualified Glean (Repo)
import Glean.Repo (LatestRepos(..))

import Data.Text (Text)
import qualified Data.Foldable as Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Maybe

-- | The 'SchemaRepos' type supports search and queries by giving
-- information needed to find the Glean repo to use.  Note that
-- the fields are not merely angle schema names, for example the
-- 'cxxRepo' is used for both schema cxx1 and schema pp searches.
--
-- This will probably get fancier, to reflect SymbolSpecUniverse
data SchemaRepos x = SchemaRepos
  { cxxRepo, hsRepo, javaRepo, hackRepo, pythonRepo :: x }
  deriving (Show, Functor, Foldable, Traversable)

instance Semigroup x => Semigroup (SchemaRepos x) where
  s1 <> s2 = SchemaRepos
    { cxxRepo = cxxRepo s1 <> cxxRepo s2
    , hsRepo = hsRepo s1 <> hsRepo s2
    , javaRepo = javaRepo s1 <> javaRepo s2
    , hackRepo = hackRepo s1 <> hackRepo s2
    , pythonRepo = pythonRepo s1 <> pythonRepo s2
    }

instance Monoid x => Monoid (SchemaRepos x) where
  mempty = SchemaRepos mempty mempty mempty mempty mempty

instance Applicative SchemaRepos where
  pure x = SchemaRepos
    { cxxRepo = x
    , hsRepo = x
    , javaRepo = x
    , hackRepo = x
    , pythonRepo = x
    }

  s1 <*> s2 = SchemaRepos
    { cxxRepo = cxxRepo s1 (cxxRepo s2)
    , hsRepo = hsRepo s1 (hsRepo s2)
    , javaRepo = javaRepo s1 (javaRepo s2)
    , hackRepo = hackRepo s1 (hackRepo s2)
    , pythonRepo = pythonRepo s1 (pythonRepo s2)
    }

-- | The 'Repos' type supports searching by providing a list of resolved
-- 'Glean.Repo' to use for queries related each field of 'SchemaRepos'
type Repos = SchemaRepos [Glean.Repo]

-- | The name of a repo; the type of values for the @repo_name@ field
-- of 'Glean.Repo'.
type RepoName = Text

-- | This 'FromSchemaRepos' is a useful polymorphic getter
newtype FromSchemaRepos =
  FromSchemaRepos { fromSchemaRepos :: forall x. SchemaRepos x -> x }

-- | Default repos to use for searching different parts of the schema
defaultSchemaRepos :: SchemaRepos [RepoName]
defaultSchemaRepos = SchemaRepos
  { cxxRepo = ["fbsource"]
  , hsRepo = ["fbsource-hs", "sigma_hiedb"]
  , javaRepo = ["fbsource.fbandroid"]
  , hackRepo = ["www.hack"]
  , pythonRepo = ["instagram.server"]
  }

schemaRepoNames :: SchemaRepos [RepoName] -> HashSet RepoName
schemaRepoNames = HashSet.fromList . concat . Foldable.toList

resolveLatestSchemaRepos
  :: LatestRepos
  -> SchemaRepos [RepoName]
  -> Repos
resolveLatestSchemaRepos lr = fmap (mapMaybe (`Map.lookup` latestRepos lr))

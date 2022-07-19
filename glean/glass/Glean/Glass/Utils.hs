{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Utils
  (
  -- Search utilities
    fetchData
  , fetchDataRecursive
  , searchWithLimit
  , searchReposWithLimit
  , searchReposAndFilterWithLimit

  , searchPredicateWithLimit
  , searchRecursiveWithLimit

  -- File utilities
  , pathFragments
  , joinFragments

  -- Types
  , QueryType
  ) where

import Glean
    ( recursive, limit, search, search_, getFirstResult )
import Glean.Angle as Angle ( query, Angle )
import Glean.Typed.Binary ( Type )
import Glean.Typed.Predicate ( Predicate )
import Data.Typeable ( Typeable )
import Data.Maybe

import Data.Text as Text ( Text, pack, unpack )
import System.FilePath ( splitDirectories, joinPath )
import qualified Glean.Haxl.Repos as Glean
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl)

type QueryType q =
  ( Typeable q
  , Show q
  , Type q
  )

--
-- | Evaluate an Angle data query and return the first result or Nothing
-- No more than 1 result will be returned from the server.
--
fetchData :: (QueryType a) => Angle a -> RepoHaxl u w (Maybe a)
fetchData = getFirstResult . query

-- | Fetch exactly 0 or 1 results, recusively
fetchDataRecursive :: (QueryType a) => Angle a -> RepoHaxl u w (Maybe a)
fetchDataRecursive = getFirstResult . recursive . query

-- | Run a non-recursive data query with optional limit on search results
searchWithLimit :: QueryType q => Maybe Int -> Angle q -> RepoHaxl u w [q]
searchWithLimit Nothing =
  search_ . Angle.query
searchWithLimit (Just n) =
  fmap fst <$> search . limit n . Angle.query

-- | Run a non-recursive data query with optional limit over multiple repos
searchReposWithLimit
  :: QueryType q
  => Maybe Int
  -> Angle q
  -> (q -> RepoHaxl u w a)
  -> ReposHaxl u w [a]
searchReposWithLimit limit angle act = do
  results <- Glean.queryAllRepos $ do
    res <- searchWithLimit limit angle
    mapM act res -- we would like this to be concurrent
  return $ maybe id take limit results

-- | Search and filter over set of repos. Limit applies to filtered results
searchReposAndFilterWithLimit
  :: QueryType q
  => Maybe Int
  -> Angle q
  -> (q -> RepoHaxl u w (Maybe a))
  -> ReposHaxl u w [a]
searchReposAndFilterWithLimit limit angle act = do
  results <- Glean.queryAllRepos $ do
    res <- searchWithLimit limit angle
    catMaybes <$> mapM act res -- we would like this to be concurrent
  return $ maybe id take limit results

-- | Run a non-recursive predicate only query with optional limit
searchPredicateWithLimit
  :: (Predicate q, QueryType q) => Maybe Int -> Angle q -> RepoHaxl u w [q]
searchPredicateWithLimit Nothing =
  search_ . Angle.query
searchPredicateWithLimit (Just n) =
  fmap fst <$> search . limit n . Angle.query

-- | Run a recursive data query with optional limit on search results
searchRecursiveWithLimit
  :: QueryType q => Maybe Int -> Angle q -> RepoHaxl u w [q]
searchRecursiveWithLimit Nothing =
  search_ . recursive . Angle.query
searchRecursiveWithLimit (Just n) =
  fmap fst <$> search . recursive . limit n . Angle.query

-- | Split a filepath into a list of directory components
pathFragments :: Text -> [Text]
pathFragments = map Text.pack . splitDirectories . Text.unpack

joinFragments :: [Text] -> Text
joinFragments = Text.pack . joinPath . map Text.unpack

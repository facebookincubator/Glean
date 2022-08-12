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
  , searchWithTimeLimit
  , searchReposWithLimit

  , searchPredicateWithLimit
  , searchRecursiveWithLimit

  -- File utilities
  , pathFragments
  , joinFragments

  -- List utils
  , takeFairN
  , splitOnAny

  -- Types
  , QueryType
  ) where

import Data.Text ( Text )
import qualified Data.Text as Text
import System.FilePath ( splitDirectories, joinPath )
import qualified Data.List as List

import Glean ( recursive, limit, limitTime, search, search_, getFirstResult )
import Glean.Angle as Angle ( query, Angle )
import Glean.Typed.Binary ( Type )
import Glean.Typed.Predicate ( Predicate )
import Data.Typeable ( Typeable )

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

-- | Like searchWithLimit but enforce a time bound on the query too.
-- Time budget in milliseconds
searchWithTimeLimit
  :: QueryType q => Maybe Int -> Int -> Angle q -> RepoHaxl u w [q]
searchWithTimeLimit Nothing time =
  search_ . limitTime time . Angle.query
searchWithTimeLimit (Just n) time =
  fmap fst <$> search . limitTime time . limit n . Angle.query

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

-- | Take elements in row,column ordering, so that we select
-- evenly up to N from the set of input lists
--
-- > takeFairN 3 [[1,2],[3],[4,5]] == [1,3,4]
--
takeFairN :: Int -> [[a]] -> [a]
takeFairN _ [] = []
takeFairN n [vs] = take n vs
takeFairN n xs = take n (concat (List.transpose xs))

--
-- Splitv string on substring, with left to right precedence of patterns
--
splitOnAny :: [Text] -> Text -> [Text]
splitOnAny pats src =
  List.foldl' (\acc p -> concatMap (Text.splitOn p) acc) [src] pats

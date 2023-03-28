{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Utils (
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

  -- Str utils
  , splitString

  -- Types
  , QueryType

  -- compat stuff
  , liftMaybe
  , maybeT

) where

import Data.Maybe (fromMaybe)
import Data.Tuple.Extra ( snd3 )
import Data.Text ( Text, length )
import qualified Data.Text as Text
import Data.Typeable ( Typeable )
import System.FilePath ( splitDirectories, joinPath )
import qualified Data.List as List

import Control.Monad.Extra
import Control.Monad.Trans

import Glean ( recursive, limit, limitTime, search, getFirstResult )
import Glean.Angle as Angle ( query, Angle )
import Glean.Typed.Binary ( Type )
import Glean.Typed.Predicate ( Predicate )
import Util.Text (slice)

import qualified Glean.Haxl.Repos as Glean
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl)

import Glean.Glass.Types (
    mAXIMUM_SYMBOLS_QUERY_LIMIT,
    mAXIMUM_QUERY_TIME_LIMIT
  )

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

-- | Run a non-recursive data query with optional limit on search results.
-- Without an explicit limit the global limits apply.
-- This uses the global time limit of 30s per query (2x the expected thrift
-- limit)
searchWithLimit :: QueryType q => Maybe Int -> Angle q -> RepoHaxl u w [q]
searchWithLimit mlimit = searchWithTimeLimit mlimit
  (fromIntegral mAXIMUM_QUERY_TIME_LIMIT)

-- | Like searchWithLimit but enforce a time bound on the query too.
-- Time budget in milliseconds
searchWithTimeLimit
  :: QueryType q => Maybe Int -> Int -> Angle q -> RepoHaxl u w [q]
searchWithTimeLimit mlimit time =
    fmap fst <$> search . limitTime time . limit item . Angle.query
  where
    item = fromMaybe (fromIntegral mAXIMUM_SYMBOLS_QUERY_LIMIT) mlimit

-- | Run a non-recursive predicate only query with optional limit
searchPredicateWithLimit
  :: (Predicate q, QueryType q) => Maybe Int -> Angle q -> RepoHaxl u w [q]
searchPredicateWithLimit = searchWithLimit

-- | Run a recursive data query with optional limit on search results
-- If not limit set, MAXIMUM_SYMBOLS_QUERY_LIMIT is enforced
--
-- Returns a status bit indicating truncated results
--
searchRecursiveWithLimit
  :: QueryType q => Maybe Int -> Angle q -> RepoHaxl u w ([q], Bool)
searchRecursiveWithLimit mlimit =
    search . recursive . limitTime time . limit item . Angle.query
  where
    item = fromMaybe (fromIntegral mAXIMUM_SYMBOLS_QUERY_LIMIT) mlimit
    time = fromIntegral mAXIMUM_QUERY_TIME_LIMIT

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

subString :: Int -> Int -> Text -> Text
subString start len = slice start len

-- Split a type string along reference spans. Annotate extracted fragments.
-- (annotation, start, length) if spans are inconsistent, don't fragment
splitString :: Text -> [(ann, Int, Int)] -> [(Text, Maybe ann)]
splitString s xrefs =
  let res = reverse $ splitStringAux 0 (List.sortOn snd3 xrefs) []
      check = Text.concat $ fmap fst res
  in if check == s then res else [(s, Nothing)]
  where
    n = Data.Text.length s
    splitStringAux pos xrefs res = case xrefs of
        [] | pos == n -> res
           | otherwise -> (subString pos (n - pos) s, Nothing) : res
        (ann, start, length) : xrefs' ->
          if pos == start then
            splitStringAux
              (pos + length)
              xrefs'
              ((subString pos length s, Just ann) : res)
          else
            splitStringAux
              start
              xrefs
              ((subString pos (start - pos) s, Nothing) : res)

-- From Control.Monad.Extra in newer versions
liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

maybeT :: (MonadTrans t, Monad m, MonadPlus (t m)) => m (Maybe b) -> t m b
maybeT act = lift act >>= liftMaybe

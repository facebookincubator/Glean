{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Haxl.DataSource.Glean
  ( get
  , getRec
  , getOfId
  , getRecOfId
  , getKey
  , getKeyRec
  , getKeyOfId
  , getKeyRecOfId
  , haxlRepo
  , search
  , search_
  , count
  , countApprox
  , withRepo
  , HasRepo(..)
  ) where

import Data.Default
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Monoid
import Data.Typeable

import Haxl.Core hiding (Env)

import Glean.Backend.Types
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Typed as Typed

-- User Interface --------

class HasRepo u where
  getRepoFromUserEnv :: u -> Repo

instance HasRepo Repo where
  getRepoFromUserEnv = id

haxlRepo :: HasRepo u => GenHaxl u w Repo
haxlRepo = do
  userEnv <- env userEnv
  return $ getRepoFromUserEnv userEnv

withRepo :: HasRepo u => u -> GenHaxl u w a -> GenHaxl u w a
withRepo userEnv action = do
  coreEnv <- env id
  withEnv coreEnv{userEnv=userEnv} action

get, getRec
  :: (Typeable p, Show p, Predicate p, HasRepo u)
  => p
  -> GenHaxl u w p

get p = haxlRepo >>= \repo -> dataFetch $ Get (getId p) False repo
getRec p = haxlRepo >>= \repo -> dataFetch $ Get (getId p) True repo

getOfId, getRecOfId
  :: (Typeable p, Show p, Predicate p, HasRepo u)
  => IdOf p
  -> GenHaxl u w p

getOfId p = haxlRepo >>= \repo -> dataFetch $ Get p False repo
getRecOfId p = haxlRepo >>= \repo -> dataFetch $ Get p True repo

getKey, getKeyRec
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p, HasRepo u )
  => p
  -> GenHaxl u w (KeyType p)

getKey p = haxlRepo >>= \repo -> dataFetch $ GetKey (getId p) False repo
getKeyRec p = haxlRepo >>= \repo -> dataFetch $ GetKey (getId p) True repo

getKeyOfId, getKeyRecOfId
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p, HasRepo u )
  => IdOf p
  -> GenHaxl u w (KeyType p)

getKeyOfId id = haxlRepo >>= \repo -> dataFetch $ GetKey id False repo
getKeyRecOfId id = haxlRepo >>= \repo -> dataFetch $ GetKey id True repo

-- | Perform a query using Glean. Returns the results and a 'Bool'
-- indicating whether the results were truncated (either by the server
-- or by an explicit 'limit' applied to the query).
search
  :: (Typeable q, Show q, HasRepo u)
  => Query q
  -> GenHaxl u w ([q], Bool)
search q = haxlRepo >>= \repo -> do
  (a, b) <- dataFetch $ mkQueryReq repo q False
  return (fromAppendList a, b)

-- | Like 'search', but returns results only. Always returns all the
-- results, streaming results from the server if necessary.
search_
  :: (Typeable q, Show q, HasRepo u)
  => Query q
  -> GenHaxl u w [q]
search_ q = haxlRepo >>= \repo ->
  fmap (fromAppendList . fst) $ dataFetch $ mkQueryReq repo q True

newtype UniqueResults a = UniqueResults { fromUniqueResults :: HashSet a }
  deriving (Semigroup, Monoid, Show)

instance (Hashable q, Eq q) => QueryResult q (UniqueResults q) where
  fromResults = UniqueResults . HashSet.fromList

-- | Count the unique results of a query.
count
  :: forall q u w.
     (Typeable q, Show q, Hashable q, Eq q, HasRepo u)
  => Query q
  -> GenHaxl u w Int
count q = haxlRepo >>= \repo -> do
  (UniqueResults (s :: HashSet q), _) <- dataFetch $ mkQueryReq repo q True
  return (HashSet.size s)

-- | Returns an upper bound on the number of unique results of a
-- query. This is much more efficient than 'countUnique' because it
-- doesn't keep all the results in memory, but since Glean might
-- return duplicate results it can return a result greater than the
-- true count.
countApprox
  :: (Typeable q, Show q, HasRepo u)
  => Query q
  -> GenHaxl u w Int
countApprox q = haxlRepo >>= \repo ->
  fmap (getSum . fst) $ dataFetch $ mkQueryReq repo q True

-- -----------------------------------------------------------------------------

-- | smart constructor to ensure the query result encoding is set to binary
mkQueryReq
  :: forall q r . (Show q, Typeable q, QueryResult q r)
  => Repo
  -> Query q
  -> Bool
  -> GleanQuery (r, Bool)
mkQueryReq repo (Query q) b = QueryReq (Query q' :: Query q) repo b
  where
  q' = q { userQuery_encodings = [UserQueryEncoding_bin def] }

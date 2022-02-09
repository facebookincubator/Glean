{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
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
  , search
  , search_
  , withRepo
  ) where

import Data.Default
import Data.Typeable

import Haxl.Core hiding (Env)

import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Typed as Typed

import Haxl.DataSource.Glean.Common

-- User Interface --------

class HasRepo u where
  getRepoFromUserEnv :: u -> Repo

instance HasRepo Repo where
  getRepoFromUserEnv = id

getRepo :: HasRepo u => GenHaxl u w Repo
getRepo = do
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

get p = getRepo >>= \repo -> dataFetch $ Get (getId p) False repo
getRec p = getRepo >>= \repo -> dataFetch $ Get (getId p) True repo

getOfId, getRecOfId
  :: (Typeable p, Show p, Predicate p, HasRepo u)
  => IdOf p
  -> GenHaxl u w p

getOfId p = getRepo >>= \repo -> dataFetch $ Get p False repo
getRecOfId p = getRepo >>= \repo -> dataFetch $ Get p True repo

getKey, getKeyRec
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p, HasRepo u )
  => p
  -> GenHaxl u w (KeyType p)

getKey p = getRepo >>= \repo -> dataFetch $ GetKey (getId p) False repo
getKeyRec p = getRepo >>= \repo -> dataFetch $ GetKey (getId p) True repo

getKeyOfId, getKeyRecOfId
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p, HasRepo u )
  => IdOf p
  -> GenHaxl u w (KeyType p)

getKeyOfId id = getRepo >>= \repo -> dataFetch $ GetKey id False repo
getKeyRecOfId id = getRepo >>= \repo -> dataFetch $ GetKey id True repo

-- | Perform a query using Glean. Returns the results and a 'Bool'
-- indicating whether the results were truncated (either by the server
-- or by an explicit 'limit' applied to the query).
search
  :: (Typeable q, Show q, HasRepo u)
  => Query q
  -> GenHaxl u w ([q], Bool)
search q = getRepo >>= \repo -> dataFetch $ mkQueryReq repo q False

-- | Like 'search', but returns results only. Always returns all the
-- results, streaming results from the server if necessary.
search_
  :: (Typeable q, Show q, HasRepo u)
  => Query q
  -> GenHaxl u w [q]
search_ q = getRepo >>= \repo -> fmap fst $ dataFetch $ mkQueryReq repo q True


-- -----------------------------------------------------------------------------

-- | smart constructor to ensure the query result encoding is set to binary
mkQueryReq
  :: (Show q, Typeable q)
  => Repo
  -> Query q
  -> Bool
  -> GleanQuery ([q], Bool)
mkQueryReq repo (Query q decoder) b = QueryReq (Query q' decoder) repo b
  where q' = q { userQuery_encodings = [UserQueryEncoding_bin def] }

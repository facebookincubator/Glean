-- Copyright (c) Facebook, Inc. and its affiliates.

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

get, getRec
  :: (Typeable p, Show p, Predicate p)
  => p
  -> GenHaxl u w p

get p = dataFetch $ Get (getId p) False
getRec p = dataFetch $ Get (getId p) True

getOfId, getRecOfId
  :: (Typeable p, Show p, Predicate p)
  => IdOf p
  -> GenHaxl u w p

getOfId p = dataFetch $ Get p False
getRecOfId p = dataFetch $ Get p True

getKey, getKeyRec
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p )
  => p
  -> GenHaxl u w (KeyType p)

getKey p = dataFetch $ GetKey (getId p) False
getKeyRec p = dataFetch $ GetKey (getId p) True

getKeyOfId, getKeyRecOfId
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p )
  => IdOf p
  -> GenHaxl u w (KeyType p)

getKeyOfId id = dataFetch $ GetKey id False
getKeyRecOfId id = dataFetch $ GetKey id True

-- | Perform a query using Glean. Returns the results and a 'Bool'
-- indicating whether the results were truncated (either by the server
-- or by an explicit 'limit' applied to the query).
search
  :: (Typeable q, Show q)
  => Query q
  -> GenHaxl u w ([q], Bool)
search q = dataFetch $ mkQueryReq q False

-- | Like 'search', but returns results only. Always returns all the
-- results, streaming results from the server if necessary.
search_
  :: (Typeable q, Show q)
  => Query q
  -> GenHaxl u w [q]
search_ q = fmap fst $ dataFetch $ mkQueryReq q True


-- -----------------------------------------------------------------------------

-- | smart constructor to ensure the query result encoding is set to binary
mkQueryReq
  :: (Show q, Typeable q)
  => Query q
  -> Bool
  -> GleanQuery ([q], Bool)
mkQueryReq (Query q decoder) b = QueryReq (Query q' decoder) b
  where q' = q { userQuery_encodings = [UserQueryEncoding_bin def] }

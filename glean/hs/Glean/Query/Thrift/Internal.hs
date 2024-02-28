{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Define internal pieces, please import "Glean.Query.Thrift" instead
module Glean.Query.Thrift.Internal
  ( -- * Types
    Query(..)
    -- * Query combinators
  , angle
  , angleData
  , keys
  , recursive
  , limit
  , limitBytes
  , limitTime
  , store
  , allFacts
  , expanding
    -- * Support
  , reportUserQueryStats
  , showUserQueryStats
  , displayQuery
  , decodeResults
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Dynamic
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc hiding ((<>))
import qualified Data.Vector as Vector
import Text.Printf

import Util.Log

import Glean.Typed as Typed
import Glean.Types as Thrift

type ResultDecoder a =
     IntMap Thrift.Fact               -- ^ serialized nested facts
  -> IORef (IntMap Dynamic)           -- ^ cached deserialized facts
  -> IdOf a
  -> Thrift.Fact
  -> IO a

-- | A query that can be performed by 'runQuery'. Build using 'query'.
data Query a = Type a => Query
  { querySpec :: UserQuery
  }

instance Show (Query a) where
  show (Query spec) = show spec

instance Eq (Query a) where
  Query spec1 == Query spec2 = spec1 == spec2

instance Hashable (Query a) where
  hashWithSalt salt (Query spec) = hashWithSalt salt spec

decodeResults :: UserQueryEncodedResults -> ResultDecoder a -> IO [a]
decodeResults results decoder =
  case results of
    UserQueryEncodedResults_bin UserQueryResultsBin{..} -> do
      cacheRef <- newIORef IntMap.empty
      let serialized = IntMap.fromList
            [ (fromIntegral id,f)
            | (id,f) <- Map.toList userQueryResultsBin_nestedFacts ]
      forM (Map.toList userQueryResultsBin_facts) $ \(fid, fact) -> do
        liftIO $ decoder serialized cacheRef (Typed.IdOf (Fid fid)) fact

    UserQueryEncodedResults_listbin UserQueryResultsListBin{..} -> do
      cacheRef <- newIORef IntMap.empty
      let serialized = IntMap.fromList
            [ (fromIntegral id,f)
            | (id,f) <- Map.toList userQueryResultsListBin_nestedFacts ]
          fids = Vector.toList userQueryResultsListBin_ids
          facts = Vector.toList userQueryResultsListBin_facts
          f fid fact = liftIO $
            decoder serialized cacheRef (Typed.IdOf (Fid fid)) fact
      zipWithM f fids facts

    _other -> throwIO $ ErrorCall
      "runQueryPage: server returned the wrong encoding"

-- | A human-readable form of the Query.
displayQuery :: Query a -> Text
displayQuery (Query UserQuery{..}) = Text.decodeUtf8 userQuery_query

allFacts :: forall q . (Predicate q) => Query q
allFacts = angle $ Text.pack $
  show (pretty (getName (Proxy :: Proxy q))) <> " _"

-- | Build a query for facts using Angle syntax
--
-- Warning: nothing is checking that the type of the query matches the
-- expected type at the callsite. If you get this wrong, undefined
-- behaviour will ensue.
angle :: forall r. (Predicate r) => Text -> Query r
angle q = Query
  { querySpec = angleQuery (getName (Proxy @r)) q
  }

-- | Fetch just the keys of the given fact query
keys :: (Predicate r) => Query r -> Query (KeyType r)
keys (Query spec) = Query { querySpec = spec }

angleQuery :: PredicateRef -> Text -> UserQuery
angleQuery pref query = def
  { userQuery_predicate = predicateRef_name pref
  , userQuery_predicate_version =
    Just (fromIntegral (predicateRef_version pref))
  , userQuery_query = Text.encodeUtf8 query
  , userQuery_options = Just def
    { userQueryOptions_expand_results = False
    , userQueryOptions_syntax = QuerySyntax_ANGLE
    }
  }

-- | A query for arbitrary data, as opposed to 'angle' which queries
-- for facts.
--
-- For example:
--
-- > angleData "{ 3, false }" :: Query (Nat, Bool)
--
-- Warning: nothing is checking that the type of the query matches the
-- expected type at the callsite. If you get this wrong, undefined
-- behaviour will ensue.
angleData :: (Type r) => Text -> Query r
angleData query = Query { querySpec = spec  }
  where
  spec = def
    { userQuery_query = Text.encodeUtf8 query
    , userQuery_options = Just def
      { userQueryOptions_expand_results = False
      , userQueryOptions_syntax = QuerySyntax_ANGLE
      }
    }

-- | Make a query recursive
recursive :: Query a -> Query a
recursive (Query q) = Query q'
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_recursive = True } }

-- | Instead of 'recursive', pick individual predicates to expand in
-- the result
--
-- e.g.
--   expanding @My.Predicate1 $ expanding @My.Predicate2 $ query ...
--
expanding :: forall p a . Predicate p => Query a -> Query a
expanding (Query q) = Query q'
  where
  old = fromMaybe def (userQuery_options q)
  PredicateRef name version = getName (Proxy @p)
  ref = Thrift.SourcePredicate name (Just version)
  q' = q { userQuery_options = Just old
    { userQueryOptions_expand_predicates =
       ref : userQueryOptions_expand_predicates old }}

-- | Set a limit on the number of results returned by a query. This controls
-- query result page size when streaming.
limit :: Int -> Query a -> Query a
limit n (Query q) = Query q'
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_results = Just (fromIntegral n) } }

-- | Set a limit on the size of data returned by a query. This
-- controls query result page size when streaming.
limitBytes :: Int -> Query a -> Query a
limitBytes n (Query q) = Query q'
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_bytes = Just (fromIntegral n) } }

-- | Set a limit on the time that a query can execute for. When streaming,
-- this imposes a time budget on each query page.
limitTime
  :: Int  -- ^ time budget in milliseconds
  -> Query a -> Query a
limitTime n (Query q) = Query q'
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_time_ms = Just (fromIntegral n) } }

-- | Store derived facts arising from the query. This is intended for
-- caching derived facts after creating a DB, not for normal usage. If
-- the datbase is read-only, this will cause the query to fail.
store :: Query a -> Query a
store (Query q) = Query q'
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_store_derived_facts = True } }

reportUserQueryStats :: Thrift.UserQueryStats -> IO ()
reportUserQueryStats stats =
  vlog 1 $ showUserQueryStats stats

showUserQueryStats :: Thrift.UserQueryStats -> String
showUserQueryStats Thrift.UserQueryStats{..} =
  printf "%d facts, %.2fms, %ld bytes\n"
    userQueryStats_num_facts
    (realToFrac userQueryStats_elapsed_ns / 1000000 :: Double)
    userQueryStats_allocated_bytes

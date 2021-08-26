-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
-- | Define internal pieces, please import "Glean.Query.Thrift" instead
module Glean.Query.Thrift.Internal
  ( -- * Types
    ThriftQuery
  , Query(..)
    -- * Query combinators
  , query
  , angle
  , angleData
  , keys
  , recursive
  , limit
  , limitBytes
  , limitTime
  , store
  , allFacts
    -- ** Query JSON
  , MkQuery(..)
    -- * Support
  , reportUserQueryStats
  , showUserQueryStats
  , queryPredicate
  , displayQuery
  , decodeResults
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict)
import Data.ByteString.UTF8 as Utf8String
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
import qualified Text.JSON
import Text.Printf

import Thrift.Protocol
import Thrift.Protocol.JSON
import Util.Log

import Glean.Typed as Typed
import Glean.Types as Thrift

-- | Collection of constraints on a query
type ThriftQuery q =
  ( ThriftSerializable (QueryOf q)
  , Predicate q
  )

type ResultDecoder a =
     IntMap Thrift.Fact               -- ^ serialized nested facts
  -> IORef (IntMap Dynamic)           -- ^ cached deserialized facts
  -> IdOf a
  -> Thrift.Fact
  -> IO a

-- | A query that can be performed by 'runQuery'. Build using 'query'.
data Query a = Query
  { querySpec :: UserQuery
  , queryDecoder :: ResultDecoder a
  }

instance Show (Query a) where
  show (Query spec _) = show spec

instance Eq (Query a) where
  Query spec1 _ == Query spec2 _ = spec1 == spec2

instance Hashable (Query a) where
  hashWithSalt salt (Query spec _) = hashWithSalt salt spec

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

    _other -> throwIO $ ErrorCall
      "runQueryPage: server returned the wrong encoding"

-- | Extract the predicate.  If there is a version then
-- this is reported as 'PredicateRef' otherwise only the predicate name.
queryPredicate :: Query a -> PredicateRef
queryPredicate (Query q _) = case userQuery_predicate_version q of
  Nothing -> error $ "queryPredicate unexpected Nothing version for: " <>
    show (userQuery_predicate q)
  Just v -> PredicateRef{ predicateRef_name = userQuery_predicate q
                        , predicateRef_version = v }

-- | A human-readable form of the Query (as JSON).
displayQuery :: Query a -> Text
displayQuery (Query UserQuery{..} _) = Text.decodeUtf8 userQuery_query

-- | Build a query for passing to `runQuery` (see "Glean.Query.Thrift")
query :: forall q . (ThriftQuery q) => QueryOf q -> Query q
query = mkQuery . serializeJSON

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
  , queryDecoder = Typed.decodeFact
  }

-- | Fetch just the keys of the given fact query
keys :: Predicate r => Query r -> Query (KeyType r)
keys (Query spec _) = Query spec decoder
  where
  decoder x y _fid (Thrift.Fact _ k _) = decodeWithCache x y decodeRtsValue k

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
angleData :: Typed.Type r => Text -> Query r
angleData query = Query spec decoder
  where
  decoder x y _fid (Thrift.Fact _ k _) = decodeWithCache x y decodeRtsValue k
  spec = def
    { userQuery_query = Text.encodeUtf8 query
    , userQuery_options = Just def
      { userQueryOptions_expand_results = False
      , userQueryOptions_syntax = QuerySyntax_ANGLE
      }
    }


-- | Convert various JSON formats into Query, trusting the type, for
-- passing to `runQuery` (see "Glean.Query.Thrift")
class MkQuery a where
  mkQuery :: Predicate q => a -> Query q

instance MkQuery ByteString where
  mkQuery :: forall q. Predicate q => ByteString -> Query q
  mkQuery queryBS = Query spec Typed.decodeFact
   where
   pref = getName (Proxy @q)
   spec = def
     { userQuery_predicate = predicateRef_name pref
     , userQuery_predicate_version =
       Just (fromIntegral (predicateRef_version pref))
     , userQuery_query = queryBS
     , userQuery_options = Just def { userQueryOptions_expand_results = False }
     }

instance MkQuery LBS.ByteString where
  mkQuery = mkQuery . LBS.toStrict

instance MkQuery Aeson.Value where
  mkQuery = mkQuery . LBS.toStrict . Aeson.encode

instance MkQuery Text where
  mkQuery = mkQuery . Text.encodeUtf8

instance MkQuery String where
  mkQuery = mkQuery . Utf8String.fromString

instance MkQuery Text.JSON.JSValue where
  mkQuery = mkQuery . Utf8String.fromString . Text.JSON.encode

-- | Make a query recursive
recursive :: Query a -> Query a
recursive (Query q decoder) = Query q' decoder
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_recursive = True } }

-- | Set a limit on the number of results returned by a query. This controls
-- query result page size when streaming.
limit :: Int -> Query a -> Query a
limit n (Query q decoder) = Query q' decoder
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_results = Just (fromIntegral n) } }

-- | Set a limit on the size of data returned by a query. This
-- controls query result page size when streaming.
limitBytes :: Int -> Query a -> Query a
limitBytes n (Query q decoder) = Query q' decoder
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_bytes = Just (fromIntegral n) } }

-- | Set a limit on the time that a query can execute for. When streaming,
-- this imposes a time budget on each query page.
limitTime
  :: Int  -- ^ time budget in milliseconds
  -> Query a -> Query a
limitTime n (Query q decoder) = Query q' decoder
  where
  q' = q { userQuery_options = Just (fromMaybe def (userQuery_options q))
    { userQueryOptions_max_time_ms = Just (fromIntegral n) } }

-- | Store derived facts arising from the query. This is intended for
-- caching derived facts after creating a DB, not for normal usage. If
-- the datbase is read-only, this will cause the query to fail.
store :: Query a -> Query a
store (Query q decoder) = Query q' decoder
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

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Haxl.DataSource.Glean.Common
  ( GleanGet(..)
  , GleanFetcher
  , mkRequest
  , putResults
  , requestByRepo
  , GleanQuery(..)
  , GleanQueryer
  , putQueryResults
  , State(GleanGetState, GleanQueryState)
  , intId
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import TextShow

import Haxl.Core
import Unsafe.Coerce

import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Typed as Typed


data GleanGet p where
  Get
    :: (Typeable p, Show p, Predicate p)
    => {-# UNPACK #-} !(IdOf p)
    -> Bool
    -> Repo
    -> GleanGet p
  GetKey
    :: (Typeable p, Show p, Predicate p)
    => {-# UNPACK #-} !(IdOf p)
    -> Bool
    -> Repo
    -> GleanGet (KeyType p)

deriving instance Show (GleanGet a)
instance ShowP GleanGet where showp = show

instance Eq (GleanGet p) where
  (Get p rec repo) == (Get q rec' repo') =
    p == q && rec == rec' && repo == repo'
  (GetKey (p :: IdOf a) rec repo) == (GetKey (q :: IdOf b) rec' repo')
    | Just Refl <- eqT @a @b = p == q && rec == rec' && repo == repo'
    -- the KeyTypes being equal doesn't tell us that the predicates are
    -- equal, so we need to check that with eqT here.
  _ == _ = False

instance Hashable (GleanGet a) where
  hashWithSalt salt (Get p rec repo) =
    hashWithSalt salt (0::Int, typeOf p, p, rec, repo)
  hashWithSalt salt (GetKey p rec repo) =
    hashWithSalt salt (1::Int, typeOf p, p, rec, repo)

instance DataSourceName GleanGet where
  dataSourceName _ = "GleanGet"

type GleanFetcher = PerformFetch GleanGet

instance StateKey GleanGet where
  data State GleanGet = GleanGetState GleanFetcher

instance DataSource u GleanGet where
  fetch (GleanGetState fetcher) _ _ = fetcher

{-# INLINE intId #-}
intId :: IdOf p -> Id
intId id = fromIntegral (fromFid (idOf id))


mkRequest :: Maybe UserQueryClientInfo -> [BlockedFetch GleanGet] -> UserQueryFacts
mkRequest minfo requests = def
  { userQueryFacts_facts = map toFactQuery requests
  , userQueryFacts_options = Just def
    { userQueryOptions_expand_results = False
    , userQueryOptions_recursive = False
    , userQueryOptions_max_results =
       Just (fromIntegral (length requests)) }
  , userQueryFacts_encodings = [UserQueryEncoding_bin def]
  , userQueryFacts_client_info = minfo
  }
  where
    toFactQuery :: BlockedFetch GleanGet -> FactQuery
    toFactQuery (BlockedFetch (Get (p :: IdOf p) rec _repo) _) =
      FactQuery
        (intId p)
        (Just (predicateRef_version (getName (Proxy @p))))
        rec
    toFactQuery (BlockedFetch (GetKey (p :: IdOf p) rec _repo) _) =
      FactQuery
        (intId p)
        (Just (predicateRef_version (getName (Proxy @p))))
        rec

requestByRepo :: [BlockedFetch GleanGet] -> HashMap Repo [BlockedFetch GleanGet]
requestByRepo requests =
  HashMap.fromListWith (++) $ map (\req -> (repoOf req, [req])) requests
  where
    repoOf (BlockedFetch (Get _ _ repo) _) = repo
    repoOf (BlockedFetch (GetKey _ _ repo) _) = repo

putResults :: UserQueryResults -> [BlockedFetch GleanGet] -> IO ()
putResults UserQueryResults{..} requests = do
  mapM_ reportUserQueryStats userQueryResults_stats
  UserQueryResultsBin{..} <- expectBinResults userQueryResults_results
  cacheRef <- newIORef IntMap.empty
  emptyCacheRef <- newIORef IntMap.empty
  let
    serialized = IntMap.fromList
      [ (fromIntegral id,f)
      | (id,f) <- Map.toList userQueryResultsBin_nestedFacts ]

    decodeResult (BlockedFetch (Get (p :: IdOf p) rec _repo) rvar) = do
      let fid = intId p
      case Map.lookup fid userQueryResultsBin_facts of
        Nothing ->
          putFailure rvar (Exception $ "can't find fact: " <> showt fid)
        Just fact -> do
          fact' <- liftIO $ decodeFact
            (if rec then serialized else IntMap.empty)
            (if rec then cacheRef else emptyCacheRef)
            (IdOf (Fid fid)) fact
          putSuccess rvar (unsafeCoerce (fact' :: p))
    decodeResult (BlockedFetch (GetKey (p :: IdOf p) rec _repo) rvar) = do
      let fid = intId p
      case Map.lookup fid userQueryResultsBin_facts of
        Nothing ->
          putFailure rvar (Exception $ "can't find fact: " <> showt fid)
        Just (Fact _ k _) -> do
          key <- liftIO $ decodeWithCache
            (if rec then serialized else IntMap.empty)
            (if rec then cacheRef else emptyCacheRef)
            decodeRtsValue k
          putSuccess rvar key

  mapM_ decodeResult requests

expectBinResults :: UserQueryEncodedResults -> IO UserQueryResultsBin
expectBinResults (UserQueryEncodedResults_bin r) = return r
expectBinResults _ = throwIO $ Exception "server returned the wrong encoding"


{-
Why is streaming handled behind the datasource abstraction instead of
exposing resumable queries as a request?  Because exposing resumable
queries as a Haxl data fetch would mean hashing the continuation and
keeping it in the Haxl cache.
-}

data GleanQuery r where
  QueryReq
    :: (Show q, Typeable q)
    => Query q   -- The query
    -> Repo
    -> Bool -- stream all results?
    -> GleanQuery ([q], Bool)

deriving instance Show (GleanQuery q)
instance ShowP GleanQuery where showp = show

instance Eq (GleanQuery r) where
  QueryReq (q1 :: Query a) repo1 s1 == QueryReq (q2 :: Query b) repo2 s2
    | Just Refl <- eqT @a @b = q1 == q2 && repo1 == repo2 && s1 == s2
  _ == _ = False

instance Hashable (GleanQuery q) where
  hashWithSalt salt (QueryReq q s repo) = hashWithSalt salt (q,s,repo)


instance DataSourceName GleanQuery where
  dataSourceName _ = "GleanQuery"

type GleanQueryer = PerformFetch GleanQuery

instance StateKey GleanQuery where
  data State GleanQuery = GleanQueryState GleanQueryer

instance DataSource u GleanQuery where
  fetch (GleanQueryState queryer) _ _ = queryer


putQueryResults
  :: Query q
  -> UserQueryResults
  -> Maybe ([q] -> [q]) -- results so far
  -> ResultVar ([q], Bool)
  -> (Query q -> Maybe ([q] -> [q]) -> IO ())
     -- ^ How to resume if we're streaming
  -> IO ()
putQueryResults (Query q decoder) UserQueryResults{..} maybeAcc rvar more = do
  mapM_ reportUserQueryStats userQueryResults_stats
  UserQueryResultsBin{..} <- expectBinResults userQueryResults_results
  cacheRef <- newIORef IntMap.empty
  let
    serialized = IntMap.fromList
      [ (fromIntegral id,f)
      | (id,f) <- Map.toList userQueryResultsBin_nestedFacts ]
  results <- forM (Map.toList userQueryResultsBin_facts) $ \(fid, fact) -> do
    liftIO $ decoder serialized cacheRef
      (Typed.IdOf (Fid fid)) fact

  if
    -- If the server gave us fewer results and streaming is enabled,
    -- fetch more results.
    | Just acc <- maybeAcc, Just cont <- userQueryResults_continuation
      -> more
         (Query q { userQuery_options = Just
           (fromMaybe def (userQuery_options q))
             { userQueryOptions_continuation = Just cont } } decoder)
         (Just (acc . (results++)))

    | otherwise -> do
      let allResults = fromMaybe id maybeAcc results
      putSuccess rvar (allResults, isJust userQueryResults_continuation)

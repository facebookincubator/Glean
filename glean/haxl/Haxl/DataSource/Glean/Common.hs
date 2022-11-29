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
  , putQueryResultsOrException
  , State(GleanGetState, GleanQueryState)
  , intId
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Default
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

import Glean.Backend.Types
import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Typed as Typed
import qualified Haxl.Core.DataSource as Haxl


{-# INLINE intId #-}
intId :: IdOf p -> Id
intId id = fromIntegral (fromFid (idOf id))

mkRequest
  :: Maybe UserQueryClientInfo
  -> Maybe SchemaId
  -> [BlockedFetch GleanGet]
  -> UserQueryFacts
mkRequest minfo schema requests = def
  { userQueryFacts_facts = map toFactQuery requests
  , userQueryFacts_options = Just def
    { userQueryOptions_expand_results = False
    , userQueryOptions_recursive = False
    , userQueryOptions_max_results =
       Just (fromIntegral (length requests)) }
  , userQueryFacts_encodings = [UserQueryEncoding_bin def]
  , userQueryFacts_client_info = minfo
  , userQueryFacts_schema_id = coerce schema
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


putQueryResults
  :: Query q
  -> UserQueryResults
  -> Maybe ([q] -> [q]) -- results so far
  -> ResultVar ([q], Bool)
  -> (Query q -> Maybe ([q] -> [q]) -> IO ())
     -- ^ How to resume if we're streaming
  -> IO ()
putQueryResults (Query q) UserQueryResults{..} maybeAcc rvar more = do
  mapM_ reportUserQueryStats userQueryResults_stats
  UserQueryResultsBin{..} <- expectBinResults userQueryResults_results
  cacheRef <- newIORef IntMap.empty
  let
    serialized = IntMap.fromList
      [ (fromIntegral id,f)
      | (id,f) <- Map.toList userQueryResultsBin_nestedFacts ]
  results <- forM (Map.toList userQueryResultsBin_facts) $ \(fid, fact) -> do
    liftIO $ decodeAsFact serialized cacheRef
      (Typed.IdOf (Fid fid)) fact

  if
    -- If the server gave us fewer results and streaming is enabled,
    -- fetch more results.
    | Just acc <- maybeAcc, Just cont <- userQueryResults_continuation
      -> more
         (Query q { userQuery_options = Just
           (fromMaybe def (userQuery_options q))
             { userQueryOptions_continuation = Just cont } })
         (Just (acc . (results++)))

    | otherwise -> do
      let allResults = fromMaybe id maybeAcc results
      putSuccess rvar (allResults, isJust userQueryResults_continuation)


putQueryResultsOrException
  :: Query q
  -> UserQueryResultsOrException
  -> Maybe ([q] -> [q]) -- results so far
  -> ResultVar ([q], Bool)
  -> (Query q -> Maybe ([q] -> [q]) -> IO ())
     -- ^ How to resume if we're streaming
  -> IO ()
putQueryResultsOrException q r maybeAcc rvar more =
  case r of
    UserQueryResultsOrException_results r ->
      putQueryResults q r maybeAcc rvar more
    UserQueryResultsOrException_badQuery ex ->
      Haxl.putFailure rvar (toException ex)
    UserQueryResultsOrException_retry ex ->
      Haxl.putFailure rvar (toException ex)
    UserQueryResultsOrException_other ex ->
      Haxl.putFailure rvar (toException ex)
    _ -> error "TODO"

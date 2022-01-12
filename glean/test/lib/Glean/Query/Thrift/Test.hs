{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds, NamedFieldPuns #-}

-- | Query API for testing only.  See "Glean.Query.Thrift" for the real API.
module Glean.Query.Thrift.Test
  ( -- * Decode result
    runQueryJSON
  , runQueryJSON_
  , runQueryCompact
  , runQueryCompact_
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Builder as B
import Data.Default
import Data.Maybe

import Thrift.Protocol
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

import Glean.Typed as Typed
import Glean.Types as Thrift
import Glean.Backend (Backend(..))
import Glean.Query.Thrift.Internal

type TestQuery q =
  ( Typed.Predicate q
  , ThriftSerializable (QueryOf q)
  , ThriftSerializable q
  )

-- | Perform a query using the Thrift query and result types, using
-- JSON transport for the results. This is only for testing, use
-- `runQuery` instead.
runQueryJSON_
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO [q]
runQueryJSON_ be repo q = fst <$> runQueryJSON be repo q


runQueryJSON
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO ([q], Bool)
runQueryJSON be repo (Query query _) = do
  -- Only works for whole facts, but that's enforced by the TestQuery context.
  let
    query' = query
      { userQuery_options = Just (fromMaybe def (userQuery_options query))
        { userQueryOptions_expand_results = True } }
  UserQueryResults{..} <- userQuery be repo query'
  mapM_ reportUserQueryStats userQueryResults_stats
  results <- forM userQueryResults_facts $ \fact -> do
    case deserializeJSON fact of
      Left err -> throwIO $ ErrorCall $ "deserialization failure: " ++ err
      Right x -> return x
  return (results, isJust userQueryResults_continuation)

runQueryCompact_
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO [q]
runQueryCompact_ be repo q = fst <$> runQueryCompact be repo q

runQueryCompact
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO ([q], Bool)
runQueryCompact be repo (Query query _) = do
  -- Only works for whole facts, but that's enforced by the TestQuery context.
  let
    query' = query
      { userQuery_encodings =
          [ UserQueryEncoding_compact UserQueryEncodingCompact
              { userQueryEncodingCompact_expand_results = True }
          ]
      }
  UserQueryResults{..} <- userQuery be repo query'
  UserQueryEncodedResults_compact UserQueryResultsCompact{..} <-
    return userQueryResults_results
  mapM_ reportUserQueryStats userQueryResults_stats
  results <- forM userQueryResultsCompact_facts $ \fact -> do
    case deserializeCompact fact of
      Left err -> throwIO $ ErrorCall $
        "deserialization failure: "
        ++ err
        ++ " " ++ show (B.toLazyByteString $ B.byteStringHex fact)
      Right x -> return x
  return (results, isJust userQueryResults_continuation)

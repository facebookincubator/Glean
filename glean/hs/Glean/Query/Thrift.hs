{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Thrift
  ( -- * Types
    Query
    -- * Perform query
  , runQuery
  , runQuery_
  , runQueryPage
    -- ** streaming
  , runQueryMapPages_
  , runQueryEach
  , runQueryEachBatch
    -- * Query combinators
  , angle
  , angleData
  , keys
  , recursive
  , limit
  , limitBytes
  , limitTime
  , expanding
  , store
  , allFacts
    -- * Support
  , queryPredicate
  , displayQuery
  ) where

import Control.Concurrent.Async
import Control.Monad
import Data.Default
import Data.Maybe

import Util.Log.Text

import Glean.Typed.Binary
import Glean.Types as Thrift
import Glean.Backend.Types (Backend(..))
import Glean.Query.Thrift.Internal
import Glean.Write.SendBatch

-- | Perform a query using the Thrift query and result types.
runQuery_
  :: forall q backend . (Backend backend)
  => backend    -- ^ Backend to use to perform the query
  -> Repo       -- ^ Repo to query
  -> Query q    -- ^ The query, a Thrift-generated type from glean/schema
  -> IO [q]
runQuery_ backend repo query = do
  r <- runQueryEachBatch backend repo query id $
    \acc page -> return (acc . (page++))
  return (r [])

runQuery
  :: forall q backend . (Backend backend)
  => backend    -- ^ Backend to use to perform the query
  -> Repo       -- ^ Repo to query
  -> Query q    -- ^ The query, a Thrift-generated type from glean/schema
  -> IO ([q], Bool)
       -- ^ The 'Bool' is 'True' if the query results were truncated
       -- by a limit (either a user-supplied limit, or one imposed by
       -- the query server).
runQuery backend repo query = do
  (io, cont) <- runQueryPage backend repo Nothing query
  results <- io
  return (results, isJust cont)


-- | Perform a query using the Thrift query and result types.
runQueryPage
  :: forall q backend . (Backend backend)
  => backend
  -> Repo
  -> Maybe UserQueryCont
  -> Query q
  -> IO (IO [q], Maybe UserQueryCont)

runQueryPage be repo cont (Query query) = do
  let
    query' = query
      { userQuery_options = Just
          (fromMaybe def (userQuery_options query))
            { userQueryOptions_continuation = cont }
      , userQuery_encodings = [
          UserQueryEncoding_listbin def,
          UserQueryEncoding_bin def
        ]
      , userQuery_schema_id = schemaId be
      }
  UserQueryResults{..} <- userQuery be repo query'
  mapM_ reportUserQueryStats userQueryResults_stats
  mapM_ (vlog 1) userQueryResults_diagnostics
  mapM_ (waitBatch be) userQueryResults_handle
  let results = decodeResults userQueryResults_results decodeAsFact
  return (results, userQueryResults_continuation)


-- | Perform a query and map an IO function over the results, running the
-- query over multiple pages of results if necessary.
runQueryMapPages_
  :: forall q backend . (Backend backend)
  => backend
  -> Repo
  -> ([q] -> IO ())
  -> Query q
  -> IO ()
runQueryMapPages_ be repo fn query =
  runQueryEachBatch be repo query () (const fn)

-- | Perform a query, by pages, and map a state function over each
-- result in each page. The query is pipelined so that each page of
-- results are decoded and processed while the next page is being
-- fetched.
runQueryEach
  :: forall q backend s . (Backend backend)
  => backend
  -> Repo
  -> Query q
  -> s
  -> (s -> q -> IO s)
  -> IO s
runQueryEach be repo query s fn =
  runQueryEachBatch be repo query s (foldM fn)

-- | Like runQueryEach, but process results in batches.
runQueryEachBatch
  :: forall q backend s . (Backend backend)
  => backend
  -> Repo
  -> Query q
  -> s
  -> (s -> [q] -> IO s)
  -> IO s
runQueryEachBatch be repo query init f = do
  (page, cont) <- runQueryPage be repo Nothing query
  go page cont init
  where
  go page Nothing s = f s =<< page
  go page cont@Just{} s = do
    ((nextPage, nextCont), s2) <- concurrently
      (runQueryPage be repo cont query)
      (f s =<< page)
    go nextPage nextCont s2

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Support for using the Glean Haxl datasource with an arbitrary
-- Glean Backend, either local or remote.
module Haxl.DataSource.Glean.Backend
  ( initGlobalState
  ) where

import Control.Monad (forM_)
import Data.Typeable

import Haxl.Core hiding (Env)
import Util.Control.Exception

import Glean.Backend.Remote
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Types

import Haxl.DataSource.Glean.Common
import qualified Haxl.DataSource.Glean.Remote as Remote
import qualified Data.HashMap.Strict as HashMap


syncGet
  :: Backend b
  => b
  -> PerformFetch GleanGet
syncGet env = SyncFetch $ \requests -> do
  forM_ (HashMap.toList $ requestByRepo requests) $ \(repo, requests) -> do
    results <- userQueryFacts env repo (mkRequest Nothing requests)
    putResults results requests


syncQuery :: Backend b => b -> PerformFetch GleanQuery
syncQuery env = SyncFetch $ mapM_ fetch
  where
    fetch :: BlockedFetch GleanQuery -> IO ()
    fetch (BlockedFetch (QueryReq q repo stream) rvar) =
      runSyncQuery repo env q (if stream then Just id else Nothing) rvar


runSyncQuery
  :: forall q b. (Show q, Typeable q, Backend b)
  => Repo
  -> b
  -> Query q
  -> Maybe ([q] -> [q]) -- results so far
  -> ResultVar ([q], Bool)
  -> IO ()
runSyncQuery repo env q@(Query req _) acc rvar = do
  r <- tryAll $ userQuery env repo req
  case r of
    Left e -> putFailure rvar e
    Right results ->
      putQueryResults q results acc rvar $
        \(q :: Query q) acc -> runSyncQuery repo env q acc rvar


initGlobalState
  :: Backend b
  => b
  -> IO (State GleanGet, State GleanQuery)
initGlobalState backend =  do
  case maybeRemote backend of
    Just t -> Remote.initGlobalState t
    Nothing ->
      return
        ( GleanGetState (syncGet backend)
        , GleanQueryState(syncQuery backend)
        )

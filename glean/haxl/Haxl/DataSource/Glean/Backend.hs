-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Support for using the Glean Haxl datasource with an arbitrary
-- Glean Backend, either local or remote.
module Haxl.DataSource.Glean.Backend
  ( initGlobalState
  ) where

import Data.Typeable

import Haxl.Core hiding (Env)
import Util.Control.Exception

import Glean.Backend.Remote
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Types

import Haxl.DataSource.Glean.Common
import qualified Haxl.DataSource.Glean.Remote as Remote


syncGet
  :: Backend b
  => b
  -> Repo
  -> PerformFetch GleanGet
syncGet env repo = SyncFetch $ \requests -> do
  results <- userQueryFacts env repo (mkRequest requests)
  putResults results requests


syncQuery :: Backend b => b -> Repo -> PerformFetch GleanQuery
syncQuery env repo = SyncFetch $ mapM_ fetch
  where
    fetch :: BlockedFetch GleanQuery -> IO ()
    fetch (BlockedFetch (QueryReq q stream) rvar) =
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
  -> Repo
  -> IO (State GleanGet, State GleanQuery)
initGlobalState backend repo =  do
  case maybeRemote backend of
    Just t -> Remote.initGlobalState t repo
    Nothing ->
      return
        ( GleanGetState repo (syncGet backend)
        , GleanQueryState repo (syncQuery backend)
        )

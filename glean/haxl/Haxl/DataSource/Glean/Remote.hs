{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

-- | Support for using the Glean Haxl datasource with a remote server
module Haxl.DataSource.Glean.Remote
  ( initGlobalState
  ) where

import Control.Exception
import Control.Monad
import Data.Typeable

import Haxl.Core hiding (Env)
import Haxl.DataSource.Thrift
import Thrift.Channel
import Util.EventBase

import Glean.Backend.Remote
import Glean.ClientConfig.Types
import Glean.GleanService.Client
import Glean.Impl.ThriftService
import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Util.ThriftService

import Haxl.DataSource.Glean.Common


initGlobalState
  :: ThriftBackend
  -> Repo
  -> IO (State GleanGet, State GleanQuery)
initGlobalState backend repo =  do
  return
    ( GleanGetState repo (remoteFetch backend)
    , GleanQueryState repo (remoteQuery backend)
    )

remoteFetch :: ThriftBackend -> Repo -> PerformFetch GleanGet
remoteFetch (ThriftBackend config evb ts clientInfo) repo =
  BackgroundFetch $ \requests -> do
  let
    ts' = case clientConfig_use_shards config of
      NO_SHARDS -> ts
      USE_SHARDS -> thriftServiceWithShard ts (Just (dbShard repo))
      USE_SHARDS_AND_FALLBACK ->
        thriftServiceWithShard ts (Just (dbShard repo)) -- TODO

  runThrift evb ts' $ do
    let
      sendCob :: Maybe ChannelException -> IO ()
      sendCob Nothing = return ()
      sendCob (Just ex) = putException (toException ex) requests

      recvCob
        :: (Response -> Either SomeException UserQueryResults)
        -> RecvCallback
      recvCob _ (Left ex) = putException (toException ex) requests
      recvCob deserialize (Right response) =
        case deserialize response of
          Left err -> putException err requests
          Right res -> putResults res requests

    dispatchCommon
      (\p c ct s r o x -> send_userQueryFacts p c ct s r o repo x)
      sendCob
      (recvCob . recv_userQueryFacts)
      (mkRequest (Just clientInfo) requests)

putException :: SomeException -> [BlockedFetch a] -> IO ()
putException ex requests =
  forM_ requests $ \(BlockedFetch _ rvar) -> putFailure rvar ex


remoteQuery :: ThriftBackend -> Repo -> PerformFetch GleanQuery
remoteQuery (ThriftBackend config evb ts clientInfo) repo =
  BackgroundFetch $ mapM_ fetch
  where
  ts' = case clientConfig_use_shards config of
    NO_SHARDS -> ts
    USE_SHARDS -> thriftServiceWithShard ts (Just (dbShard repo))
    USE_SHARDS_AND_FALLBACK ->
      thriftServiceWithShard ts (Just (dbShard repo)) -- TODO

  fetch :: BlockedFetch GleanQuery -> IO ()
  fetch (BlockedFetch (QueryReq q stream) rvar) =
    runRemoteQuery evb repo q' ts' acc rvar
    where
      q' = withClientInfo clientInfo q
      acc = if stream then Just id else Nothing

  withClientInfo :: UserQueryClientInfo -> Query q -> Query q
  withClientInfo info (Query q d) = Query q' d
    where
      q' = q { userQuery_client_info = Just info }


runRemoteQuery
  :: forall q. (Show q, Typeable q)
  => EventBaseDataplane
  -> Repo
  -> Query q
  -> ThriftService GleanService
  -> Maybe ([q] -> [q]) -- results so far
  -> ResultVar ([q], Bool)
  -> IO ()
runRemoteQuery evb repo q@(Query req _) ts acc rvar =
  runThrift evb ts $ do
    let
      recvCob
        :: (Response -> Either SomeException UserQueryResults)
        -> RecvCallback
      recvCob _ (Left ex) = putFailure rvar (toException ex)
      recvCob deserialize (Right response) =
        case deserialize response of
          Left err -> putFailure rvar err
          Right res -> putQueryResults q res acc rvar $
            \(q :: Query q) acc -> runRemoteQuery evb repo q ts acc rvar

    dispatchCommon
      (\p c ct s r o x -> send_userQuery p c ct s r o repo x)
      (sendCobSingle rvar)
      (recvCob . recv_userQuery)
      req

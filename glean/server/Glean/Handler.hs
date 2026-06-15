{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Handler
  ( State(..)
  , handler
  ) where

import Data.Maybe

import Facebook.Fb303

import Glean as Backend
import Glean.GleanService.Service as Service
import Glean.Backend.Local
import Glean.Backend.Logging
import qualified Glean.Types as Thrift

data State = State
  { fb303State :: Fb303State
  , stEnv :: Env
  }

handler :: State -> GleanServiceCommand a -> IO a
handler State{..} req =
  let backend = LoggingBackend stEnv in -- log (most) requests
  case req of
    Service.GetSchemaInfo repo req ->
      Backend.getSchemaInfo backend (Just repo) req

    Service.GetSchemaInfoForSchema req ->
      Backend.getSchemaInfo backend Nothing req

    Service.ValidateSchema req -> Backend.validateSchema backend req

    Service.SendBatch cbatch -> Backend.enqueueBatch backend cbatch

    Service.EnqueueBatch repo batch waitPolicy
      -> Backend.enqueueBatchDescriptor backend repo batch waitPolicy

    Service.FinishBatch handle ->
      Backend.pollBatch backend Nothing handle

    Service.SendJsonBatch repo batch ->
      Backend.enqueueJsonBatch backend repo batch

    Service.KickOff rq -> Backend.kickOffDatabase backend rq

    Service.Finish repo -> Backend.finishDatabase backend repo

    Service.Finalize repo -> Backend.finalizeDatabase backend repo

    Service.UpdateProperties repo set unset ->
      Backend.updateProperties backend repo set unset

    Service.QueryFact repo id ->
      fromMaybe (Thrift.Fact 0 mempty mempty) <$>
        Backend.queryFact backend repo id

    Service.FactIdRange repo -> Backend.factIdRange backend repo

    Service.UserQueryFacts repo req ->
      Backend.userQueryFacts backend repo req

    Service.UserQuery repo query ->
      Backend.userQuery backend repo query

    Service.UserQueryBatch repo queries -> do
      Backend.userQueryBatch backend repo queries

    Service.CompletePredicates repo preds ->
      Backend.completePredicates_ backend repo preds

    Service.WaitForWrites repo ->
      Backend.waitForWrites backend repo

    Service.DeriveStored repo pred ->
      Backend.deriveStored backend (const mempty) repo pred

    Service.PredicateStats repo Thrift.PredicateStatsOpts{..} ->
      Backend.predicateStats backend repo $
        if predicateStatsOpts_excludeBase then ExcludeBase else IncludeBase

    Service.ListDatabases l ->
      Backend.listDatabases backend l

    Service.GetDatabase repo -> Backend.getDatabase backend repo

    Service.DeleteDatabase repo -> Backend.deleteDatabase backend repo

    Service.Restore loc -> Backend.restoreDatabase backend loc

    -- V2 stubs: delegate to V1 and wrap with Nothing auth fields. The
    -- real auth-aware implementations will populate these auth fields later.
    -- MIGRATION: cleanup with v2_migrated_clients
    Service.ValidateSchemaV2 req -> do
      Backend.validateSchema backend req
      return Thrift.ValidateSchemaResult
        { validateSchemaResult_auth_status = Nothing
        , validateSchemaResult_auth_message = Nothing
        }

    Service.RestoreV2 loc -> do
      Backend.restoreDatabase backend loc
      return Thrift.RestoreResult
        { restoreResult_auth_status = Nothing
        , restoreResult_auth_message = Nothing
        }

    Service.PredicateStatsV2 repo Thrift.PredicateStatsOpts{..} -> do
      stats <- Backend.predicateStats backend repo $
        if predicateStatsOpts_excludeBase then ExcludeBase else IncludeBase
      return Thrift.PredicateStatsResult
        { predicateStatsResult_stats = stats
        , predicateStatsResult_auth_status = Nothing
        , predicateStatsResult_auth_message = Nothing
        }

    Service.SendBatchV2 cbatch -> do
      response <- Backend.enqueueBatch backend cbatch
      return Thrift.SendBatchResult
        { sendBatchResult_response = response
        , sendBatchResult_auth_status = Nothing
        , sendBatchResult_auth_message = Nothing
        }

    Service.FinishBatchV2 handle -> do
      response <- Backend.pollBatch backend Nothing handle
      return Thrift.FinishBatchResult
        { finishBatchResult_response = response
        , finishBatchResult_auth_status = Nothing
        , finishBatchResult_auth_message = Nothing
        }

    Service.DeriveStoredV2 repo pred -> do
      status <- Backend.deriveStored backend (const mempty) repo pred
      return Thrift.DeriveStoredResult
        { deriveStoredResult_status = status
        , deriveStoredResult_auth_status = Nothing
        , deriveStoredResult_auth_message = Nothing
        }

    SuperFacebookService c -> fb303Handler fb303State c

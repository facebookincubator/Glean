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
import Control.Monad (void)

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
  case req of
    Service.GetSchemaInfo repo req ->
      Backend.getSchemaInfo backend (Just repo) req

    Service.GetSchemaInfoForSchema req ->
      Backend.getSchemaInfo backend Nothing req

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.ValidateSchema req -> void $ validateSchemaV2 req

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.SendBatch cbatch ->
      Thrift.sendBatchResult_response <$> sendBatchV2 cbatch

    Service.EnqueueBatch repo batch waitPolicy
      -> Backend.enqueueBatchDescriptor backend repo batch waitPolicy

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.FinishBatch handle ->
      Thrift.finishBatchResult_response <$> finishBatchV2 handle

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

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.DeriveStored repo pred ->
      Thrift.deriveStoredResult_status <$> deriveStoredV2 repo pred

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.PredicateStats repo opts ->
      Thrift.predicateStatsResult_stats <$> predicateStatsV2 repo opts

    Service.ListDatabases l ->
      Backend.listDatabases backend l

    Service.GetDatabase repo -> Backend.getDatabase backend repo

    Service.DeleteDatabase repo -> Backend.deleteDatabase backend repo

    -- AUTH MIGRATION: delete when all clients migrated to V2 calls
    Service.Restore loc -> void $ restoreV2 loc

    -- V2 handlers hold the canonical business logic; the V1 arms above
    -- delegate to these and unwrap the payload, so deleting V1 leaves the
    -- logic intact. Auth fields are Nothing here; they are populated by the
    -- dispatch-level attachAuth helper.
    -- AUTH MIGRATION: keep these V2 arms; the V1 arms above are deleted when
    -- all clients migrated to V2 calls.
    Service.ValidateSchemaV2 req -> validateSchemaV2 req

    Service.RestoreV2 loc -> restoreV2 loc

    Service.PredicateStatsV2 repo opts -> predicateStatsV2 repo opts

    Service.SendBatchV2 cbatch -> sendBatchV2 cbatch

    Service.FinishBatchV2 handle -> finishBatchV2 handle

    Service.DeriveStoredV2 repo pred -> deriveStoredV2 repo pred

    SuperFacebookService c -> fb303Handler fb303State c
  where
    backend = LoggingBackend stEnv -- log (most) requests

    validateSchemaV2 schema = do
      Backend.validateSchema backend schema
      return Thrift.ValidateSchemaResult
        { validateSchemaResult_auth_status = Nothing
        , validateSchemaResult_auth_message = Nothing
        }

    restoreV2 loc = do
      Backend.restoreDatabase backend loc
      return Thrift.RestoreResult
        { restoreResult_auth_status = Nothing
        , restoreResult_auth_message = Nothing
        }

    predicateStatsV2 repo Thrift.PredicateStatsOpts{..} = do
      stats <- Backend.predicateStats backend repo $
        if predicateStatsOpts_excludeBase then ExcludeBase else IncludeBase
      return Thrift.PredicateStatsResult
        { predicateStatsResult_stats = stats
        , predicateStatsResult_auth_status = Nothing
        , predicateStatsResult_auth_message = Nothing
        }

    sendBatchV2 cbatch = do
      response <- Backend.enqueueBatch backend cbatch
      return Thrift.SendBatchResult
        { sendBatchResult_response = response
        , sendBatchResult_auth_status = Nothing
        , sendBatchResult_auth_message = Nothing
        }

    finishBatchV2 handle = do
      response <- Backend.pollBatch backend Nothing handle
      return Thrift.FinishBatchResult
        { finishBatchResult_response = response
        , finishBatchResult_auth_status = Nothing
        , finishBatchResult_auth_message = Nothing
        }

    deriveStoredV2 repo pred = do
      status <- Backend.deriveStored backend (const mempty) repo pred
      return Thrift.DeriveStoredResult
        { deriveStoredResult_status = status
        , deriveStoredResult_auth_status = Nothing
        , deriveStoredResult_auth_message = Nothing
        }

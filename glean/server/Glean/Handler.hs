-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Handler
  ( Write(..)
  , State(..)
  , handler
  ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe

import Facebook.Fb303

import Glean.GleanService.Service as Service
import Glean.Database.Types
import qualified Glean.Backend as Backend
import Glean.Types as Thrift

data State = State
  { fb303State :: Fb303State
  , stEnv :: Env
  }

handler :: State -> GleanServiceCommand a -> IO a
handler State{..} req =
  let backend = Backend.LoggingBackend stEnv in -- log (most) requests
  case req of
    -- DEPRECATED
    Service.GetPredicates repo refs -> do
      info <- Backend.getSchemaInfo backend repo
      let by_ref = HashMap.fromList
            [(ref,id) | (id,ref) <- Map.toList $ schemaInfo_predicateIds info]
      return [HashMap.lookupDefault 0 ref by_ref | ref <- refs]

    Service.GetSchemaInfo repo ->
      Backend.getSchemaInfo backend repo

    Service.ValidateSchema req -> Backend.validateSchema backend req

    Service.SendBatch cbatch -> Backend.enqueueBatch backend cbatch

    Service.FinishBatch handle -> Backend.pollBatch backend handle

    Service.SendJsonBatch repo batch ->
      Backend.enqueueJsonBatch backend repo batch

    Service.KickOff rq -> Backend.kickOffDatabase backend rq

    Service.UpdateProperties repo set unset ->
      Backend.updateProperties backend repo set unset

    Service.GetWork rq -> Backend.getWork backend rq

    Service.WorkCancelled rq -> Backend.workCancelled backend rq

    Service.WorkHeartbeat rq -> Backend.workHeartbeat backend rq

    Service.WorkFinished rq -> Backend.workFinished backend rq

    Service.QueryFact repo id ->
      fromMaybe (Thrift.Fact 0 mempty mempty) <$>
        Backend.queryFact backend repo id

    Service.FirstFreeId repo -> Backend.firstFreeId backend repo

    Service.FactIdRange repo -> Backend.factIdRange backend repo

    Service.UserQueryFacts repo req ->
      Backend.userQueryFacts backend repo req

    Service.UserQuery repo query ->
      Backend.userQuery backend repo query

    Service.DerivePredicate repo pred ->
      Backend.derivePredicate backend repo pred

    Service.PollDerivation handle ->
      Backend.pollDerivation backend handle

    Service.PredicateStats repo ->
      Backend.predicateStats backend repo

    Service.ListDatabases l ->
      Backend.listDatabases backend l

    Service.GetDatabase repo -> Backend.getDatabase backend repo

    Service.DeleteDatabase repo -> Backend.deleteDatabase backend repo

    Service.Restore loc -> Backend.restoreDatabase backend loc

    SuperFacebookService c -> fb303Handler fb303State c

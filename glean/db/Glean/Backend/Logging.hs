{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Backend.Logging
  ( LoggingBackend(..)
  ) where

import Control.Exception
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Sum(getSum, Sum))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import TextShow (showt)

import Util.Logger

import Glean.Backend.Local ()
import Glean.Logger.Server as Logger
import qualified Glean.Database.List as Database
import qualified Glean.Database.Types as Database
import Glean.Logger
import qualified Glean.Types as Thrift
import Glean.Util.Time

import Glean.Backend.Types


-- | A logging wrapper for Env. We do it this way because some backend
-- calls invoke other backend calls, and we only want to log the
-- outermost one. For example, userQuery will call queryFact a *lot*,
-- and it would be too expensive to log each and every call to
-- queryFact.
newtype LoggingBackend = LoggingBackend Database.Env

instance Backend LoggingBackend where
  queryFact (LoggingBackend env) repo id =
    loggingAction (runLogRepo "queryFact" env repo) (const mempty) $
      queryFact env repo id
  firstFreeId (LoggingBackend env) repo =
    loggingAction (runLogRepo "firstFreeId" env repo) (const mempty) $
      firstFreeId env repo
  factIdRange (LoggingBackend env) repo =
    loggingAction (runLogRepo "factIdRange" env repo) (const mempty) $
      factIdRange env repo
  getSchemaInfo (LoggingBackend env) repo req =
    loggingAction (runLogRepo "getSchemaInfo" env repo) (const mempty) $
      getSchemaInfo env repo req
  validateSchema (LoggingBackend env) req =
    loggingAction (runLogCmd "validateSchema" env) (const mempty) $
      validateSchema env req
  predicateStats (LoggingBackend env) repo opts =
    loggingAction (runLogRepo "predicateStats" env repo) (const mempty) $
      predicateStats env repo opts
  userQueryFacts (LoggingBackend env) repo req =
    loggingAction (runLogQueryFacts "userQueryFacts" env repo req)
      logQueryResults $
        userQueryFacts env repo req
  userQuery (LoggingBackend env) repo req =
    loggingAction (runLogQuery "userQuery" env repo req) logQueryResults $
      userQuery env repo req
  userQueryBatch (LoggingBackend env) repo reqBatch =
    loggingAction
      (runLogQueryBatch "userQueryBatch" env repo reqBatch)
      logQueryResultsOrException
      (userQueryBatch env repo reqBatch)

  deriveStored (LoggingBackend env) log repo q =
    loggingAction
      (runLogDerivePredicate "deriveStored" env repo q)
      (const mempty)
      (deriveStored env (runLogDerivationResult env log repo q) repo q)

  listDatabases (LoggingBackend env) req =
    loggingAction (runLogCmd "listDatabases" env) (const mempty) $
      Database.listDatabases env req
  getDatabase (LoggingBackend env) repo =
    loggingAction (runLogRepo "getDatabase" env repo) (const mempty) $
      getDatabase env repo

  kickOffDatabase (LoggingBackend env) rq =
    loggingAction
      (runLogKickOff "kickOff" env rq)
      (const mempty) $
      kickOffDatabase env rq
  finalizeDatabase (LoggingBackend env) repo =
    loggingAction
      (runLogRepo "finalizeDatabase" env repo)
      (const mempty) $
      finalizeDatabase env repo
  updateProperties (LoggingBackend env) repo set unset =
    loggingAction
      (runLogRepo "updateProperties" env repo)
      (const mempty) $
      updateProperties env repo set unset
  getWork (LoggingBackend env) rq =
    loggingAction (runLogCmd "getWork" env) (const mempty) $
      getWork env rq
  workCancelled (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workCancelled" env
        $ Thrift.work_repo
        $ Thrift.workCancelled_work rq)
      (const mempty) $
      workCancelled env rq
  workHeartbeat (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workHeartbeat" env
        $ Thrift.work_repo
        $ Thrift.workHeartbeat_work rq)
      (const mempty) $
      workHeartbeat env rq
  workFinished (LoggingBackend env) rq =
    loggingAction
      (runLogRepo "workFinished" env
        $ Thrift.work_repo
        $ Thrift.workFinished_work rq)
      (const mempty) $
      workFinished env rq

  completePredicates_ (LoggingBackend env) repo =
    loggingAction
       (runLogRepo "completePredicates" env repo)
       (const mempty) $
       completePredicates_ env repo

  restoreDatabase (LoggingBackend env) loc =
    loggingAction (runLogCmd "restoreDatabase" env) (const mempty) $
      restoreDatabase env loc
  deleteDatabase (LoggingBackend env) repo =
    loggingAction (runLogRepo "deleteDatabase" env repo) (const mempty) $
      deleteDatabase env repo
  enqueueBatch (LoggingBackend env) cbatch =
    loggingAction
      (runLogRepo "enqueueBatch" env (Thrift.computedBatch_repo cbatch))
      (const mempty) $
        enqueueBatch env cbatch
  enqueueJsonBatch (LoggingBackend env) repo batch =
    loggingAction (runLogRepo "enqueueJsonBatch" env repo) (const mempty) $
      enqueueJsonBatch env repo batch
  pollBatch (LoggingBackend env) handle =
    loggingAction (runLogCmd "pollBatch" env) (const mempty) $
      pollBatch env handle
  displayBackend (LoggingBackend b) = displayBackend b
  hasDatabase (LoggingBackend b) repo = hasDatabase b repo
  schemaId (LoggingBackend b) = schemaId b
  usingShards (LoggingBackend b) = usingShards b
  initGlobalState (LoggingBackend b) = initGlobalState b

runLogKickOff
  :: Text
  -> Database.Env
  -> Thrift.KickOff
  -> GleanServerLog
  -> IO ()
runLogKickOff cmd env Thrift.KickOff{..} log =
  runLogRepo cmd env kickOff_repo $ log <> schemaId
  where
  schemaId = maybe mempty Logger.SetSchemaId $
    HashMap.lookup "glean.schema_id" kickOff_properties

runLogQueryFacts
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQueryFacts
  -> GleanServerLog -> IO ()
runLogQueryFacts cmd env repo Thrift.UserQueryFacts{..} log =
  runLogRepo cmd env repo $ log
    <> maybe mempty logQueryOptions userQueryFacts_options
    <> maybe mempty logQueryClientInfo userQueryFacts_client_info
    <> maybe mempty (Logger.SetSchemaId . Thrift.unSchemaId)
        userQueryFacts_schema_id

runLogQuery
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQuery
  -> GleanServerLog
  -> IO ()
runLogQuery cmd env repo Thrift.UserQuery{..} log = do
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.SetQuery
        (Text.decodeUtf8With Text.lenientDecode userQuery_query)
    , Logger.SetPredicate userQuery_predicate
    , maybe mempty (Logger.SetPredicateVersion . fromIntegral)
        userQuery_predicate_version
    , maybe mempty (Logger.SetSchemaVersion . fromIntegral)
        userQuery_schema_version
    , maybe mempty (Logger.SetSchemaId . Thrift.unSchemaId)
        userQuery_schema_id
    , maybe mempty logQueryOptions userQuery_options
    , maybe mempty logQueryClientInfo userQuery_client_info
    ]

runLogQueryBatch
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.UserQueryBatch
  -> GleanServerLog
  -> IO ()
runLogQueryBatch cmd env repo Thrift.UserQueryBatch{..} log =
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.SetQuery $ case userQueryBatch_queries of
        [] -> "0 batched queries"
        q:rest -> Text.unlines $
          Text.decodeUtf8With Text.lenientDecode q :
          [" + " <> showt n <> " batched queries"
          | let n = length rest
          , n > 1
          ]
    , Logger.SetPredicate userQueryBatch_predicate
    , maybe mempty (Logger.SetPredicateVersion . fromIntegral)
        userQueryBatch_predicate_version
    , maybe mempty (Logger.SetSchemaVersion . fromIntegral)
        userQueryBatch_schema_version
    , maybe mempty (Logger.SetSchemaId . Thrift.unSchemaId)
        userQueryBatch_schema_id
    , maybe mempty logQueryOptions userQueryBatch_options
    , maybe mempty logQueryClientInfo userQueryBatch_client_info
    ]

logQueryOptions :: Thrift.UserQueryOptions -> GleanServerLog
logQueryOptions Thrift.UserQueryOptions{..} = mconcat
  [ Logger.SetNoBase64Binary userQueryOptions_no_base64_binary
  , Logger.SetExpandResults userQueryOptions_expand_results
  , Logger.SetRecursive userQueryOptions_recursive
  , maybe mempty (Logger.SetMaxResults . fromIntegral)
      userQueryOptions_max_results
  , Logger.SetSyntax $ case userQueryOptions_syntax of
      Thrift.QuerySyntax_JSON -> "JSON"
      Thrift.QuerySyntax_ANGLE -> "Angle"
  , maybe mempty
      ( Logger.SetRequestContinuationSize
      . ByteString.length
      . Thrift.userQueryCont_continuation
      )
      userQueryOptions_continuation
  ]

logQueryClientInfo :: Thrift.UserQueryClientInfo -> GleanServerLog
logQueryClientInfo Thrift.UserQueryClientInfo{..} = mconcat
  [ maybe mempty Logger.SetClientUnixname userQueryClientInfo_unixname
  , Logger.SetClientApplication userQueryClientInfo_application
  , Logger.SetClientName userQueryClientInfo_name
  ]

logQueryResultsOrException
  :: [Thrift.UserQueryResultsOrException] -> GleanServerLog
logQueryResultsOrException results = mconcat
  [
    Logger.SetResults $ getSum $ foldMap (Sum . countQueryResults)
      [ r | Thrift.UserQueryResultsOrException_results r <- results]
  ]

logQueryResults :: Thrift.UserQueryResults -> GleanServerLog
logQueryResults it@Thrift.UserQueryResults{..} = mconcat
  [ Logger.SetResults $ countQueryResults it
  , Logger.SetTruncated (isJust userQueryResults_continuation)
  , maybe mempty logQueryStats userQueryResults_stats
  , maybe mempty Logger.SetType userQueryResults_type
  , maybe mempty
      ( Logger.SetResponseContinuationSize
      . ByteString.length
      . Thrift.userQueryCont_continuation
      )
      userQueryResults_continuation
  ]

countQueryResults :: Thrift.UserQueryResults -> Int
countQueryResults Thrift.UserQueryResults{..} =
  case userQueryResults_results of
    Thrift.UserQueryEncodedResults_bin bin ->
      Map.size (Thrift.userQueryResultsBin_facts bin)
    Thrift.UserQueryEncodedResults_json json ->
      length (Thrift.userQueryResultsJSON_facts json)
    Thrift.UserQueryEncodedResults_compact compact ->
      length (Thrift.userQueryResultsCompact_facts compact)
    _ ->
      length userQueryResults_facts

logQueryStats :: Thrift.UserQueryStats -> GleanServerLog
logQueryStats Thrift.UserQueryStats{..} = mconcat
  [ Logger.SetResults (fromIntegral userQueryStats_result_count)
  , Logger.SetFacts (fromIntegral userQueryStats_num_facts)
  , maybe mempty (Logger.SetBytecodeSize . fromIntegral)
      userQueryStats_bytecode_size
  , maybe mempty (Logger.SetCompileTimeUs . fromIntegral . (`quot` 1000))
      userQueryStats_compile_time_ns
  , maybe mempty (Logger.SetExecuteTimeUs . fromIntegral . (`quot` 1000))
      userQueryStats_execute_time_ns
  ]

runLogDerivePredicate
  :: Text
  -> Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> GleanServerLog
  -> IO ()
runLogDerivePredicate cmd env repo Thrift.DerivePredicateQuery {..} log =
  runLogRepo cmd env repo $ mconcat
    [ log
    , Logger.SetPredicate derivePredicateQuery_predicate
    , maybe mempty (Logger.SetPredicateVersion . fromIntegral)
        derivePredicateQuery_predicate_version
    , maybe mempty logQueryClientInfo derivePredicateQuery_client_info
    ]

runLogDerivationResult
  :: Database.Env
  -> LogDerivationResult
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> Either (DiffTimePoints, SomeException) Thrift.UserQueryStats
  -> IO ()
runLogDerivationResult env log repo Thrift.DerivePredicateQuery{..} res = do
  log res
  runLogRepo "deriveStored(completed)" env repo $ mconcat
    [ Logger.SetPredicate derivePredicateQuery_predicate
    , maybe mempty (Logger.SetPredicateVersion . fromIntegral)
        derivePredicateQuery_predicate_version
    , maybe mempty logQueryClientInfo derivePredicateQuery_client_info
    , case res of
        Left (_,err) -> failureLog err
        Right stats -> successLog <> logQueryStats stats
    , timeLog $ toDiffSeconds $ case res of
        Left (duration, _) -> duration
        Right Thrift.UserQueryStats{..} ->
          nanoseconds (fromIntegral userQueryStats_elapsed_ns)
    ]

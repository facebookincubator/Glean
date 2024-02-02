{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Backend.Retry
  ( RetryWritesBackend(..)
  , backendRetryWrites
  , RetryReadsBackend(..)
  , backendRetryReads
  ) where

import Glean.Backend.Types
import Glean.Util.RetryChannelException
import Glean.Util.Some

data RetryWritesBackend = RetryWritesBackend RetryPolicy (Some Backend)

-- | Make a 'Backend' that will retry all write-related operations
-- according to the given 'RetryPolicy'
backendRetryWrites :: Backend b => b -> RetryPolicy -> RetryWritesBackend
backendRetryWrites backend policy = RetryWritesBackend policy (Some backend)

instance Backend RetryWritesBackend where
  queryFact (RetryWritesBackend _ backend) = queryFact backend
  factIdRange (RetryWritesBackend _ backend) = factIdRange backend
  getSchemaInfo (RetryWritesBackend _ backend) = getSchemaInfo backend
  validateSchema (RetryWritesBackend _ backend) = validateSchema backend
  predicateStats (RetryWritesBackend _ backend) = predicateStats backend
  listDatabases (RetryWritesBackend _ backend) = listDatabases backend
  getDatabase (RetryWritesBackend _ backend) = getDatabase backend
  userQueryFacts (RetryWritesBackend _ backend) = userQueryFacts backend
  userQuery (RetryWritesBackend _ backend) = userQuery backend
  userQueryBatch (RetryWritesBackend _ backend) = userQueryBatch backend
  deriveStored (RetryWritesBackend _ backend) = deriveStored backend

  kickOffDatabase (RetryWritesBackend policy backend) kickOff =
    retryChannelExceptions policy $ kickOffDatabase backend kickOff
  finalizeDatabase (RetryWritesBackend policy backend) repo =
    retryChannelExceptions policy $ finalizeDatabase backend repo
  updateProperties (RetryWritesBackend policy backend) repo props vals =
    retryChannelExceptions policy $ updateProperties backend repo props vals
  getWork (RetryWritesBackend policy backend) get =
    retryChannelExceptions policy $ getWork backend get
  workCancelled (RetryWritesBackend policy backend) work =
    retryChannelExceptions policy $ workCancelled backend work
  workHeartbeat (RetryWritesBackend policy backend) work =
    retryChannelExceptions policy $ workHeartbeat backend work
  workFinished (RetryWritesBackend policy backend) work =
    retryChannelExceptions policy $ workFinished backend work
  completePredicates_ (RetryWritesBackend policy backend) repo preds =
    retryChannelExceptions policy $ completePredicates_ backend repo preds

  restoreDatabase (RetryWritesBackend _ backend) = restoreDatabase backend
  deleteDatabase (RetryWritesBackend _ backend) = deleteDatabase backend

  enqueueBatch (RetryWritesBackend policy backend) batch =
    retryChannelExceptions policy $ enqueueBatch backend batch
  enqueueJsonBatch (RetryWritesBackend policy backend) repo batch =
    retryChannelExceptions policy $ enqueueJsonBatch backend repo batch
  pollBatch (RetryWritesBackend policy backend) handle =
    retryChannelExceptions policy $ pollBatch backend handle

  displayBackend (RetryWritesBackend _ backend) = displayBackend backend
  hasDatabase (RetryWritesBackend _ backend) = hasDatabase backend
  schemaId (RetryWritesBackend _ backend) = schemaId backend
  usingShards (RetryWritesBackend _ backend) = usingShards backend
  initGlobalState (RetryWritesBackend _ backend) = initGlobalState backend

data RetryReadsBackend = RetryReadsBackend RetryPolicy (Some Backend)

-- | Make a 'Backend' that will retry all read-related operations
-- according to the given 'RetryPolicy'
backendRetryReads :: Backend b => b -> RetryPolicy -> RetryReadsBackend
backendRetryReads backend policy = RetryReadsBackend policy (Some backend)

instance Backend RetryReadsBackend where
  queryFact (RetryReadsBackend policy backend) repo id =
    retryChannelExceptions policy $ queryFact backend repo id
  factIdRange (RetryReadsBackend policy backend) repo =
    retryChannelExceptions policy $ factIdRange backend repo
  getSchemaInfo (RetryReadsBackend policy backend) repo opts =
    retryChannelExceptions policy $ getSchemaInfo backend repo opts
  validateSchema (RetryReadsBackend policy backend) req =
    retryChannelExceptions policy $ validateSchema backend req
  predicateStats (RetryReadsBackend policy backend) repo opts =
    retryChannelExceptions policy $ predicateStats backend repo opts
  listDatabases (RetryReadsBackend policy backend) req =
    retryChannelExceptions policy $ listDatabases backend req
  getDatabase (RetryReadsBackend policy backend) repo =
    retryChannelExceptions policy $ getDatabase backend repo
  userQueryFacts (RetryReadsBackend policy backend) repo req =
    retryChannelExceptions policy $ userQueryFacts backend repo req
  userQuery (RetryReadsBackend policy backend) repo req =
    retryChannelExceptions policy $ userQuery backend repo req
  userQueryBatch (RetryReadsBackend policy backend) repo req =
    retryChannelExceptions policy $ userQueryBatch backend repo req
  deriveStored (RetryReadsBackend _ backend) = deriveStored backend

  kickOffDatabase (RetryReadsBackend _ backend) = kickOffDatabase backend
  finalizeDatabase (RetryReadsBackend _ backend) = finalizeDatabase backend
  updateProperties (RetryReadsBackend _ backend) = updateProperties backend
  getWork (RetryReadsBackend _ backend) = getWork backend
  workCancelled (RetryReadsBackend _ backend) = workCancelled backend
  workHeartbeat (RetryReadsBackend _ backend) = workHeartbeat backend
  workFinished (RetryReadsBackend _ backend) = workFinished backend
  completePredicates_ (RetryReadsBackend _ backend) =
    completePredicates_ backend

  restoreDatabase (RetryReadsBackend _ backend) = restoreDatabase backend
  deleteDatabase (RetryReadsBackend _ backend) = deleteDatabase backend

  enqueueBatch (RetryReadsBackend _ backend) = enqueueBatch backend
  enqueueJsonBatch (RetryReadsBackend _ backend) = enqueueJsonBatch backend
  pollBatch (RetryReadsBackend _ backend) = pollBatch backend

  displayBackend (RetryReadsBackend _ backend) = displayBackend backend
  hasDatabase (RetryReadsBackend _ backend) = hasDatabase backend
  schemaId (RetryReadsBackend _ backend) = schemaId backend
  usingShards (RetryReadsBackend _ backend) = usingShards backend
  initGlobalState (RetryReadsBackend _ backend) = initGlobalState backend

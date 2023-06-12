{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Backend.Retry
  ( RetryBackend(..)
  , backendRetryWrites
  ) where

import Glean.Backend.Types
import Glean.Util.RetryChannelException
import Glean.Util.Some

data RetryBackend = RetryBackend RetryPolicy (Some Backend)

-- | Make a 'Backend' that will retry all write-related operations
-- according to the given 'RetryPolicy'
backendRetryWrites :: Backend b => b -> RetryPolicy -> RetryBackend
backendRetryWrites backend policy = RetryBackend policy (Some backend)

instance Backend RetryBackend where
  queryFact (RetryBackend _ backend) = queryFact backend
  factIdRange (RetryBackend _ backend) = factIdRange backend
  getSchemaInfo (RetryBackend _ backend) = getSchemaInfo backend
  validateSchema (RetryBackend _ backend) = validateSchema backend
  predicateStats (RetryBackend _ backend) = predicateStats backend
  listDatabases (RetryBackend _ backend) = listDatabases backend
  getDatabase (RetryBackend _ backend) = getDatabase backend
  userQueryFacts (RetryBackend _ backend) = userQueryFacts backend
  userQuery (RetryBackend _ backend) = userQuery backend
  userQueryBatch (RetryBackend _ backend) = userQueryBatch backend
  deriveStored (RetryBackend _ backend) = deriveStored backend

  kickOffDatabase (RetryBackend policy backend) kickOff =
    retryChannelExceptions policy $ kickOffDatabase backend kickOff
  finalizeDatabase (RetryBackend policy backend) repo =
    retryChannelExceptions policy $ finalizeDatabase backend repo
  updateProperties (RetryBackend policy backend) repo props vals =
    retryChannelExceptions policy $ updateProperties backend repo props vals
  getWork (RetryBackend policy backend) get =
    retryChannelExceptions policy $ getWork backend get
  workCancelled (RetryBackend policy backend) work =
    retryChannelExceptions policy $ workCancelled backend work
  workHeartbeat (RetryBackend policy backend) work =
    retryChannelExceptions policy $ workHeartbeat backend work
  workFinished (RetryBackend policy backend) work =
    retryChannelExceptions policy $ workFinished backend work
  completePredicates_ (RetryBackend policy backend) repo =
    retryChannelExceptions policy $ completePredicates_ backend repo

  restoreDatabase (RetryBackend _ backend) = restoreDatabase backend
  deleteDatabase (RetryBackend _ backend) = deleteDatabase backend

  enqueueBatch (RetryBackend policy backend) batch =
    retryChannelExceptions policy $ enqueueBatch backend batch
  enqueueJsonBatch (RetryBackend policy backend) repo batch =
    retryChannelExceptions policy $ enqueueJsonBatch backend repo batch
  pollBatch (RetryBackend policy backend) handle =
    retryChannelExceptions policy $ pollBatch backend handle

  displayBackend (RetryBackend _ backend) = displayBackend backend
  hasDatabase (RetryBackend _ backend) = hasDatabase backend
  schemaId (RetryBackend _ backend) = schemaId backend
  usingShards (RetryBackend _ backend) = usingShards backend
  initGlobalState (RetryBackend _ backend) = initGlobalState backend

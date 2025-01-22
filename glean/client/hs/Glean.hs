{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- | Interface providing everything that clients of a Glean server should need.
-- To use a local database store, see "Glean.LocalOrRemote".
--
-- Client code should look like:
--
-- > import Glean
-- > import Glean.Remote
-- > import Glean.Util.ConfigProvider
-- > import Glean.Impl.ConfigProvider
-- > import Glean.Schema.Builtin.Types (schema_id)
-- > import Util.EventBase
-- >
-- > main :: IO ()
-- > main =
-- >   withConfigOptions options $ \(service, cfgOpts) ->
-- >   withEventBaseDataplane $ \evb ->
-- >   withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
-- >   withRemoteBackend evb cfgAPI service (Just schema_id) $ \backend -> do
-- >     ...
--
module Glean
  (
  -- * Clients interact with Glean via a Backend
    Backend(..)
  , UseShards(..)
  , ThriftSource
  , ClientConfig(..)
  , ListDatabases(..)
  , ListDatabasesResult(..)
  , KickOff(..)
  , KickOffFill(..)
  , KickOffResponse(..)
  , Database(..)
  , DatabaseStatus(..)
  , Work(..)
  , WorkAvailable(..)
  , WorkUnavailable(..)
  , WorkHeartbeat(..)
  , GetWork(..)
  , GetWorkResponse(..)
  , AbortWork(..)
  , WorkCancelled(..)
  , WorkFinished(..)
  , GetSchemaInfo(..)
  , SchemaId(..)
  , SchemaInfo(..)
  , UnknownDatabase(..)
  , Outcome(..)
  , Success(..)
  , Failure(..)
  , Pruned(..)
  , Stacked(..)
  , Dependencies(..)
  , PosixEpochTime(..)
  , StackedDbOpts(..)
  , LogDerivationResult
  , SendJsonBatchOptions(..)
  , Subst(..)
  , DatabaseNotIncomplete(..)
  , WrongHandle(..)
  , FactIdRange(..)
  , Tasks
  , Task(..)
  , TaskState(..)
  , TaskRunning(..)
  , TaskWaiting(..)
  , TaskFinished(..)
  , ParcelState(..)
  , ParcelRunning(..)
  , ParcelWaiting(..)
  , ParcelFinished(..)
  , GetDatabaseResult(..)

  -- * Repositories
  , Repo(..)
  , getLatestRepo
  , NoDatabase(..)
  , showRepo
  , showRepoSep
  , repoToText
  , repoToTextSep
  , readRepo
  , parseRepo
  , parseRepoText
  , parseRepoTextSep

  -- * Queries
  , Query

  -- ** Performing queries
  , runQuery
  , runQuery_
  , runQueryEach
  , runQueryEachBatch
  , runQueryMapPages_
  , BadQuery(..)
  , allFacts

  -- ** Query using Angle syntax
  , angle
  , angleData
  , encodeTextForAngle

  -- ** Query modifiers
  , keys
  , recursive
  , limit
  , limitBytes
  , limitTime
  , expanding
  , store

  -- ** Inspecting Query
  , displayQuery

  -- * Haxl: high-performance query API
  , Haxl
  , runHaxl
  , runHaxlWithWrites
  , haxlRepo
  , search
  , search_
  , searchWith
  , QueryResult(..)
  , count
  , get
  , getRec
  , getKey
  , getKeyRec
  , getOfId
  , getRecOfId
  , getKeyOfId
  , getKeyRecOfId
  , keyOf
  , getFirstResult
  -- ** Error handling
  , trySyncHaxl

  -- * Manipulating predicates and facts
  , Predicate(..)
  , Nat(..)
  , toNat, fromNat
  , Byte(..)
  , Id
  , justId
  , IdOf(..)
  , Fid(..)   -- TODO: Fid isn't very useful, might want to tidy up the API here
  , PredicateRef(..)
  , predicateRef
  , SourceRef(..) -- used in Glean.Util.Derive
  , showRef
  , parseRef

  -- * Writing
  , fillDatabase
  , create
  , finish
  , finalize
  , completePredicates
  , CompletePredicates(..)
  , CompleteDerivedPredicate(..)
  , CompleteAxiomPredicates(..)
  , basicWriter
  , FactBuilder
  , makeFact
  , makeFact_
  , makeFactV
  , makeFactV_
  , NewFact(..)

  -- ** Lower-level write API
  , withSender
  , SchemaPredicates
  , Sender
  , SendAndRebaseQueue
  , sendAndRebaseQueueOptions
  , SendAndRebaseQueueSettings(..)
  , sendQueueOptions
  , SendQueueSettings(..)
  , SendQueueEvent(..)
  , withWriter
  , withWriters
  , Writer
  , WriterSettings(..)
  , WriterEvent(..)
  , writerOptions
  , writeFacts
  , writeSendAndRebaseQueue
  , withSendAndRebaseQueue
  , writeSendQueue
  , writeSendQueueJson
  , withSendQueue
  , SendQueue

  -- ** Writing binary
  , sendBatch

  -- ** Writing JSON
  , sendJsonBatch
  , SendJsonBatch(..)
  , JsonFactBatch(..)

  -- * Misc
  , showUserQueryStats
  ) where

import Glean.Backend.Types
import Glean.Angle.Lexer
import Glean.Angle.Types
import Glean.Schema.Util (showRef, parseRef)

import Glean.Impl.ConfigProvider ()
  -- re-export instance ConfigProvider ConfigAPI, so that clients
  -- don't have to import this.
import Glean.Haxl
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import Glean.Repo
import Glean.Repo.Text
import Glean.Typed
import Glean.Types
import Glean.Write.Async
import Glean.Write.Options
import Glean.Write.SendBatch
import Glean.Write.SendAndRebaseQueue
import Glean.Write.SendQueue
import Glean.Util.ThriftSource (ThriftSource)
import Glean.ClientConfig.Types

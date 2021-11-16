{-
  Copyright (c) Facebook, Inc. and its affiliates.
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
-- > import Glean.Util.ConfigProvider
-- > import Glean.Impl.ConfigProvider
-- > import Util.EventBase
-- >
-- > main :: IO ()
-- > main =
-- >   withConfigOptions options $ \(service, cfgOpts) ->
-- >   withEventBaseDataplane $ \evb ->
-- >   withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
-- >   withRemoteBackend evb cfgAPI service $ \backend -> do
-- >     ...
--
module Glean
  (
  -- * Connecting to a Glean server
    options
  , withRemoteBackend
  , withRemoteBackendSettings
  , Settings
  , setService
  , setNoShards
  , setTimeout
  , defaultClientConfigSource

  -- * Backend, for raw interaction with the server
  , Backend(..)
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
  , SchemaInfo(..)
  , UnknownDatabase(..)
  , Outcome(..)
  , Success(..)
  , Failure(..)
  , Pruned(..)
  , Dependencies(..)
  , PosixEpochTime(..)
  , StackedDbOpts(..)
  , LogDerivationResult

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
  , dbShard

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

  -- ** Query using Thrift query types
  , ThriftQuery
  , QueryResult
  , query

  -- ** Query modifiers
  , keys
  , recursive
  , limit
  , limitBytes
  , limitTime
  , store

  -- ** Inspecting Query
  , queryPredicate
  , displayQuery

  -- * Haxl: high-performance query API
  , Haxl
  , HaxlQuery
  , runHaxl
  , runHaxlWithWrites
  , search
  , search_
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
  -- ** advanced Store
  , initGlobalState
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
  , SumBranches(..)
  , PredicateQuery(..)
  , SumQuery(..)
  , QueryOf
  , ToQuery(..)

  -- * Writing
  , fillDatabase
  , finalize
  , completePredicates
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
  , sendAndRebaseQueueOptions
  , SendAndRebaseQueueSettings(..)
  , sendQueueOptions
  , SendQueueSettings(..)
  , withWriter
  , Writer
  , WriterSettings(..)
  , writerOptions
  , writeFacts
  , writeSendAndRebaseQueue
  , withSendAndRebaseQueue
  , writeSendQueue
  , withSendQueue
  , SendQueue

  -- ** Writing binary
  , sendBatch

  -- ** Writing JSON
  , sendJsonBatch
  , SendJsonBatch(..)
  , JsonFactBatch(..)
  ) where

import Glean.Backend.Remote hiding (dbShard, completePredicates)
import Glean.Angle.Lexer

import Glean.Haxl
import Glean.Query.Thrift
import Glean.Repo
import Glean.Repo.Text
import Glean.Typed
import Glean.Types
import Glean.Write
import Glean.Write.Async
import Glean.Write.Options
import Glean.Write.SendBatch
import Glean.Write.SendAndRebaseQueue
import Glean.Write.SendQueue
import Glean.Util.ThriftSource (ThriftSource)
import Glean.ClientConfig.Types

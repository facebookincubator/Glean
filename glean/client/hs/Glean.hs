--
-- | Interface providing everything that read-only applications of
-- Glean should need.
--
module Glean
  (
  -- * Connecting to a Glean server
    options
  , optionsLong
  , withBackendWithDefaultOptions
  , defaultClientConfigSource

  -- * types for backend
  , Service(..)
  , Config(..)
  , Backend(..)
  , LocalOrRemote(..)
  , UseShards(..)
  , LoggingBackend(..)
  , BackendKind(..)
  , ThriftSource
  , ClientConfig(..)
  , ListDatabases(..)
  , ListDatabasesResult(..)
  , KickOff(..)
  , KickOffFill(..)
  , Database(..)

  -- * Repositories
  , Repo(..)
  , getLatestRepo
  , NoDatabase(..)
  , parseRepo
  , showRepo
  , displayRepo
  , dbShard

  -- * Queries
  , Query

  -- ** Performing queries
  , runQuery
  , runQuery_
  , runQueryEach
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
  , basicWriter
  , FactBuilder
  , makeFact
  , makeFact_
  , NewFact

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

  -- ** Misc
  , validate
  , Validate(..)
  ) where

import Glean.Backend hiding (dbShard)
import Glean.Database.Config (Config(..))
import Glean.Database.Validate
import Glean.Angle.Lexer

import Glean.Haxl
import Glean.Query.Thrift
import Glean.Repo
import Glean.Typed
import Glean.Types
import Glean.Write.Async
import Glean.Write.Options
import Glean.Write.SendBatch
import Glean.Write.SendAndRebaseQueue
import Glean.Write.SendQueue
import Glean.Util.ThriftSource (ThriftSource)
import Glean.ClientConfig.Types

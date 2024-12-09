{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Logger.Server (
    GleanServerLog(..),
    GleanServerLogger(..),
    NullGleanServerLogger(..),
  ) where

import Data.Text (Text)

class GleanServerLogger l where
  runLog :: l -> GleanServerLog -> IO ()

data GleanServerLog
  = GleanServerLogEmpty
  | GleanServerLogAnd GleanServerLog GleanServerLog
  | SetSuccess Bool
  | SetError Text
  | SetTimeElapsed Double
  | SetAllocatedBytes Int
  | SetMethod Text
  | SetWeight Int
  | SetRepoName Text
  | SetRepoHash Text
  | SetQuery Text
  | SetPredicate Text
  | SetPredicateVersion Int
  | SetSchemaVersion Int
  | SetNoBase64Binary Bool
  | SetExpandResults Bool
  | SetRecursive Bool
  | SetMaxResults Int
  | SetTruncated Bool
  | SetResults Int
  | SetFacts Int
  | SetFullScans [Text]
  | SetSyntax Text
  | SetType Text
  | SetBytecodeSize Int
  | SetCompileTimeUs Int
  | SetExecuteTimeUs Int
  | SetClientUnixname Text
  | SetClientApplication Text
  | SetClientName Text
  | SetRequestContinuationSize Int
  | SetResponseContinuationSize Int
  | SetSchemaId Text
  | SetBatchFactsSize Int
  | SetBatchFactsCount Int
  | SetBatchOwnedSize Int
  | SetBatchDependenciesSize Int

instance Semigroup GleanServerLog where
  a <> b = GleanServerLogAnd a b

instance Monoid GleanServerLog where
  mempty = GleanServerLogEmpty

data NullGleanServerLogger = NullGleanServerLogger

instance GleanServerLogger NullGleanServerLogger where
  runLog _ _ = return ()

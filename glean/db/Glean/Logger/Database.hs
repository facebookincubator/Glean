{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Logger.Database (
    GleanDatabaseLog(..),
    GleanDatabaseLogger(..),
    NullGleanDatabaseLogger(..),
  ) where

import Data.Text (Text)

class GleanDatabaseLogger l where
  runLog :: l -> GleanDatabaseLog -> IO ()

data GleanDatabaseLog
  = GleanDatabaseLogEmpty
  | GleanDatabaseLogAnd GleanDatabaseLog GleanDatabaseLog
  | SetRepoName Text
  | SetRepoHash Text
  | SetPredicateCount Int
  | SetUploadDestination Text
  | SetPredicateName Text
  | SetPredicateVersion Int
  | SetPredicateSize Int
  | SetMetric Text
  | SetCount Int
  | SetSize Int

instance Semigroup GleanDatabaseLog where
  a <> b = GleanDatabaseLogAnd a b

instance Monoid GleanDatabaseLog where
  mempty = GleanDatabaseLogEmpty

data NullGleanDatabaseLogger = NullGleanDatabaseLogger

instance GleanDatabaseLogger NullGleanDatabaseLogger where
  runLog _ _ = return ()

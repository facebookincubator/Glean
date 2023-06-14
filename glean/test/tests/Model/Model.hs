{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-
  This module contains types for modelling the internal state of the Glean
  server. The model considers the following items:
  - config
  - catalog: local DBs, restoring DBs, etc
  - shard assignment
  - time
-}
{-# LANGUAGE DerivingStrategies #-}

module Model.Model where

import Control.Concurrent.STM (STM, readTVar)
import Data.Default (def)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int64)
import Data.Text (Text)
import Glean.Database.Catalog (
  EntriesF (..),
  Entry,
  EntryF (Entry, entryRepo),
 )
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter (
  Item (..),
  ItemStatus (ItemMissing),
 )
import Glean.Internal.Types (Meta (..))
import Glean.ServerConfig.Types (
  DatabaseBackupPolicy,
  DatabaseClosePolicy,
  DatabaseRestorePolicy,
  DatabaseRetentionPolicy,
 )
import Glean.Types (
  PosixEpochTime (PosixEpochTime),
  Repo,
 )

{- | The number of shards is also the number of distinct repo hashes, and
   the zero time value (to ensure that every DB was completed in the past)
-}
numberOfShards :: Int
numberOfShards = 10

zeroTime :: PosixEpochTime
zeroTime = PosixEpochTime $ fromIntegral numberOfShards

addTime :: Int64 -> PosixEpochTime -> PosixEpochTime
addTime t (PosixEpochTime n) = PosixEpochTime (n + t)

--------------------------------------------------------------------------------

data Model = Model
  { modelRetentionPolicy :: DatabaseRetentionPolicy
  , modelRestorePolicy :: DatabaseRestorePolicy
  , modelBackupPolicy :: DatabaseBackupPolicy
  , modelBackupDir :: String
  , modelClosePolicy :: DatabaseClosePolicy
  , modelShardAssignment :: HashSet ShardId
  , modelEntries :: EntriesF (EntryF Identity)
  , modelTime :: PosixEpochTime
  , modelRestorableDBs :: HashMap Repo Meta
  , modelDownloadingDB :: Maybe Repo
  -- TODO open dbs
  -- TODO download queue
  }

type ShardId = Text -- repo hash

initialModel :: String -> Model
initialModel backupDir =
  Model
    def
    def
    def
    backupDir
    def
    mempty
    emptyEntries
    zeroTime
    mempty
    Nothing

emptyEntries :: EntriesF a
emptyEntries = Entries mempty mempty mempty mempty mempty

mkEntry :: Item -> EntryF Identity
mkEntry Item {..} =
  Entry
    { entryRepo = itemRepo
    , entryStatus = pure ItemMissing
    , entryMeta = pure itemMeta
    , entryDirty = pure False
    , entryComitting = pure False
    , entryExpiring = pure Nothing
    }

snapshotEntry :: Entry -> STM (EntryF Identity)
snapshotEntry Entry {..} = do
  entryStatus <- Identity <$> readTVar entryStatus
  entryMeta <- Identity <$> readTVar entryMeta
  entryDirty <- Identity <$> readTVar entryDirty
  entryComitting <- Identity <$> readTVar entryComitting
  entryExpiring <- Identity <$> readTVar entryExpiring
  return Entry {..}

--------------------------------------------------------------------------------

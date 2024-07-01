{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Model.System(SystemState, modelState, readSystemState) where

import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import Data.Text (Text)
import Glean.Database.Catalog (
  EntriesF (..),
  EntryF,
  getEntries,
 )
import Glean.Database.Types (Env (..))
import Glean.Internal.Types (Meta (..))
import Glean.Types (Repo)
import Model.Model ( Model(..), snapshotEntry )
import ServiceData.GlobalStats (getCounters)
import Util.STM (atomically, readTVarIO)

data SystemState = SystemState
  { entries :: EntriesF (EntryF Identity)
  , counters :: HashMap ByteString Int
  , advertisedShards :: HashSet Text
  , restorableDBs :: HashMap Repo Meta
  , openDBs :: HashSet Repo
  }
  deriving (Show)

readSystemState :: Env -> IO SystemState
readSystemState Env {..} = do
  entries <- atomically $ getEntries envCatalog >>= traverse snapshotEntry
  _counters <- getCounters
  restorableDBs <-
    maybe mempty (HM.fromList . snd)
      <$> readTVarIO envCachedRestorableDBs
  -- openDBs <- HM.keysSet <$> readTVarIO envActive
  let advertisedShards = mempty
      counters = mempty
      openDBs = mempty
  return SystemState {..}

modelState :: Model -> SystemState
modelState Model {..} =
  SystemState
    { entries = modelEntries
    , counters
    , advertisedShards = mempty
    , restorableDBs = modelRestorableDBs
    , openDBs = mempty
    }
  where
    counters = HM.fromList [] -- TODO

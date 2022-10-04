{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | This is a very incomplete skeleton of an in-memory storage backend.
-- at the moment it is only usable for tests which don't produce or query
-- facts.

module Glean.Database.Storage.Memory
  ( Memory
  , newStorage
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import System.IO.Temp (withSystemTempDirectory)

import Glean.Database.Exception
import Glean.Database.Storage
import Glean.Repo.Text
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup
import Glean.Types (Repo)

newtype Memory = Memory (TVar (HashMap Repo (Database Memory)))

newStorage :: IO Memory
newStorage = Memory <$> newTVarIO HashMap.empty

-- | An abstract storage for fact database
instance Storage Memory where
  data Database Memory = Database
    { dbRepo :: Repo
    , dbFacts :: FactSet
    , dbData :: TVar (HashMap ByteString ByteString)
    }

  describe = const "memory:"

  open (Memory v) repo (Create start _) _ = do
    facts <- FactSet.new start
    atomically $ do
      dbs <- readTVar v
      case HashMap.lookup repo dbs of
        Nothing -> do
          db <- Database repo facts <$> newTVar mempty
          writeTVar v $ HashMap.insert repo db dbs
          return db
        Just _ -> dbError repo "database already exists"
  open (Memory v) repo _ _ = do
    dbs <- readTVarIO v
    case HashMap.lookup repo dbs of
      Just db -> return db
      Nothing -> dbError repo "database doesn't exist"

  -- TODO
  close _ = return ()

  delete (Memory v) = atomically . modifyTVar' v . HashMap.delete

  safeRemoveForcibly = delete

  predicateStats = FactSet.predicateStats . dbFacts

  store db key value =
    atomically $ modifyTVar' (dbData db) $ HashMap.insert key value
  retrieve db key =
    atomically $ HashMap.lookup key <$> readTVar (dbData db)

  -- TODO: ownership
  commit db facts _ = FactSet.append (dbFacts db) facts

  optimize _ = return ()

  -- TODO: ownership
  computeOwnership _ _ = return (error "unimplemented computeOwnership")
  getUnitId _ _ = return (error "unimplemented getUnitId")
  getUnit _ _ = return (error "unimplemented getUnit")
  storeOwnership _ _ = return ()  -- can't fail, otherwise we fail tests
  getOwnership _ = return Nothing
  addDefineOwnership _ _ =
    return (error "unimplemented addDefineOwnership")
  computeDerivedOwnership _ _ _ =
    return (error "unimplemented computeDerivedOwnership")

  -- TODO
  getTotalCapacity _ = return maxBound

  -- TODO
  getUsedCapacity _ = return 0

  -- TODO
  getFreeCapacity _ = return maxBound

  withScratchRoot _ f = withSystemTempDirectory "glean" f

  -- TODO
  backup db _ _ = dbError (dbRepo db) "unimplemented 'backup'"
  -- TODO
  restore _ repo _ _ = dbError repo "unimplemented 'restore'"

instance CanLookup (Database Memory) where
  lookupName Database{..} = "memory:" <> repoToText dbRepo
  withLookup = withLookup . dbFacts

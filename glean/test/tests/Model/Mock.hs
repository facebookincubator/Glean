{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Model.Mock where

import qualified Glean.Database.Catalog.Local.Memory as Catalog
import Glean.Database.Config
import Glean.Database.Storage (Storage (..))
import qualified Glean.Database.Storage.Memory as Memory
import Glean.RTS.Foreign.Lookup (CanLookup)
import Glean.Test.Mock (Mock, call, implement)
import Glean.Types (Repo)
import Glean.Database.Storage.Memory (Memory)

data MockDataStore s = MockDataStore
  { mockedDataStore :: DataStore
  , mockDataStoreStorage :: MockStorage s
  }

memoryDataStoreForSchema :: IO (MockDataStore Memory)
memoryDataStoreForSchema = do
  mockDataStoreStorage <- mockStorage =<< Memory.newStorage
  let mockedDataStore =
        DataStore
          { withDataStore = \_ f -> do
              cat <- Catalog.memoryCatalog
              f cat mockDataStoreStorage
          , dataStoreTag = "memory"
          }
  return MockDataStore {..}

data MockStorage s = MockStorage
  { mockedStorage :: s
  , mockRestore :: Mock (Repo -> FilePath -> FilePath -> IO ())
  }

mockStorage :: Storage s => s -> IO (MockStorage s)
mockStorage mockedStorage = do
  mockRestore <- implement "restore" (restore mockedStorage)
  return MockStorage {..}

instance Storage s => Storage (MockStorage s) where
  newtype Database (MockStorage s) = MockDatabase
    {unmockDatabase :: Glean.Database.Storage.Database s}
  describe = describe . mockedStorage
  open MockStorage {mockedStorage} r m db =
    MockDatabase <$> open mockedStorage r m db
  close = close . unmockDatabase
  delete MockStorage {mockedStorage} = delete mockedStorage
  safeRemoveForcibly MockStorage {mockedStorage} =
    safeRemoveForcibly mockedStorage
  predicateStats = predicateStats . unmockDatabase
  store = store . unmockDatabase
  retrieve = retrieve . unmockDatabase
  commit = commit . unmockDatabase
  optimize = optimize . unmockDatabase
  computeOwnership = computeOwnership . unmockDatabase
  storeOwnership = storeOwnership . unmockDatabase
  getOwnership = getOwnership . unmockDatabase
  cacheOwnership = cacheOwnership . unmockDatabase
  getUnitId = getUnitId . unmockDatabase
  getUnit = getUnit . unmockDatabase
  addDefineOwnership = addDefineOwnership . unmockDatabase
  computeDerivedOwnership = computeDerivedOwnership . unmockDatabase
  getTotalCapacity MockStorage {mockedStorage} = getTotalCapacity mockedStorage
  getUsedCapacity MockStorage {mockedStorage} = getUsedCapacity mockedStorage
  getFreeCapacity MockStorage {mockedStorage} = getFreeCapacity mockedStorage
  withScratchRoot MockStorage {mockedStorage} = withScratchRoot mockedStorage
  backup = backup . unmockDatabase
  restore MockStorage {mockRestore} = call mockRestore

deriving instance
  CanLookup (Glean.Database.Storage.Database s) =>
  CanLookup (Glean.Database.Storage.Database (MockStorage s))

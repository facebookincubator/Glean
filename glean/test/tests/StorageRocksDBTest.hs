{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module StorageRocksDBTest where

import Data.Default
import Data.Maybe (isJust, isNothing)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit

import Glean.Database.Storage (Storage(..))
import qualified Glean.Database.Storage.RocksDB as RocksDB
import qualified Glean.ServerConfig.Types as ServerConfig
import TestRunner
import Glean.Init
import Glean.Util.Disk (getDiskSize)

--------------------- Disk Capacity tests ------------------------------

testNoDiskLimit :: Test
testNoDiskLimit =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    storage <- RocksDB.newStorage tmpDir def
    assertEqual "No disk limit is set" Nothing
      (RocksDB.rocksMaxDiskSize storage)

    fullDiskCapacity <- getDiskSize tmpDir
    Just rocksTotalCapacity <- getTotalCapacity storage
    assertEqual "rocks has full disk capacity" fullDiskCapacity
      rocksTotalCapacity

testDiskLimitWithRatio :: Test
testDiskLimitWithRatio =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    let config = def
          { ServerConfig.config_db_rocksdb_disk_mem_capacity_ratio_limit =
              Just 20
          }
    storage <- RocksDB.newStorage tmpDir config
    assertBool "Disk limit exist" $
      isJust (RocksDB.rocksMaxDiskSize storage)

testGetTotalCapacity :: Test
testGetTotalCapacity =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    storage <- RocksDB.newStorage tmpDir def

    -- limit is definitely smaller than the actual disk size
    -- limit will be applied
    let maxDiskSmall = 2 * 1024 * 1024 * 1024 -- 2 GB
        storageMocked = storage
          { RocksDB.rocksMaxDiskSize = Just maxDiskSmall }
    totalCapacity <- getTotalCapacity storageMocked
    assertEqual
      "RocksDB disk is limited"
      (Just maxDiskSmall)
      totalCapacity

    -- limit is definitely bigger than actual disk size
    -- actual disk capacity should be used
    let maxDiskBig = maxBound :: Int -- ~8000 Petabytes
        storageMocked = storage
          { RocksDB.rocksMaxDiskSize = Just maxDiskBig }
    fullDiskCapacity <- getDiskSize tmpDir
    totalCapacity <- getTotalCapacity storageMocked
    assertEqual
      "RocksDB ignores too big disk limit"
      (Just fullDiskCapacity)
      totalCapacity

    -- If totalCapacity is Nothing, Meta's internal load balacing will be
    -- at risk since it depends on this value
    assertBool "totalCapacity is not empty" (isJust totalCapacity)

--------------------- Cache tests ------------------------------

testCacheWithRatioAndCacheMb :: Test
testCacheWithRatioAndCacheMb =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    -- When both cache_to_mem_ratio and cache_mb are provided,
    -- cache should be created using memCapacity * ratio
    let config = def
          { ServerConfig.config_db_rocksdb_cache_to_mem_ratio = Just 0.1
          , ServerConfig.config_db_rocksdb_cache_mb = 100
          }
    storage <- RocksDB.newStorage tmpDir config
    capacity <- RocksDB.getCacheCapacity storage
    assertBool
      "Cache capacity should be greater than 100MB (cache_mb is ignored)"
        (capacity > 100 * 1024 * 1024)

testCacheWithOnlyCacheMb :: Test
testCacheWithOnlyCacheMb =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    -- When cache_to_mem_ratio is Nothing, should use cache_mb
    let config = def
          { ServerConfig.config_db_rocksdb_cache_to_mem_ratio = Nothing
          , ServerConfig.config_db_rocksdb_cache_mb = 100
          }
    storage <- RocksDB.newStorage tmpDir config
    let cache = RocksDB.rocksCache storage
    assertBool "has cache" $ isJust cache
    cache_capacity <- RocksDB.getCacheCapacity storage
    assertEqual "Cache size equals config_db_rocksdb_cache_mb"
      cache_capacity (100 * 1024 * 1024)

testCacheIsNothingWhenCacheMbIsZero :: Test
testCacheIsNothingWhenCacheMbIsZero =
  TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
    -- When cache_mb is 0 and no ratio, cache should be Nothing
    let config = def
          { ServerConfig.config_db_rocksdb_cache_to_mem_ratio = Nothing
          , ServerConfig.config_db_rocksdb_cache_mb = 0
          }
    storage <- RocksDB.newStorage tmpDir config
    assertBool "Cache should be Nothing when cache_mb is 0" $
      isNothing (RocksDB.rocksCache storage)


-- TODO: implment last 3 tests, devmate drafted them, might be incorrect
main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "Test no disk limit" testNoDiskLimit
  , TestLabel "Test disk limit with ratio" testDiskLimitWithRatio
  , TestLabel "Test getTotalCapacity" testGetTotalCapacity
  , TestLabel "Test cache with ratio and cache_mb" testCacheWithRatioAndCacheMb
  , TestLabel "Test cache with only cache_mb" testCacheWithOnlyCacheMb
  , TestLabel "Test cache is Nothing when cache_mb is 0 and no ratio"
      testCacheIsNothingWhenCacheMbIsZero
  ]

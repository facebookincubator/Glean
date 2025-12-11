{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module StorageRocksDBTest where

import Data.Default
import Data.Maybe (isJust)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit

import Glean.Database.Storage (Storage(..))
import qualified Glean.Database.Storage.RocksDB as RocksDB
import qualified Glean.ServerConfig.Types as ServerConfig
import TestRunner
import Glean.Init
import Glean.Util.Disk (getDiskSize)


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "Test no disk limit" $
      TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
        storage <- RocksDB.newStorage tmpDir def
        assertEqual "No disk limit is set" Nothing
          (RocksDB.rocksMaxDiskSize storage)

        fullDiskCapacity <- getDiskSize tmpDir
        Just rocksTotalCapacity <- getTotalCapacity storage
        assertEqual "rocks has full disk capacity" fullDiskCapacity
          rocksTotalCapacity

  , TestLabel "Test disk limit with disk-to-mem ratio" $
      TestCase $ withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
        let config = def
              { ServerConfig.config_db_rocksdb_disk_mem_capacity_ratio_limit =
                  Just 20
              }
        storage <- RocksDB.newStorage tmpDir config
        assertBool "Disk limit exist" $
          isJust (RocksDB.rocksMaxDiskSize storage)

  , TestLabel "Test getTotalCapacity" $ TestCase $
      withSystemTempDirectory "rocksdb-test" $ \tmpDir -> do
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
  ]

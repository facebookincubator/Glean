{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Storage.LMDB
  ( LMDB(..)
  , newStorage
  ) where

import Control.Exception
import Control.Monad
import Data.Int
import qualified Data.Text as Text
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Directory
import System.IO.Temp (withTempDirectory)
import System.FilePath
import System.Process

import Util.FFI
import Util.IO (safeRemovePathForcibly)

import Glean.Database.Backup.Backend (Data(Data))
import Glean.Database.Repo (databasePath)
import Glean.Database.Storage
import Glean.Database.Storage.DB
import Glean.FFI
import Glean.RTS.Foreign.Lookup (CanLookup(..))
import Glean.RTS.Foreign.Ownership as Ownership
  hiding (computeDerivedOwnership)
import Glean.RTS.Types (Fid(..), invalidFid)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types (Repo)
import Glean.Util.Disk
import Glean.Impl.MemoryReader

data LMDB = LMDB
  { lmdbRoot :: FilePath
  , lmdbMaxDiskSize :: Maybe Int
    -- ^ virtual limit to report capped disk capacities. The limit is
    -- not enforced. It's up to each io usage to check diskspace before writing.
    -- We're using this to avoid serving too many dbs on query servers,
    -- and smarter sharding.
  }

newStorage :: FilePath -> ServerConfig.Config -> IO LMDB
newStorage root ServerConfig.Config{..} = do
  mem_capacity <- totalMemCapacityKB
  return LMDB
    { lmdbRoot = root
    , lmdbMaxDiskSize = case mem_capacity of
        Just mem -> (* (mem * 1024)) . fromIntegral <$>
          config_db_rocksdb_disk_mem_capacity_ratio_limit
        Nothing -> Nothing
    }

data instance Database LMDB = Database DB LMDB

instance CanLookup (Database LMDB) where
  withLookup (Database db _) = withLookup db
  lookupName (Database db _) = lookupName db

instance Storage LMDB where
  describe db = "lmdb:" <> lmdbRoot db

  -- we started at 3 because that's what RocksDB was on
  readableVersions _ = [DBVersion 3]
  writableVersions _ = [DBVersion 3]

  open lmdb repo mode (DBVersion version) = do
    (cmode, start, ownership) <- case mode of
      ReadOnly -> do
        exists <- doesDirectoryExist (path </> "data.mdb")
        when (not exists) $ do
          haveSquash <- doesFileExist squash
          when haveSquash $ do
            createDirectoryIfMissing True path
            callProcess "squashfuse_ll" [squash, path]
        return (0, invalidFid, Nothing)
      ReadWrite -> return (1, invalidFid, Nothing)
      Create start ownership _ -> do
        createDirectoryIfMissing True path
        return (2, start, ownership)
    withCString path $ \cpath ->
      using (invoke $ glean_lmdb_container_open cpath cmode)
        $ \container -> do
      fp <- mask_ $ do
        first_unit_id <- maybe (return firstUsetId) nextUsetId ownership
        p <- invoke $
          glean_lmdb_container_open_database container start
            first_unit_id version
        newForeignPtr glean_rocksdb_database_free p
      return (Database (DB (castForeignPtr fp) repo) lmdb)
    where
      path = containerPath lmdb repo
      squash = path <.> "squashfs"

  delete lmdb = safeRemovePathForcibly . containerPath lmdb

  safeRemoveForcibly lmdb =
      safeRemovePathForcibly . databasePath (lmdbRoot lmdb)

  getTotalCapacity lmdb = do
    exists <- doesDirectoryExist (lmdbRoot lmdb)
    if exists
      then do
        fullDiskCapacity <- getDiskSize (lmdbRoot lmdb)
        return $ Just $ case lmdbMaxDiskSize lmdb of
          Just maxDiskSize -> min maxDiskSize fullDiskCapacity
          Nothing -> fullDiskCapacity
      else return Nothing

  getUsedCapacity lmdb = do
    exists <- doesDirectoryExist (lmdbRoot lmdb)
    if exists
      then Just <$> getUsedDiskSpace (lmdbRoot lmdb)
      else return Nothing

  getFreeCapacity lmdb = do
    used <- getUsedCapacity lmdb
    total <- getTotalCapacity lmdb
    case (used,total) of
      (Just used, Just total) -> return $ total - used
      _ -> getFreeDiskSpace (lmdbRoot lmdb) -- not aware of disk limit

  withScratchRoot rocks f = f $ lmdbRoot rocks </> ".scratch"

  restore lmdb cfg repo scratch scratch_file = do
    withTempDirectory scratch "restore" $ \scratch_restore -> do
      let target = containerPath lmdb repo
      createDirectoryIfMissing True $ takeDirectory target
      if ServerConfig.config_db_lmdb_restore_unpack cfg
        then do
          let db = scratch_restore </> "db"
          createDirectoryIfMissing True db
          callProcess "unsquashfs" ["-d", db, scratch_file ]
            -- to avoid retaining an extra copy of the DB during restore,
            -- delete the input file now.
          renameDirectory db target
        else
          renameFile scratch_file (target <.> "squashfs")

containerPath :: LMDB -> Repo -> FilePath
containerPath LMDB{..} repo = databasePath lmdbRoot repo </> "db"

instance DatabaseOps (Database LMDB) where
  close (Database db@(DB _ repo) lmdb) = do
    close db
    let path = containerPath lmdb repo; squash = path <.> "squashfs"
    haveSquash <- doesFileExist squash
    when haveSquash $ callProcess "umount" [path]
      `catch` \(_ :: IOException) -> return ()

  predicateStats (Database db _) = predicateStats db
  store (Database db _) = store db
  retrieve (Database db _) = retrieve db
  commit (Database db _) = commit db
  addOwnership (Database db _) = addOwnership db
  optimize (Database db _) = optimize db
  computeOwnership (Database db _) = computeOwnership db
  storeOwnership (Database db _) = storeOwnership db
  getOwnership (Database db _) = getOwnership db
  getUnitId (Database db _) = getUnitId db
  getUnit (Database db _) = getUnit db
  addDefineOwnership (Database db _) = addDefineOwnership db
  computeDerivedOwnership (Database db _) = computeDerivedOwnership db
  cacheOwnership (Database db _) = cacheOwnership db
  prepareFactOwnerCache (Database db _) = prepareFactOwnerCache db

  backup (Database db _) cfg scratch process =
    backup db cfg scratch $ \path _ -> do
      withTempDirectory scratch "out" $ \tmpdir -> do
        let out = tmpdir </> "db.squashfs"
        callProcess "mksquashfs" $ [ path, out ] <>
          map Text.unpack (ServerConfig.config_db_lmdb_mksquashfs_args cfg)
        size <- getFileSize out
        process out (Data $ fromIntegral size)


foreign import ccall safe glean_lmdb_container_open
  :: CString
  -> CInt
  -> Ptr Container
  -> IO CString

foreign import ccall safe glean_lmdb_container_open_database
  :: Container
  -> Fid
  -> UsetId
  -> Int64
  -> Ptr (Ptr (Database LMDB))
  -> IO CString
foreign import ccall safe "&glean_rocksdb_database_free"
  glean_rocksdb_database_free :: Destroy (Database LMDB)

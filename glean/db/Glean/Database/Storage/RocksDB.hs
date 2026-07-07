{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Storage.RocksDB
  ( RocksDB(..)
  , newStorage
  , getCacheCapacity
  ) where

import Control.Exception
import Control.Monad
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Directory
import System.IO (Handle)
import qualified Data.ByteString.Char8 as BS
import System.IO.Temp (withTempDirectory)
import System.FilePath
import System.Process
  ( CreateProcess(..), StdStream(..), proc
  , readProcessWithExitCode, waitForProcess, withCreateProcess
  )
import System.Exit (ExitCode(ExitSuccess))

import Util.FFI
import Util.IO (safeRemovePathForcibly)
import Util.Log (logInfo)

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
import System.IO.Extra (withTempFile)

newtype Cache = Cache (ForeignPtr Cache)

instance Object Cache where
  wrap = Cache
  unwrap (Cache p) = p
  destroy = glean_rocksdb_free_cache

newCache :: Int -> IO Cache
newCache size =
  construct $ invoke $ glean_rocksdb_new_cache (fromIntegral size)

withCache :: Maybe Cache -> (Ptr Cache -> IO a) -> IO a
withCache (Just cache) f = with cache f
withCache Nothing f = f nullPtr

data RocksDB = RocksDB
  { rocksRoot :: FilePath
  , rocksCache :: Maybe Cache
  , rocksCacheIndexAndFilterBlocks :: Bool
  , rocksMaxDiskSize :: Maybe Int
    -- ^ virtual limit (bytes) to report capped disk capacities. The limit is
    -- not enforced. It's up to each io usage to check diskspace before writing.
    -- We're using this to avoid serving too many dbs on query servers,
    -- and smarter sharding.
  }

-- | Compute the size of the cache in bytes
-- There's two configs that control the size:
-- 1. db_rocksdb_cache_to_mem_ratio is used if provided
--    sets the rocksdb cache to a fraction of the total memory.
--    This is useful if the same server binary runs with the same server_config,
--    but with varying RAM capacity
-- 2. db_rocksdb_cache_mb is used if the above is not provided,
--    or if the memory capacity cannot be read
cacheSize :: ServerConfig.Config -> Maybe Int ->  Int
cacheSize ServerConfig.Config{..} memCapacityKB =
  case (memCapacityKB, config_db_rocksdb_cache_to_mem_ratio) of
    (Just ramKB, Just ratio) -> round $ fromIntegral ramKB * ratio * 1024
    _ -> fromIntegral config_db_rocksdb_cache_mb * 1024 * 1024

newStorage :: FilePath -> ServerConfig.Config -> IO RocksDB
newStorage root config@ServerConfig.Config{..} = do
  mem_capacity <- totalMemCapacityKB
  let cache_size = cacheSize config mem_capacity
  cache <- if cache_size > 0
    then Just <$> newCache cache_size
    else return Nothing
  return RocksDB
    { rocksRoot = root
    , rocksCache = cache
    , rocksCacheIndexAndFilterBlocks =
        config_db_rocksdb_cache_index_and_filter_blocks
    , rocksMaxDiskSize = case mem_capacity of
        Just mem -> (* (mem * 1024)) . fromIntegral <$>
          config_db_rocksdb_disk_mem_capacity_ratio_limit
        Nothing -> Nothing
    }

newtype instance Database RocksDB = Database DB
  deriving (CanLookup)

instance Storage RocksDB where
  describe rocks = "rocksdb:" <> rocksRoot rocks

  readableVersions _ = [DBVersion 3]
  writableVersions _ = [DBVersion 3]

  open rocks repo mode (DBVersion version) = do
    (cmode, start, ownership) <- case mode of
      ReadOnly -> return (0, invalidFid, Nothing)
      ReadWrite -> return (1, invalidFid, Nothing)
      Create start ownership _ _ -> do
        createDirectoryIfMissing True path
        return (2, start, ownership)
    withCString path $ \cpath ->
      withCache (rocksCache rocks) $ \cache_ptr ->
      using (invoke $
          glean_rocksdb_container_open cpath
            cmode
            (fromIntegral (fromEnum (rocksCacheIndexAndFilterBlocks rocks)))
            cache_ptr)
        $ \container -> do
      fp <- mask_ $ do
        first_unit_id <- maybe (return firstUsetId) nextUsetId ownership
        p <- invoke $
          glean_rocksdb_container_open_database container start
            first_unit_id version
        newForeignPtr glean_rocksdb_database_free p
      return (Database (DB (castForeignPtr fp) repo))
    where
      path = containerPath rocks repo

  delete rocks = safeRemovePathForcibly . containerPath rocks

  safeRemoveForcibly rocks =
      safeRemovePathForcibly . databasePath (rocksRoot rocks)

  getTotalCapacity rocksdb = do
    exists <- doesDirectoryExist (rocksRoot rocksdb)
    if exists
      then do
        fullDiskCapacity <- getDiskSize (rocksRoot rocksdb)
        return $ Just $ case rocksMaxDiskSize rocksdb of
          Just maxDiskSize -> min maxDiskSize fullDiskCapacity
          Nothing -> fullDiskCapacity
      else return Nothing

  getUsedCapacity rocksdb = do
    exists <- doesDirectoryExist (rocksRoot rocksdb)
    if exists
      then Just <$> getUsedDiskSpace (rocksRoot rocksdb)
      else return Nothing

  getFreeCapacity rocksdb = do
    used <- getUsedCapacity rocksdb
    total <- getTotalCapacity rocksdb
    case (used,total) of
      (Just used, Just total) -> return $ total - used
      _ -> getFreeDiskSpace (rocksRoot rocksdb) -- not aware of disk limit

  withScratchRoot rocks f = f $ rocksRoot rocks </> ".scratch"

  restore rocks _ repo scratch src =
    withTempDirectory scratch "restore" $ \scratch_restore -> do
      case src of
        SourceFile p -> unTar p scratch_restore
        SourceStream h -> unTarFromHandle h scratch_restore

      restoreFromDir rocks repo scratch scratch_restore

instance DatabaseOps (Database RocksDB) where
  close (Database db) = close db
  predicateStats (Database db) = predicateStats db
  store (Database db) = store db
  retrieve (Database db) = retrieve db
  addBatchDescriptor (Database db) = addBatchDescriptor db
  markBatchDescriptorAsWritten (Database db) = markBatchDescriptorAsWritten db
  isBatchDescriptorStored (Database db) = isBatchDescriptorStored db
  getUnprocessedBatchDescriptors (Database db) =
    getUnprocessedBatchDescriptors db
  commit (Database db) = commit db
  addOwnership (Database db) = addOwnership db
  registerACLUnits (Database db) = registerACLUnits db
  optimize (Database db) = optimize db
  flush (Database db) = flush db
  computeOwnership (Database db) = computeOwnership db
  storeOwnership (Database db) = storeOwnership db
  getOwnership (Database db) = getOwnership db
  getUnitId (Database db) = getUnitId db
  getUnit (Database db) = getUnit db
  getUnitsByPrefix (Database db) = getUnitsByPrefix db
  addDefineOwnership (Database db) = addDefineOwnership db
  computeDerivedOwnership (Database db) = computeDerivedOwnership db
  cacheOwnership (Database db) = cacheOwnership db
  prepareFactOwnerCache (Database db) = prepareFactOwnerCache db

  backup (Database db) cfg scratch process = do
    let mkTar path _ = do
          let (base, dir) = splitFileName path
          withTempFile $ \tarFile -> do
            tar ["-cf", tarFile, "-C", base, dir]
            size <- getFileSize tarFile
            process tarFile (Data $ fromIntegral size)
    if ServerConfig.config_db_backup_use_checkpoint cfg
      then do
        logInfo "backup: packing as RocksDB checkpoint (db/ tarball)"
        checkpoint db scratch mkTar
      else do
        logInfo "backup: packing as BackupEngine backup (backup/ tarball)"
        backup db cfg scratch mkTar


unTar :: FilePath -> FilePath -> IO ()
unTar scratch_file scratch_restore =
  tar ["-xf", scratch_file, "-C", scratch_restore]

-- | Extract a tar stream read directly from a 'Handle' into a directory,
-- without first materialising the tarball on disk.
unTarFromHandle :: Handle -> FilePath -> IO ()
unTarFromHandle h dir = do
  tarPath <- findExecutable "tar"
  case tarPath of
    Nothing -> throwIO $ userError "Cannot find tar executable"
    Just path ->
      -- withCreateProcess guarantees the tar process is reaped and its
      -- handles closed on all exit paths, including exceptions while
      -- reading stderr.
      withCreateProcess (proc path ["-x", "-C", dir])
        { std_in = UseHandle h, std_err = CreatePipe } $ \_ _ mb_herr ph -> do
      herr <- maybe (throwIO $ userError "tar: stderr pipe was not created")
                pure mb_herr
      -- Read stderr fully (and force it) before waiting, both to surface a
      -- useful diagnostic on failure and to avoid blocking the tar process
      -- if it fills the stderr pipe buffer.
      err <- BS.hGetContents herr
      _ <- evaluate (BS.length err)
      ec <- waitForProcess ph
      unless (ec == ExitSuccess) $
        throwIO $ userError $ "tar -x from stream failed: " <> BS.unpack err

-- | Given a directory containing an extracted backup (either a RocksDB
-- backup under @backup/@ or a plain DB tarball under @db/@), restore it
-- into place for the given repo.
restoreFromDir :: RocksDB -> Repo -> FilePath -> FilePath -> IO ()
restoreFromDir rocks repo scratch scratch_restore = do
  -- If the tarfile contains "backup/.." then it is a RocksDB backup
  -- If the tarfile contains "db/.." then it is a plain tarball of the DB
  let scratch_restore_backup = scratch_restore </> "backup"
  is_rocksdb_backup <- doesDirectoryExist scratch_restore_backup
  db <-
    if is_rocksdb_backup
      then do
        let scratch_db = scratch </> "db"
        createDirectoryIfMissing True scratch_db
        withCString scratch_db $ \p_target ->
          withCString (scratch_restore </> "backup") $ \p_source ->
            invoke $ glean_rocksdb_restore p_target p_source
        return scratch_db
      else do
        let scratch_restore_db = scratch_restore </> "db"
        is_copy <- doesDirectoryExist scratch_restore_db
        if is_copy
          then return scratch_restore_db
          else throwIO $ userError "unrecognised backup"
  let target = containerPath rocks repo
  createDirectoryIfMissing True $ takeDirectory target
  renameDirectory db target

tar :: [String] -> IO ()
tar args = do
  tarPath <- findExecutable "tar"
  case tarPath of
    Nothing  -> throwIO $ userError "Cannot find tar executable"
    Just path -> do
      (ec, _, err) <- readProcessWithExitCode path args ""
      unless (ec == ExitSuccess) $ throwIO $ userError err

containerPath :: RocksDB -> Repo -> FilePath
containerPath RocksDB{..} repo = databasePath rocksRoot repo </> "db"

foreign import ccall unsafe glean_rocksdb_new_cache
  :: CSize -> Ptr (Ptr Cache) -> IO CString
foreign import ccall unsafe "&glean_rocksdb_free_cache"
  glean_rocksdb_free_cache :: Destroy Cache
foreign import ccall unsafe glean_rocksdb_cache_capacity
  :: Ptr Cache -> IO CSize

getCacheCapacity :: RocksDB -> IO Int
getCacheCapacity rocks = do
  withCache (rocksCache rocks) $
    fmap fromIntegral . glean_rocksdb_cache_capacity


foreign import ccall safe glean_rocksdb_container_open
  :: CString
  -> CInt
  -> CBool
  -> Ptr Cache
  -> Ptr Container
  -> IO CString

foreign import ccall safe glean_rocksdb_container_open_database
  :: Container
  -> Fid
  -> UsetId
  -> Int64
  -> Ptr (Ptr (Database RocksDB))
  -> IO CString
foreign import ccall safe "&glean_rocksdb_database_free"
  glean_rocksdb_database_free :: Destroy (Database RocksDB)

foreign import ccall safe glean_rocksdb_restore
  :: CString
  -> CString
  -> IO CString

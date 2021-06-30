-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Storage.RocksDB
  ( RocksDB(..)
  , newStorage
  ) where

import qualified Codec.Archive.Tar as Tar
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.List (unzip4)
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.FilePath

import Util.IO (safeRemovePathForcibly)

import Glean.Database.Repo (databasePath)
import Glean.Database.Storage
import Glean.FFI
import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Lookup
  (CanLookup(..), Lookup(..))
import Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Types (Fid(..), invalidFid, Pid(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types (Repo)
import qualified Glean.Types as Thrift

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
  }

newStorage :: FilePath -> ServerConfig.Config -> IO RocksDB
newStorage root ServerConfig.Config{..} = do
  cache <- if config_db_rocksdb_cache_mb > 0
    then
      Just <$> newCache (fromIntegral config_db_rocksdb_cache_mb * 1024 * 1024)
    else return Nothing
  return RocksDB
    { rocksRoot = root
    , rocksCache = cache
    }

newtype Container = Container (Ptr Container)
  deriving(Storable)

instance Static Container where
  destroyStatic = glean_rocksdb_container_free

instance Storage RocksDB where
  newtype Database RocksDB = Database (ForeignPtr (Database RocksDB))

  open rocks repo mode (DBVersion version) = do
    (cmode, start) <- case mode of
      ReadOnly -> return (0, invalidFid)
      ReadWrite -> return (1, invalidFid)
      Create start _ -> do
        createDirectoryIfMissing True path
        return (2, start)
    withCString path $ \cpath ->
      withCache (rocksCache rocks) $ \cache_ptr ->
      using (invoke $ glean_rocksdb_container_open cpath cmode cache_ptr)
        $ \container ->
      construct
        $ invoke $ glean_rocksdb_container_open_database container start version
    where
      path = containerPath rocks repo

  close db = withContainer db glean_rocksdb_container_close

  delete rocks = safeRemovePathForcibly . containerPath rocks

  predicateStats db = with db $ \db_ptr -> do
    (count, pids, counts, sizes) <- invoke $ glean_rocksdb_database_stats db_ptr
    usingMalloced pids $
      usingMalloced counts $
      usingMalloced sizes $
      forM [0 .. fromIntegral count - 1] $ \i -> do
        pid <- peekElemOff pids i
        count <- peekElemOff counts i
        size <- peekElemOff sizes i
        return (Pid pid, Thrift.PredicateStats
                      (fromIntegral count)
                      (fromIntegral size))

  store db key value =
    withContainer db $ \s_ptr ->
    unsafeWithBytes key $ \key_ptr key_size ->
    unsafeWithBytes value $ \value_ptr value_size ->
    invoke $ glean_rocksdb_container_write_data
      s_ptr
      key_ptr
      key_size
      value_ptr
      value_size

  retrieve db key =
    withContainer db $ \s_ptr ->
    unsafeWithBytes key $ \key_ptr key_size -> do
      (value_ptr, value_size, found)
        <- invoke $ glean_rocksdb_container_read_data s_ptr key_ptr key_size
      if found /= 0
        then Just <$> unsafeMallocedByteString value_ptr value_size
        else return Nothing

  commit db facts owned = with db $ \db_ptr -> do
    with facts $ \facts_ptr -> invoke $ glean_rocksdb_commit db_ptr facts_ptr
    when (not $ HashMap.null owned) $
      withMany entry (HashMap.toList owned) $ \xs ->
      let (unit_ptrs, unit_sizes, facts_ptrs, facts_sizes) = unzip4 xs
      in
      withArray unit_ptrs $ \p_unit_ptrs ->
      withArray unit_sizes $ \p_unit_sizes ->
      withArray facts_ptrs $ \p_facts_ptrs ->
      withArray facts_sizes $ \p_facts_sizes ->
      invoke $ glean_rocksdb_add_ownership
        db_ptr
        (fromIntegral $ HashMap.size owned)
        p_unit_ptrs
        p_unit_sizes
        p_facts_ptrs
        p_facts_sizes
    where
      entry (unit, facts) f =
        unsafeWithBytes unit $ \unit_ptr unit_size ->
        VS.unsafeWith facts $ \facts_ptr ->
        f (unit_ptr, unit_size, facts_ptr, fromIntegral $ VS.length facts)


  optimize db = withContainer db $ invoke . glean_rocksdb_container_optimize

  computeOwnership db inv =
    with db $ \db_ptr ->
    using (invoke $ glean_rocksdb_get_ownership_unit_iterator db_ptr) $
    Ownership.compute inv db

  storeOwnership db own =
    with db $ \db_ptr ->
    with own $ \own_ptr ->
    invoke $ glean_rocksdb_store_ownership db_ptr own_ptr

  getOwnership db =
    with db $ \db_ptr ->
    construct $ invoke $ glean_rocksdb_get_ownership db_ptr

  getUnitId db unit =
    with db $ \db_ptr->
    unsafeWithBytes unit $ \unit_ptr unit_size -> do
      w64 <- invoke $ glean_rocksdb_get_unit_id db_ptr unit_ptr unit_size
      if w64 > 0xffffffff
        then return Nothing
        else return (Just (UnitId (fromIntegral w64)))

  backup db scratch process = do
    createDirectoryIfMissing True path
    withContainer db $ \s_ptr ->
      withCString path $ invoke . glean_rocksdb_container_backup s_ptr
    entries <- Tar.pack scratch ["backup"]
    process $ Tar.write entries
    where
      path = scratch </> "backup"

  restore rocks repo scratch bytes = do
    createDirectoryIfMissing True scratch_restore
    createDirectoryIfMissing True scratch_db
    Tar.unpack scratch_restore $ Tar.read bytes
    withCString scratch_db $ \p_target ->
      withCString (scratch_restore </> "backup") $ \p_source ->
      invoke $ glean_rocksdb_restore p_target p_source
    createDirectoryIfMissing True $ takeDirectory target
    renameDirectory scratch_db target
    where
      scratch_restore = scratch </> "restore"
      scratch_db = scratch </> "db"

      target = containerPath rocks repo

containerPath :: RocksDB -> Repo -> FilePath
containerPath RocksDB{..} repo = databasePath rocksRoot repo </> "db"

instance Object (Database RocksDB) where
  wrap = Database
  unwrap (Database p) = p
  destroy = glean_rocksdb_database_free

instance CanLookup (Database RocksDB) where
  withLookup db f = with db $ f . glean_rocksdb_database_lookup

withContainer :: Database RocksDB -> (Container -> IO a) -> IO a
withContainer db f = with db $ f . glean_rocksdb_database_container

foreign import ccall unsafe glean_rocksdb_new_cache
  :: CSize -> Ptr (Ptr Cache) -> IO CString
foreign import ccall unsafe "&glean_rocksdb_free_cache"
  glean_rocksdb_free_cache :: Destroy Cache


foreign import ccall safe glean_rocksdb_container_open
  :: CString
  -> CInt
  -> Ptr Cache
  -> Ptr Container
  -> IO CString
foreign import ccall safe glean_rocksdb_container_free
  :: Container -> IO ()

foreign import ccall safe glean_rocksdb_container_close
  :: Container -> IO ()

foreign import ccall unsafe glean_rocksdb_container_write_data
  :: Container
  -> Ptr ()
  -> CSize
  -> Ptr ()
  -> CSize
  -> IO CString

foreign import ccall unsafe glean_rocksdb_container_read_data
  :: Container
  -> Ptr ()
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr CChar
  -> IO CString

foreign import ccall safe glean_rocksdb_container_optimize
  :: Container -> IO CString

foreign import ccall safe glean_rocksdb_container_backup
  :: Container -> CString -> IO CString

foreign import ccall unsafe glean_rocksdb_container_open_database
  :: Container
  -> Fid
  -> Int64
  -> Ptr (Ptr (Database RocksDB))
  -> IO CString
foreign import ccall safe "&glean_rocksdb_database_free"
  glean_rocksdb_database_free :: Destroy (Database RocksDB)

foreign import ccall unsafe glean_rocksdb_database_container
  :: Ptr (Database RocksDB) -> Container

foreign import ccall unsafe glean_rocksdb_database_lookup
  :: Ptr (Database RocksDB) -> Lookup

foreign import ccall safe glean_rocksdb_commit
  :: Ptr (Database RocksDB)
  -> Ptr FactSet
  -> IO CString

foreign import ccall safe glean_rocksdb_add_ownership
  :: Ptr (Database RocksDB)
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr (Ptr Fid)
  -> Ptr CSize
  -> IO CString

foreign import ccall safe glean_rocksdb_get_ownership_unit_iterator
  :: Ptr (Database RocksDB)
  -> Ptr Ownership.UnitIterator
  -> IO CString

foreign import ccall unsafe glean_rocksdb_database_stats
  :: Ptr (Database RocksDB)
  -> Ptr CSize
  -> Ptr (Ptr Int64)
  -> Ptr (Ptr Word64)
  -> Ptr (Ptr Word64)
  -> IO CString

foreign import ccall safe glean_rocksdb_restore
  :: CString
  -> CString
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_unit_id
  :: Ptr (Database RocksDB)
  -> Ptr ()
  -> CSize
  -> Ptr Word64
  -> IO CString

foreign import ccall unsafe glean_rocksdb_store_ownership
  :: Ptr (Database RocksDB)
  -> Ptr MemoryOwnership
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_ownership
  :: Ptr (Database RocksDB)
  -> Ptr (Ptr Ownership)
  -> IO CString

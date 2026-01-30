{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Generic DB operations shared by the RocksDB and LMDB backends
module Glean.Database.Storage.DB (
    DB(..), Container(..), withContainer
  ) where

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

import Util.FFI

import Glean.Database.Backup.Backend (Data(Data))
import Glean.Database.Storage
import Glean.FFI
import Glean.Repo.Text
import Glean.RTS.Foreign.FactSet (FactSet)
import Glean.RTS.Foreign.Lookup
  (CanLookup(..), Lookup(..))
import Glean.RTS.Foreign.Ownership as Ownership
import Glean.RTS.Foreign.Stats (marshalPredicateStats)
import Glean.RTS.Types (Fid(..), Pid(..))
import Glean.Types (Repo)

data DB = DB
  { dbPtr :: ForeignPtr DB
  , dbRepo :: Repo
  }

instance CanLookup DB where
  lookupName db = "rocksdb:" <> repoToText (dbRepo db)
  withLookup db f = unsafeWithForeignPtr (dbPtr db) $
    f . glean_rocksdb_database_lookup

instance DatabaseOps DB where
  close db = withContainer db glean_rocksdb_container_close

  predicateStats db = unsafeWithForeignPtr (dbPtr db)
    $ marshalPredicateStats . glean_rocksdb_database_predicateStats

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

  commit db facts = unsafeWithForeignPtr (dbPtr db) $ \db_ptr -> do
    with facts $ \facts_ptr -> invoke $ glean_rocksdb_commit db_ptr facts_ptr

  addOwnership db _ owned =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
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

  optimize db compact = withContainer db $ \s_ptr ->
    invoke $ glean_rocksdb_container_optimize s_ptr
      (fromIntegral (fromEnum compact))

  computeOwnership db base inv =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    using (invoke $ glean_rocksdb_get_ownership_unit_iterator db_ptr) $
    Ownership.compute inv db base

  storeOwnership db _ own =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    with own $ \own_ptr ->
    invoke $ glean_rocksdb_store_ownership db_ptr own_ptr

  getOwnership db = fmap Just $
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    construct $ invoke $ glean_rocksdb_get_ownership db_ptr

  getUnitId db unit =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    unsafeWithBytes unit $ \unit_ptr unit_size -> do
      w64 <- invoke $ glean_rocksdb_get_unit_id db_ptr unit_ptr unit_size
      if w64 > 0xffffffff
        then return Nothing
        else return (Just (UnitId (fromIntegral w64)))

  getUnit db unit =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr -> do
      (unit_ptr, unit_size) <- invoke $ glean_rocksdb_get_unit db_ptr unit
      if unit_size /= 0
        then Just <$> unsafeMallocedByteString unit_ptr unit_size
        else return Nothing

  addDefineOwnership db _ define =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    with define $ \define_ptr ->
      invoke $ glean_rocksdb_add_define_ownership db_ptr define_ptr

  computeDerivedOwnership db _ ownership base (Pid pid) =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
    using
      (invoke $
        glean_rocksdb_get_derived_fact_ownership_iterator
          db_ptr
          (fromIntegral pid)) $
      Ownership.computeDerivedOwnership ownership base

  cacheOwnership db =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
      invoke $ glean_rocksdb_cache_ownership db_ptr

  prepareFactOwnerCache db =
    unsafeWithForeignPtr (dbPtr db) $ \db_ptr ->
      invoke $ glean_rocksdb_prepare_fact_owner_cache db_ptr

  backup db _ scratch process = do
    let path = scratch </> "backup"
    createDirectoryIfMissing True path
    withContainer db $ \s_ptr ->
      withCString path $ invoke . glean_rocksdb_container_backup s_ptr
    process path (Data 0)

newtype Container = Container (Ptr Container)
  deriving(Storable)

instance Static Container where
  destroyStatic = glean_rocksdb_container_free

withContainer :: DB -> (Container -> IO a) -> IO a
withContainer db f = unsafeWithForeignPtr (dbPtr db) $
  f . glean_rocksdb_database_container

foreign import ccall safe glean_rocksdb_container_free
  :: Container -> IO ()

foreign import ccall safe glean_rocksdb_container_close
  :: Container -> IO ()

foreign import ccall safe glean_rocksdb_container_backup
  :: Container -> CString -> IO CString

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
  :: Container -> CBool -> IO CString

foreign import ccall unsafe glean_rocksdb_database_container
  :: Ptr DB -> Container

foreign import ccall unsafe glean_rocksdb_database_lookup
  :: Ptr DB -> Ptr Lookup

foreign import ccall safe glean_rocksdb_commit
  :: Ptr DB
  -> Ptr FactSet
  -> IO CString

foreign import ccall safe glean_rocksdb_add_ownership
  :: Ptr DB
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr (Ptr Fid)
  -> Ptr CSize
  -> IO CString

foreign import ccall safe glean_rocksdb_get_ownership_unit_iterator
  :: Ptr DB
  -> Ptr Ownership.UnitIterator
  -> IO CString

foreign import ccall unsafe glean_rocksdb_database_predicateStats
  :: Ptr DB
  -> Ptr CSize
  -> Ptr (Ptr Int64)
  -> Ptr (Ptr Word64)
  -> Ptr (Ptr Word64)
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_unit_id
  :: Ptr DB
  -> Ptr ()
  -> CSize
  -> Ptr Word64
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_unit
  :: Ptr DB
  -> UnitId
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

foreign import ccall safe glean_rocksdb_store_ownership
  :: Ptr DB
  -> Ptr ComputedOwnership
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_ownership
  :: Ptr DB
  -> Ptr (Ptr Ownership)
  -> IO CString

foreign import ccall safe glean_rocksdb_add_define_ownership
  :: Ptr DB
  -> Ptr DefineOwnership
  -> IO CString

foreign import ccall unsafe glean_rocksdb_get_derived_fact_ownership_iterator
  :: Ptr DB
  -> Word64
  -> Ptr Ownership.DerivedFactOwnershipIterator
  -> IO CString

foreign import ccall unsafe glean_rocksdb_cache_ownership
  :: Ptr DB
  -> IO CString

foreign import ccall safe glean_rocksdb_prepare_fact_owner_cache
  :: Ptr DB
  -> IO CString

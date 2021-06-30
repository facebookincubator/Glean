-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.Ownership
  ( UnitIterator
  , compute
  , UnitId(..)
  , Ownership
  , MemoryOwnership
  , toOwnership
  , Slice
  , slice
  , sliced
  , Sliced
  ) where

import Control.Exception
import Data.Coerce
import Foreign hiding (with)
import Foreign.C

import Glean.FFI
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup

newtype UnitIterator = UnitIterator (Ptr UnitIterator)
  deriving(Storable)

instance Static UnitIterator where
  destroyStatic = glean_ownership_unit_iterator_free

newtype UnitId = UnitId Word32

newtype Ownership = Ownership (ForeignPtr Ownership)

instance Object Ownership where
  wrap = Ownership
  unwrap (Ownership p) = p
  destroy = glean_ownership_free

newtype MemoryOwnership = MemoryOwnership (ForeignPtr MemoryOwnership)

instance Object MemoryOwnership where
  wrap = MemoryOwnership
  unwrap (MemoryOwnership p) = p
  destroy = glean_memory_ownership_free

toOwnership :: MemoryOwnership -> Ownership
toOwnership (MemoryOwnership fp) = Ownership (castForeignPtr fp)

compute
  :: CanLookup a
  => Inventory
  -> a
  -> UnitIterator
  -> IO MemoryOwnership
compute inv l iter =
  with inv $ \inv_ptr ->
  withLookup l $ \lookup ->
  construct $ invoke $ glean_ownership_compute inv_ptr lookup iter

newtype Slice = Slice (ForeignPtr Slice)

instance Object Slice where
  wrap = Slice
  unwrap (Slice p) = p
  destroy = glean_slice_free

-- | Construct a slice view of a DB from a set of UnitIds
slice :: Ownership -> [UnitId] -> Bool -> IO Slice
slice ownership units exclude =
  with ownership $ \ownership_ptr ->
  withArrayLen (coerce units) $ \unit_arr_size unit_arr ->
  construct $ invoke $ glean_slice_compute
    ownership_ptr
    unit_arr
    (fromIntegral unit_arr_size)
    (fromIntegral (fromEnum exclude))

data Sliced base = Sliced Ownership Slice base

sliced :: CanLookup base => Ownership -> Slice -> base -> Sliced base
sliced = Sliced

instance CanLookup base => CanLookup (Sliced base) where
  withLookup (Sliced ownership slice base) f =
    withLookup base $ \p_base ->
    with ownership $ \ownership_ptr ->
    with slice $ \slice_ptr ->
    bracket
      (invoke $ glean_make_sliced p_base ownership_ptr slice_ptr)
      glean_sliced_free
      f

foreign import ccall unsafe glean_ownership_unit_iterator_free
  :: UnitIterator -> IO ()

foreign import ccall safe glean_ownership_compute
  :: Ptr Inventory
  -> Lookup
  -> UnitIterator
  -> Ptr (Ptr MemoryOwnership)
  -> IO CString

foreign import ccall unsafe "&glean_ownership_free"
   glean_ownership_free :: FunPtr (Ptr Ownership -> IO ())

foreign import ccall unsafe "&glean_ownership_free"
   glean_memory_ownership_free :: FunPtr (Ptr MemoryOwnership -> IO ())

foreign import ccall safe glean_slice_compute
  :: Ptr Ownership
  -> Ptr Word32
  -> CSize
  -> CInt
  -> Ptr (Ptr Slice)
  -> IO CString

foreign import ccall unsafe "&glean_slice_free"
   glean_slice_free :: FunPtr (Ptr Slice -> IO ())

foreign import ccall unsafe glean_make_sliced
  :: Lookup
  -> Ptr Ownership
  -> Ptr Slice
  -> Ptr Lookup
  -> IO CString

foreign import ccall unsafe
   glean_sliced_free :: Lookup -> IO ()

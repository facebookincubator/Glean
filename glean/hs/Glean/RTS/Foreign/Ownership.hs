-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.Ownership
  ( UnitIterator
  , compute
  , UnitId(..)
  , UsetId(..)
  , Ownership
  , ComputedOwnership
  , Slice
  , slice
  , sliced
  , Sliced
  , newDefineOwnership
  , DefineOwnership
  , substDefineOwnership
  , DerivedFactOwnershipIterator
  , computeDerivedOwnership
  ) where

import Control.Exception
import Data.Coerce
import Foreign hiding (with)
import Foreign.C

import Glean.FFI
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup
import Glean.RTS.Foreign.Subst
import Glean.RTS.Types

newtype UnitIterator = UnitIterator (Ptr UnitIterator)
  deriving(Storable)

instance Static UnitIterator where
  destroyStatic = glean_ownership_unit_iterator_free

-- | Id of a unit
newtype UnitId = UnitId Word32
  deriving (Storable)

-- | Id of an ownership set
newtype UsetId = UsetId Word32
  deriving (Storable)

newtype Ownership = Ownership (ForeignPtr Ownership)

instance Object Ownership where
  wrap = Ownership
  unwrap (Ownership p) = p
  destroy = glean_ownership_free

newtype ComputedOwnership = ComputedOwnership (ForeignPtr ComputedOwnership)

instance Object ComputedOwnership where
  wrap = ComputedOwnership
  unwrap (ComputedOwnership p) = p
  destroy = glean_computed_ownership_free

compute
  :: CanLookup a
  => Inventory
  -> a
  -> UnitIterator
  -> IO ComputedOwnership
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

newtype DefineOwnership = DefineOwnership (ForeignPtr DefineOwnership)

instance Object DefineOwnership where
  wrap = DefineOwnership
  unwrap (DefineOwnership p) = p
  destroy = glean_define_ownership_free

newDefineOwnership :: Ownership -> Pid -> IO DefineOwnership
newDefineOwnership ownership (Pid pid) =
  with ownership $ \ownership_ptr ->
    construct $ invoke $
      glean_new_define_ownership ownership_ptr (fromIntegral pid)

substDefineOwnership :: DefineOwnership -> Subst -> IO ()
substDefineOwnership define subst =
  with define $ \define_ptr ->
  with subst $ \subst_ptr ->
    invoke $ glean_define_ownership_subst define_ptr subst_ptr

newtype DerivedFactOwnershipIterator =
  DerivedFactOwnershipIterator (Ptr DerivedFactOwnershipIterator)
  deriving(Storable)

instance Static DerivedFactOwnershipIterator where
  destroyStatic = glean_derived_fact_ownership_iterator_free

computeDerivedOwnership
  :: Ownership
  -> DerivedFactOwnershipIterator
  -> IO ComputedOwnership
computeDerivedOwnership ownership iter =
  with ownership $ \ownership_ptr ->
    construct $ invoke $ glean_derived_ownership_compute ownership_ptr iter

foreign import ccall unsafe glean_new_define_ownership
  :: Ptr Ownership
  -> Word64
  -> Ptr (Ptr DefineOwnership)
  -> IO CString

foreign import ccall unsafe "&glean_define_ownership_free"
  glean_define_ownership_free :: FunPtr (Ptr DefineOwnership -> IO ())

foreign import ccall unsafe glean_define_ownership_subst
  :: Ptr DefineOwnership
  -> Ptr Subst
  -> IO CString

foreign import ccall unsafe glean_ownership_unit_iterator_free
  :: UnitIterator -> IO ()

foreign import ccall unsafe glean_derived_fact_ownership_iterator_free
  :: DerivedFactOwnershipIterator -> IO ()

foreign import ccall safe glean_ownership_compute
  :: Ptr Inventory
  -> Lookup
  -> UnitIterator
  -> Ptr (Ptr ComputedOwnership)
  -> IO CString

foreign import ccall safe glean_derived_ownership_compute
  :: Ptr Ownership
  -> DerivedFactOwnershipIterator
  -> Ptr (Ptr ComputedOwnership)
  -> IO CString

foreign import ccall unsafe "&glean_ownership_free"
   glean_ownership_free :: FunPtr (Ptr Ownership -> IO ())

foreign import ccall unsafe "&glean_computed_ownership_free"
   glean_computed_ownership_free :: FunPtr (Ptr ComputedOwnership -> IO ())

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

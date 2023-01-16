{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Ownership
  ( UnitIterator
  , compute
  , UnitId(..)
  , UsetId(..)
  , firstUsetId
  , Ownership
  , ComputedOwnership
  , Slice
  , slice
  , slicedStack
  , SlicedStack
  , newDefineOwnership
  , DefineOwnership
  , substDefineOwnership
  , defineOwnershipSortByOwner
  , DerivedFactOwnershipIterator
  , computeDerivedOwnership
  , getFactOwner
  , SetOp(..)
  , getOwnershipSet
  , OwnershipStats(..)
  , getOwnershipStats
  , showOwnershipStats
  , nextUsetId
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce
import qualified Data.Vector.Storable as VS
import Data.Text (Text)
import Foreign hiding (with)
import Foreign.C
import TextShow

import Foreign.CPP.HsStruct
import Foreign.CPP.Marshallable
import Util.FFI
import Util.PrettyPrint

import Glean.FFI hiding (withMany)
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
  deriving (Storable, Show)

-- | Id of an ownership set
newtype UsetId = UsetId Word32
  deriving (Storable)

firstUsetId :: UsetId
firstUsetId = UsetId 0

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
  :: (CanLookup db, CanLookup base)
  => Inventory
  -> db
  -> Maybe base
     -- ^ base DB if there is one
  -> UnitIterator
  -> IO ComputedOwnership
compute inv l base iter =
  with inv $ \inv_ptr ->
  withLookup l $ \lookup ->
  maybe ($ nullPtr) withLookup base $ \base_lookup_ptr ->
  construct $ invoke $
    glean_ownership_compute inv_ptr lookup iter base_lookup_ptr

newtype Slice = Slice (ForeignPtr Slice)

instance Object Slice where
  wrap = Slice
  unwrap (Slice p) = p
  destroy = glean_slice_free

-- | Construct a slice view of a DB from a set of UnitIds
slice :: Ownership -> [Slice] -> [UnitId] -> Bool -> IO Slice
slice ownership bases units exclude =
  with ownership $ \ownership_ptr ->
  withArrayLen (coerce units) $ \unit_arr_size unit_arr ->
  withMany with bases $ \bases_ptrs ->
  withArrayLen bases_ptrs $ \bases_arr_size bases_arr ->
  construct $ invoke $ glean_slice_compute
    ownership_ptr
    unit_arr
    (fromIntegral unit_arr_size)
    (fromIntegral (fromEnum exclude))
    bases_arr
    (fromIntegral bases_arr_size)

data SlicedStack base = SlicedStack [Slice] base

slicedStack
  :: CanLookup base
  => [Slice]
  -> base
  -> SlicedStack base
slicedStack = SlicedStack

instance CanLookup base => CanLookup (SlicedStack base) where
  lookupName (SlicedStack _ base) = "slicedstack:" <> lookupName base
  withLookup (SlicedStack list base) f =
    withLookup base $ \p_base ->
    withMany with list $ \slices ->
    withArray slices $ \slices_arr ->
    bracket
      (invoke $ glean_make_sliced_stack
        p_base (fromIntegral (length list)) slices_arr)
      glean_sliced_stack_free
      f

newtype DefineOwnership = DefineOwnership (ForeignPtr DefineOwnership)

instance Object DefineOwnership where
  wrap = DefineOwnership
  unwrap (DefineOwnership p) = p
  destroy = glean_define_ownership_free

newDefineOwnership :: Ownership -> Pid -> Fid -> IO DefineOwnership
newDefineOwnership ownership (Pid pid) (Fid first_id) =
  with ownership $ \ownership_ptr ->
    construct $ invoke $
      glean_new_define_ownership ownership_ptr
        (fromIntegral pid)
        (fromIntegral first_id)

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
  :: (CanLookup base)
  => Ownership
  -> Maybe base
  -> DerivedFactOwnershipIterator
  -> IO ComputedOwnership
computeDerivedOwnership ownership base iter =
  with ownership $ \ownership_ptr ->
  maybe ($ nullPtr) withLookup base $ \base_lookup_ptr ->
    construct $ invoke $
      glean_derived_ownership_compute ownership_ptr base_lookup_ptr iter

defineOwnershipSortByOwner
  :: DefineOwnership
  -> Int
  -> IO (VS.Vector Int64)
defineOwnershipSortByOwner define count =
  with define $ \define_ptr -> do
  withDefaultCxxObject $ \arr_ptr -> do
    invoke $ glean_define_ownership_sort_by_owner define_ptr
      (fromIntegral count) arr_ptr
    hsArrayStorable <$> peek (castPtr arr_ptr)

#include <glean/rts/ownership/uset.h>
#include <glean/rts/ownership.h>

getFactOwner :: CanLookup a => a -> Fid -> IO (Maybe UsetId)
getFactOwner lookup (Fid fid) =
  withLookup lookup $ \p_lookup -> do
    usetId <- invoke $ glean_get_fact_owner p_lookup (fromIntegral fid)
    if usetId == (#const facebook::glean::rts::INVALID_USET)
      then return Nothing
      else return (Just (UsetId usetId))

data SetOp = Or | And
  deriving (Eq, Ord, Enum)

getOwnershipSet :: Ownership -> UsetId -> IO (Maybe (SetOp, VS.Vector UsetId))
getOwnershipSet ownership usetid =
  with ownership $ \ownership_ptr ->
  bracket
    (invoke $ glean_get_ownership_set ownership_ptr usetid)
    (\(_, arr_ptr) -> when (arr_ptr /= nullPtr) $ delete arr_ptr)
    (\(cop, arr_ptr) -> do
      if arr_ptr == nullPtr
        then return Nothing
        else do
          vec <- hsArrayStorable <$> peek (castPtr arr_ptr)
          let op | cop == (#const facebook::glean::rts::Or) = Or
                 | cop == (#const facebook::glean::rts::And) = And
                 | otherwise = error "unkonwn SetOp"
          return $ Just (op, coerce (vec :: VS.Vector Word32))
    )

data OwnershipStats = OwnershipStats
  { numUnits :: Word64
  , unitsSize :: Word64
  , numSets :: Word64
  , setsSize :: Word64
  , numOwnerEntries :: Word64
  , ownersSize :: Word64
  }

showOwnershipStats :: OwnershipStats -> Text
showOwnershipStats OwnershipStats{..} =
  showt numUnits <> " units (" <>
    renderBytes (fromIntegral unitsSize) <> "), " <>
  showt numSets <> " sets (" <>
    renderBytes (fromIntegral setsSize) <> "), " <>
  showt numOwnerEntries <> " owners (" <>
    renderBytes (fromIntegral ownersSize) <> ")"

instance Storable OwnershipStats where
  peek p = do
    numUnits <- (# peek facebook::glean::rts::OwnershipStats, num_units) p
    unitsSize <- (# peek facebook::glean::rts::OwnershipStats, units_size) p
    numSets <- (# peek facebook::glean::rts::OwnershipStats, num_sets) p
    setsSize <- (# peek facebook::glean::rts::OwnershipStats, sets_size) p
    numOwnerEntries <- (# peek facebook::glean::rts::OwnershipStats, num_owner_entries) p
    ownersSize <- (# peek facebook::glean::rts::OwnershipStats, owners_size) p
    return OwnershipStats{..}
  sizeOf _ = (# size facebook::glean::rts::OwnershipStats)
  alignment _ = (# alignment facebook::glean::rts::OwnershipStats)
  poke _ = error "Storable OwnershipStats"

getOwnershipStats :: Ownership -> IO OwnershipStats
getOwnershipStats ownership =
  with ownership $ \ownership_ptr -> do
    invoke $ glean_get_ownership_stats ownership_ptr

nextUsetId :: Ownership -> IO UsetId
nextUsetId ownership =
  with ownership $ \ownership_ptr -> do
    invoke $ glean_ownership_next_set_id ownership_ptr

foreign import ccall unsafe glean_get_ownership_stats
  :: Ptr Ownership
  -> Ptr OwnershipStats
  -> IO CString

foreign import ccall unsafe glean_new_define_ownership
  :: Ptr Ownership
  -> Word64
  -> Word64
  -> Ptr (Ptr DefineOwnership)
  -> IO CString

foreign import ccall unsafe "&glean_define_ownership_free"
  glean_define_ownership_free :: FunPtr (Ptr DefineOwnership -> IO ())

foreign import ccall unsafe glean_define_ownership_subst
  :: Ptr DefineOwnership
  -> Ptr Subst
  -> IO CString

foreign import ccall unsafe glean_define_ownership_sort_by_owner
  :: Ptr DefineOwnership
  -> Word64
  -> Ptr (HsArray Int64)
  -> IO CString

foreign import ccall unsafe glean_ownership_unit_iterator_free
  :: UnitIterator -> IO ()

foreign import ccall unsafe glean_derived_fact_ownership_iterator_free
  :: DerivedFactOwnershipIterator -> IO ()

foreign import ccall safe glean_ownership_compute
  :: Ptr Inventory
  -> Ptr Lookup
  -> UnitIterator
  -> Ptr Lookup
  -> Ptr (Ptr ComputedOwnership)
  -> IO CString

foreign import ccall unsafe glean_get_fact_owner
  :: Ptr Lookup
  -> Word64
  -> Ptr Word32
  -> IO CString

foreign import ccall unsafe glean_get_ownership_set
  :: Ptr Ownership
  -> UsetId
  -> Ptr CInt
  -> Ptr (Ptr (HsArray Word32))
  -> IO CString

foreign import ccall safe glean_derived_ownership_compute
  :: Ptr Ownership
  -> Ptr Lookup
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
  -> CSize
  -> Ptr (Ptr Slice)
  -> IO CString

foreign import ccall unsafe "&glean_slice_free"
   glean_slice_free :: FunPtr (Ptr Slice -> IO ())

foreign import ccall unsafe glean_make_sliced_stack
  :: Ptr Lookup
  -> CSize
  -> Ptr (Ptr Slice)
  -> Ptr (Ptr Lookup)
  -> IO CString

foreign import ccall unsafe
   glean_sliced_stack_free :: Ptr Lookup -> IO ()

foreign import ccall unsafe
   glean_ownership_next_set_id :: Ptr Ownership -> Ptr UsetId -> IO CString

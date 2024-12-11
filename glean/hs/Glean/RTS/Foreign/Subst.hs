{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Subst
  ( Subst
  , empty
  , serialize
  , substIntervals
  , unsafeSubstIntervalsAndRelease
  , subst
  , substVector
  , deserialize
  , substOffset
  ) where

import Control.Exception
import Data.Int
import Data.Word
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe

import Util.FFI

import Glean.FFI
import Glean.RTS.Types (Fid(..), lowestFid)
import qualified Glean.Types as Thrift

newtype Subst = Subst (ForeignPtr Subst)

instance Object Subst where
  wrap = Subst
  unwrap (Subst p) = p
  destroy = glean_free_subst

instance Semigroup Subst where
  s <> t = unsafePerformIO $ with s $ \s_ptr -> with t $ \t_ptr ->
    construct $ invoke $ glean_subst_compose s_ptr t_ptr

instance Monoid Subst where
  mempty = empty

empty :: Subst
{-# NOINLINE empty #-}
empty = unsafePerformIO $ construct $ invoke $ glean_new_subst lowestFid 0

serialize :: Subst -> Thrift.Subst
serialize subst = unsafePerformIO $
  with subst $ \subst_ptr -> do
    (Fid id,count,p) <- invoke $ glean_serialize_subst subst_ptr
    vec <- unsafeMallocedVector p count
    return $ Thrift.Subst id vec

substOffset :: Subst -> Int
substOffset subst = fromIntegral $ unsafeDupablePerformIO $
  with subst glean_subst_offset

-- | Apply the given substitution to one Fid
subst :: Subst -> Fid -> Fid
subst subst old = unsafePerformIO $
  with subst $ \subst_ptr ->
  mask_ $ invoke $ glean_subst_subst subst_ptr old

-- | Apply the given subsitution to all the Fids in the vector
substVector :: Subst -> VS.Vector Fid -> IO (VS.Vector Fid)
substVector subst vec =
  with subst $ \subst_ptr ->
  VS.unsafeWith vec $ \vec_ptr -> mask_ $ do
    (outs_ptr, outs_size) <- invoke $ glean_subst_vector
      subst_ptr
      vec_ptr
      (fromIntegral $ VS.length vec)
    unsafeMallocedVector outs_ptr outs_size

-- | Apply the given substitution to the intervals
substIntervals :: Subst -> VS.Vector Fid -> IO (VS.Vector Fid)
substIntervals subst ins =
  with subst $ \subst_ptr ->
  VS.unsafeWith ins $ \ins_ptr -> mask_ $ do
    (outs_ptr, outs_size) <- invoke $ glean_subst_intervals
      subst_ptr
      ins_ptr
      (fromIntegral $ VS.length ins)
    unsafeMallocedVector outs_ptr outs_size

-- | Like 'substIntervals', but also releases the input vector
-- immediately, rather than waiting for the GC to run. The input
-- vector must never be used again.
unsafeSubstIntervalsAndRelease :: Subst -> VS.Vector Fid -> IO (VS.Vector Fid)
unsafeSubstIntervalsAndRelease subst ins = do
  new <- substIntervals subst ins
  let !(fp, _) = VS.unsafeToForeignPtr0 ins
  finalizeForeignPtr fp
  return new

deserialize :: Thrift.Subst -> IO Subst
deserialize Thrift.Subst{..} =
  VS.unsafeWith subst_ids $ \ids_ptr ->
    construct $ invoke $
      glean_subst_deserialize
        (Fid subst_firstId)
        (fromIntegral $ VS.length subst_ids)
        ids_ptr

foreign import ccall unsafe glean_new_subst
  :: Fid -> CSize -> Ptr (Ptr Subst) -> IO CString
foreign import ccall unsafe "&glean_free_subst" glean_free_subst
  :: FunPtr (Ptr Subst -> IO ())

foreign import ccall unsafe glean_subst_compose
  :: Ptr Subst
  -> Ptr Subst
  -> Ptr (Ptr Subst)
  -> IO CString

foreign import ccall safe glean_serialize_subst
  :: Ptr Subst
  -> Ptr Fid
  -> Ptr CSize
  -> Ptr (Ptr Int64)
  -> IO CString

foreign import ccall safe glean_subst_deserialize
  :: Fid
  -> CSize
  -> Ptr Int64
  -> Ptr (Ptr Subst)
  -> IO CString

foreign import ccall safe glean_subst_intervals
  :: Ptr Subst
  -> Ptr Fid
  -> CSize
  -> Ptr (Ptr Fid)
  -> Ptr CSize
  -> IO CString

foreign import ccall unsafe glean_subst_subst
  :: Ptr Subst
  -> Fid
  -> Ptr Fid
  -> IO CString

foreign import ccall safe glean_subst_vector
  :: Ptr Subst
  -> Ptr Fid
  -> CSize
  -> Ptr (Ptr Fid)
  -> Ptr CSize
  -> IO CString

foreign import ccall unsafe glean_subst_offset
  :: Ptr Subst
  -> IO Word64

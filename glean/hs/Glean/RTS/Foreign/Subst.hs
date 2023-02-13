{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Subst (
  Subst, empty, serialize, substIntervals, rebaseIntervals, deserialize,
) where

import Control.Exception
import Data.Int
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

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

-- | Apply the given substitution to the intervals
substIntervals :: Subst -> VS.Vector Fid -> VS.Vector Fid
substIntervals subst ins = substIntervals_ subst False ins

-- | Apply the given substitution to the intervals, and also rename
-- Ids in the same way as FactSet.rebase. For use when rebasing a
-- FactSet with respect to a substitution.
rebaseIntervals :: Subst -> VS.Vector Fid -> VS.Vector Fid
rebaseIntervals subst ins = substIntervals_ subst True ins

substIntervals_ :: Subst -> Bool -> VS.Vector Fid -> VS.Vector Fid
substIntervals_ subst rebase ins = unsafePerformIO $
  with subst $ \subst_ptr ->
  VS.unsafeWith ins $ \ins_ptr -> mask_ $ do
    (outs_ptr, outs_size) <- invoke $ glean_subst_intervals
      subst_ptr
      (fromIntegral (fromEnum rebase))
      ins_ptr
      (fromIntegral $ VS.length ins)
    unsafeMallocedVector outs_ptr outs_size

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

foreign import ccall unsafe glean_serialize_subst
  :: Ptr Subst
  -> Ptr Fid
  -> Ptr CSize
  -> Ptr (Ptr Int64)
  -> IO CString

foreign import ccall unsafe glean_subst_deserialize
  :: Fid
  -> CSize
  -> Ptr Int64
  -> Ptr (Ptr Subst)
  -> IO CString

foreign import ccall safe glean_subst_intervals
  :: Ptr Subst
  -> CBool
  -> Ptr Fid
  -> CSize
  -> Ptr (Ptr Fid)
  -> Ptr CSize
  -> IO CString

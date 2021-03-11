module Glean.RTS.Foreign.Subst (
  Subst, empty, serialize
) where

import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

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

-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.Stacked
  ( Stacked
  , stacked
  ) where

import Control.Exception (bracket)
import Foreign.C.String
import Foreign.Ptr

import Glean.FFI
import Glean.RTS.Foreign.Define (Define(..), CanDefine(..))
import Glean.RTS.Foreign.Lookup (Lookup(..), CanLookup(..))

-- | An `a` stacked on top of a base 'CanLookup'.
data Stacked a = forall base. CanLookup base => Stacked base a

-- | Stack a value on top of a base 'CanLookup'
stacked :: CanLookup base => base -> a -> Stacked a
stacked = Stacked

instance CanLookup a => CanLookup (Stacked a) where
  withLookup (Stacked base added) f =
    withLookup base $ \p_base ->
    withLookup added $ \p_added ->
    bracket
      (invoke $ glean_stacked_lookup_new p_base p_added)
      glean_lookup_free
      f

instance CanDefine a => CanDefine (Stacked a) where
  withDefine (Stacked base added) f =
    withLookup base $ \p_base ->
    withDefine added $ \p_added ->
    bracket
      (invoke $ glean_stacked_define_new p_base p_added)
      glean_stacked_define_free
      f

foreign import ccall unsafe glean_stacked_lookup_new
  :: Lookup -> Lookup -> Ptr Lookup -> IO CString
foreign import ccall unsafe glean_lookup_free
  :: Lookup -> IO ()

foreign import ccall unsafe glean_stacked_define_new
  :: Lookup -> Define -> Ptr Define -> IO CString
foreign import ccall unsafe glean_stacked_define_free
  :: Define -> IO ()

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Stacked
  ( Stacked
  , stacked
  ) where

import Control.Exception (bracket)
import Data.Text
import Foreign.C.String
import Foreign.Ptr

import Util.FFI

import Glean.RTS.Foreign.Define (Define(..), CanDefine(..))
import Glean.RTS.Foreign.Lookup (Lookup(..), CanLookup(..))

-- | An `a` stacked on top of a base 'CanLookup'.
data Stacked a = forall base. CanLookup base => Stacked base a Text

-- | Stack a value on top of a base 'CanLookup'
stacked :: (CanLookup base, CanLookup a) => base -> a -> Stacked a
stacked base l = Stacked base l name
  where name = lookupName base <> ";" <> lookupName l

instance CanLookup a => CanLookup (Stacked a) where
  lookupName (Stacked _ _ name) = name
  withLookup (Stacked base added _) f =
    withLookup base $ \p_base ->
    withLookup added $ \p_added ->
    bracket
      (invoke $ glean_stacked_lookup_new p_base p_added)
      glean_lookup_free
      f

instance CanDefine a => CanDefine (Stacked a) where
  withDefine (Stacked base added _) f =
    withLookup base $ \p_base ->
    withDefine added $ \p_added ->
    bracket
      (invoke $ glean_stacked_define_new p_base p_added)
      glean_stacked_define_free
      f

foreign import ccall unsafe glean_stacked_lookup_new
  :: Ptr Lookup -> Ptr Lookup -> Ptr (Ptr Lookup) -> IO CString
foreign import ccall unsafe glean_lookup_free
  :: Ptr Lookup -> IO ()

foreign import ccall unsafe glean_stacked_define_new
  :: Ptr Lookup -> Define -> Ptr Define -> IO CString
foreign import ccall unsafe glean_stacked_define_free
  :: Define -> IO ()

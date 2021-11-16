{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Support for base types
module Glean.Typed.Prim
  ( fromNat, toNat,
  ) where

import Data.Word

import Glean.Types


fromNat :: Nat -> Word64
fromNat = fromIntegral . unNat

toNat :: Word64 -> Nat
toNat = Nat . fromIntegral

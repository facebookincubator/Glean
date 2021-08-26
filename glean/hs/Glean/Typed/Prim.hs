-- Copyright (c) Facebook, Inc. and its affiliates.


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

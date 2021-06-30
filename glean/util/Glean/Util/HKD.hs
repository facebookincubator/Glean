-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeFamilies #-}
-- | Useful for options, parsing, see
-- <http://reasonablypolymorphic.com/blog/higher-kinded-data/>
module Glean.Util.HKD
  ( HKD, Identity
  ) where

import Data.Functor.Identity ( Identity )

-- | Closed type family
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

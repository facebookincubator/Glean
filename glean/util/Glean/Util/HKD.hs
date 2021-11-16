{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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

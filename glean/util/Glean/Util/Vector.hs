{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Util.Vector (
    unsafeCoerceVector
  ) where

import Data.Vector.Storable
#if !MIN_VERSION_vector(0,13,0)
import Data.Coerce
#endif

#if !MIN_VERSION_vector(0,13,0)
unsafeCoerceVector :: Coercible a b => Vector a -> Vector b
unsafeCoerceVector = coerce
#endif

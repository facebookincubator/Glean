{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module B
  ( b
  ) where

import A
import Data.Char(ord)

b :: [Int]
b = reverse $ map ord a

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module B
  ( b
  , r
  ) where

import A
import Data.Char(ord)

b :: [Int]
b = reverse $ map ord a

r :: R Char
r = A.R { f1 = 'a', f2 = "abc" }

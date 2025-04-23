{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module A
  ( a
  , A
  , T(..)
  , R(..)
  , C(..)
  , zero
  , f
  ) where

import Data.Char (toLower, ord)

a :: String
a = map toLower "A"

type A = Int

data T a = C1 Float | C2 A | C3 a

data R a = R { f1 :: Char, f2 :: [a] }

class Eq a => C a where
  m :: a -> Bool

instance C Int where
  m x = x == x

zero :: A
zero = 0

f :: R a -> T a
f R{f2 = [x]} = C3 x
f r | m (3::Int) = C2 (ord (f1 r))


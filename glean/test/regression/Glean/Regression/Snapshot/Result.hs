{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Snapshot.Result (Result(..), failure)
where

data Result
  = Success [FilePath]
  | Failure ([String] -> [String])

instance Semigroup Result where
  Success paths1 <> Success paths2 = Success (paths1 ++ paths2)
  Success _ <> x = x
  x <> Success _ = x
  Failure f <> Failure g = Failure $ f . g

failure :: String -> Result
failure s = Failure (s:)

instance Monoid Result where
  mempty = Success []

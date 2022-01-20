-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Glean.Regression.Snapshot.Result (Result(..), failure)
where

data Result
  = Success
  | Failure ([String] -> [String])

instance Semigroup Result where
  Success <> Success = Success
  Success <> x = x
  x <> Success = x
  Failure f <> Failure g = Failure $ f . g

failure :: String -> Result
failure s = Failure (s:)

instance Monoid Result where
  mempty = Success

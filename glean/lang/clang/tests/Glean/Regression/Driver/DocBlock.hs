-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Regression.Driver.DocBlock (main) where

import Control.Monad

import qualified Glean.Clang.Test as Clang (driver)
import qualified Glean.DocBlock.Test as DocBlock (driver)
import Glean.Regression.Config (Driver(..))
import Glean.Regression.Test (testMain)
import Glean.Derive (derivePredicate)
import Glean.Write (parseRef)

combined :: Driver
combined = Clang.driver
  { driverGenerator = \ test backend repo -> do
      driverGenerator Clang.driver test backend repo
      driverGenerator DocBlock.driver test backend repo
      forM_ passes $ \predicate ->
        derivePredicate backend repo Nothing Nothing predicate Nothing
  }
  where
  passes = map parseRef
    [ "docmarkup.EntityByDocAttrKey"
    ]

main :: IO ()
main = testMain combined

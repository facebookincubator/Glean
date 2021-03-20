module Glean.Regression.Driver.DocBlock (main) where

import qualified Glean.Clang.Test as Clang (driver)
import qualified Glean.DocBlock.Test as DocBlock (driver)
import Glean.Regression.Config (Driver(..))
import Glean.Regression.Test (testMain)

combined :: Driver
combined = Clang.driver
  { driverGenerator = \ test backend repo -> do
      driverGenerator Clang.driver test backend repo
      driverGenerator DocBlock.driver test backend repo
  }

main :: IO ()
main = testMain combined

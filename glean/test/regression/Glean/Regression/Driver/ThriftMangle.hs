module Glean.Regression.Driver.ThriftMangle (main) where

import qualified Derive.ThriftMangle.RegressionTest (driver)
import Glean.Regression.Test (testMain)

main :: IO ()
main = testMain Derive.ThriftMangle.RegressionTest.driver

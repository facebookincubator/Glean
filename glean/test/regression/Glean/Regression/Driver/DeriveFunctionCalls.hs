module Glean.Regression.Driver.DeriveFunctionCalls (main) where

import Derive.Types (DerivePass(..))
import Glean.Clang.Test.DerivePass (driver)
import Glean.Regression.Test (testMain)

main :: IO ()
main = testMain $ driver [DeriveFunctionCalls, DeriveFunctionCalls_Pass_2]

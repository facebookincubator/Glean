module Glean.Regression.Driver.DeriveDeclFamilies (main) where

import Derive.Types (DerivePass(..))
import Glean.Clang.Test.DerivePass (driver)
import Glean.Regression.Test (testMain)

main :: IO ()
main = testMain $ driver [DeriveDeclFamilies]

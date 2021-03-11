module Glean.Regression.Driver.DeriveObjcInheritance(main) where

import Derive.Types (DerivePass(..))
import Glean.Clang.Test.DerivePass (driver)
import Glean.Regression.Test (testMain)

main :: IO ()
main = testMain $ driver [DeriveGeneric "cxx1.ObjcContainerInheritance"]

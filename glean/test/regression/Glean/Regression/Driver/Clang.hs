module Glean.Regression.Driver.Clang (main) where

import qualified Glean.Clang.Test as Clang
import Glean.Regression.Test

main :: IO ()
main = testMain Clang.driver

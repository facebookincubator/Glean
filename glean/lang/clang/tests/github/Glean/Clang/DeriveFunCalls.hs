module Glean.Clang.DeriveFunCalls where

import System.Environment

import qualified Glean.Regression.Driver.DeriveFunctionCalls as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path]) D.main

  where
    path = "glean/lang/clang/tests/regression_derive-function-calls"

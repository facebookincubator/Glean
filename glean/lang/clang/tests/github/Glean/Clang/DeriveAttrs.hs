module Glean.Clang.DeriveAttrs where

import System.Environment

import qualified Glean.Regression.Driver.DeriveFunctionDeclAttribute as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path]) D.main

  where
    path = "glean/lang/clang/tests/regression_derive-attributes"

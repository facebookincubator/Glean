module Glean.Clang.DeriveByName where

import System.Environment

import qualified Glean.Regression.Driver.DeriveDeclByName as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path]) D.main

  where
    path = "glean/lang/clang/tests/regression_derive-decl-by-name"

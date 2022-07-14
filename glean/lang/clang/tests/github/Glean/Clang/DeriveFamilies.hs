module Glean.Clang.DeriveFamilies where

import System.Environment

import qualified Glean.Regression.Driver.DeriveDeclFamilies as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path]) D.main

  where
    path = "glean/lang/clang/tests/regression_derive-decl-families"

module Glean.Clang.CodeMarkup where

import System.Environment

import qualified Glean.Regression.Driver.DeriveForCodemarkup as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path]) D.main

  where
    path = "glean/lang/codemarkup/tests/clang"

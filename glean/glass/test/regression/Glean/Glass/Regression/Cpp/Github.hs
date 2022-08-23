module Glean.Glass.Regression.Cpp.Github (main) where

import qualified Glean.Glass.Regression.Cpp as Cpp
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  withArgs
    (["--root", "glean/lang/codemarkup/tests/clang/glass/namespace1"] ++ args)
    Cpp.main

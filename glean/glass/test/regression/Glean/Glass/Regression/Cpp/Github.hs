{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Cpp.Github (main) where

import qualified Glean.Glass.Regression.Cpp as Cpp
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  withArgs
    (["--root", "glean/lang/codemarkup/tests/clang/glass/namespace1"] ++ args)
    Cpp.main

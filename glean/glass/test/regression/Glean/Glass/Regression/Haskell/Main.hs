{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Haskell.Main ( main ) where

import System.Environment
import qualified Glean.Glass.Regression.Haskell as Glass

main :: IO ()
main = do
  args <- getArgs
  withArgs (["--root", path, "--with-ghc", "ghc"] ++ args) Glass.main
  where
    path = "glean/lang/codemarkup/tests/haskell/code"

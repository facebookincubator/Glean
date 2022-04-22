{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Main ( main ) where

import System.Environment ( withArgs )

import qualified Driver ( main )

main :: IO ()
main = withArgs ["--root", path] Driver.main
  where
    path = "glean/lang/hack/tests/cases"

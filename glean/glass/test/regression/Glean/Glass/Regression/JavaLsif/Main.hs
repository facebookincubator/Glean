{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.JavaLsif.Main ( main ) where

import System.Environment
import qualified Glean.Glass.Regression.JavaLsif as Glass

main :: IO ()
main = getArgs >>= \args -> withArgs (args ++ ["--root", path]) Glass.main
  where
    path = "glean/lang/java-lsif/tests/cases/xrefs"

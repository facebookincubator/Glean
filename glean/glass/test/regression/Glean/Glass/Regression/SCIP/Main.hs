{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.SCIP.Main ( main ) where

import System.Environment
import qualified Glean.Glass.Regression.SCIP as Glass

main :: IO ()
main = getArgs >>= \args -> withArgs (["--input", path] ++ args) Glass.main
  where
    path = "glean/lang/scip/tests/cases/index.scip"

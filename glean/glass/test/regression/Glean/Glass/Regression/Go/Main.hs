{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Go.Main ( main ) where

import System.Environment
import qualified Glean.Glass.Regression.Go as Glass
import qualified Glean.LSIF.Driver as LSIF

main :: IO ()
main = withArgs (LSIF.testArgs path LSIF.Go) Glass.main
  where
    path = "glean/lang/go/tests/cases/xrefs"

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.PythonScip.Main ( main ) where

import System.Environment

import Glean.Indexer.PythonScip as PythonScip
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = getArgs >>= \args ->
    withArgs (args ++ ["--scip-to-glean", scipToGlean, "--root", path]) $
      testMain (driverFromIndexer PythonScip.indexer)
  where
    path = "glean/lang/python-scip/tests/cases"
    scipToGlean =
      "glean/lang/scip/indexer/scip_to_glean/target/release/scip-to-glean"

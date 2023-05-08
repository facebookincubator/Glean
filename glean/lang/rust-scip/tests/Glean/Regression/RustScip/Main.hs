{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.RustScip.Main ( main ) where

import System.Environment

import Glean.Indexer.RustScip as RustScip
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = getArgs >>= \args -> withArgs (args ++ ["--root", path]) $
    testMain (driverFromIndexer RustScip.indexer)
  where
    -- different query outputs so copy of the lsif index cases and queries
    path = "glean/lang/rust-scip/tests/cases"

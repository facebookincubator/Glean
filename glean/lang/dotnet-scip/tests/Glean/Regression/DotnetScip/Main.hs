{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.DotnetScip.Main ( main ) where

import System.Environment

import Glean.Indexer.DotnetScip as DotnetScip
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = getArgs >>= \args -> withArgs (args ++ ["--root", path]) $
    testMain (driverFromIndexer DotnetScip.indexer)
  where
    path = "glean/lang/dotnet-scip/tests/cases"
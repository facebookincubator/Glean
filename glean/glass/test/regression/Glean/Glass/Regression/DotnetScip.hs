{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.DotnetScip (main) where

import Glean.Glass.Regression.Snapshot as Glass
import Glean.Indexer.DotnetScip as Dotnet

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-dotnet"
    testPath = "glean/glass/test/regression/tests/dotnet-scip"
    testIndexer = Dotnet.indexer
    
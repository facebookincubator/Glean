{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.SCIP (main) where

import Glean.Indexer.SCIP as SCIP

import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-scip"
    testPath = "glean/glass/test/regression/tests/scip"
    testIndexer = SCIP.indexer

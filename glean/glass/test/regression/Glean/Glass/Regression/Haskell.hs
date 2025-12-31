{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Haskell (main) where

import System.Environment
import Glean.Indexer.Haskell as Haskell ( indexer )
import Glean.Glass.Regression.Snapshot ( mainGlassSnapshot )

main :: IO ()
main = do
  args <- getArgs
  withArgs ("--arg=--store-src" : args) $
    -- tells the indexer to produce src.FileContent facts, which support
    -- include_content=true in documentSymbolIndex calls.
    mainGlassSnapshot testName testPath testIndexer (const [])
  where
    testName = "glass-regression-haskell"
    testPath = "glean/glass/test/regression/tests/haskell"
    testIndexer = Haskell.indexer

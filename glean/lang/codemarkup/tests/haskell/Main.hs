{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Main ( main ) where

import System.Environment

import qualified Glean.Indexer.Haskell as Haskell ( indexer )
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = do
  args <- getArgs
  withArgs (["--root", path, "--with-ghc", "ghc"] <> args) $
    testMain (driverFromIndexer Haskell.indexer)
  where
    path = "glean/lang/codemarkup/tests/haskell/code"

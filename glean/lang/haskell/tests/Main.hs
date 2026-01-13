{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Main ( main ) where

#if __GLASGOW_HASKELL__ != 902
import qualified Data.Text as Text
#endif
import System.Environment

import qualified Glean.Indexer.Haskell as Haskell ( indexer )
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = do
  args <- getArgs
  withArgs (["--root", path, "--with-ghc", "ghc"] <> args) $
    testMain driver
  where
    path = "glean/lang/haskell/tests/code"
    driver = (driverFromIndexer Haskell.indexer) {
      driverOutSuffix = outSuffix
    }

outSuffix :: Maybe String
#if __GLASGOW_HASKELL__ == 902
outSuffix = Nothing
#else
outSuffix =
  Just $
  Text.unpack $
  Text.intercalate "." $
  take 2 $
  Text.splitOn "." __GLASGOW_HASKELL_FULL_VERSION__
#endif

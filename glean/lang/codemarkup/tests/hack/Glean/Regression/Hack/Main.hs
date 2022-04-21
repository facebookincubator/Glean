{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Hack.Main ( main ) where

import System.Environment ( withArgs )

import Glean.Indexer.Hack as Hack
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = withArgs ["--root", path] $ testMain driver
  where
      driver = driverFromIndexer Hack.indexer
      path = "glean/lang/codemarkup/tests/hack/cases"

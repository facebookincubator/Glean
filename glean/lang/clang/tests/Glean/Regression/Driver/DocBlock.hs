{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.DocBlock (main) where

import qualified Glean.Clang.Test as Clang
import qualified Glean.Clang.Test.DocBlock as DocBlock
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot
import Glean.Indexer


main :: IO ()
main = testMain (Clang.driver { driverIndexer = indexer })
  where
  indexer = driverIndexer Clang.driver `indexerThen` \_ -> DocBlock.indexer

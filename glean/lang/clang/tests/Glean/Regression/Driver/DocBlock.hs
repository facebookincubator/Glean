{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.DocBlock (main) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import qualified Glean.DocBlock.Test as DocBlock (runIndexer)
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot
import Glean.Derive (derivePredicate)
import Glean.Write (parseRef)

indexer :: Indexer Clang.Options
indexer = driverIndexer Clang.driver `indexerThen` docblocks
  where
  docblocks backend repo params = do
    DocBlock.runIndexer backend repo params
    forM_ passes $ \predicate ->
      derivePredicate backend repo
        Nothing Nothing predicate Nothing
    where
    passes = map parseRef
      [ "docmarkup.EntityByDocAttrKey"
      ]

main :: IO ()
main = testMain (Clang.driver { driverIndexer = indexer })

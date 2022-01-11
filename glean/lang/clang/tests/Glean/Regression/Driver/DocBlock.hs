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
import Glean.Regression.Options
import Glean.Regression.Test
import Glean.Derive (derivePredicate)
import Glean.Write (parseRef)

indexer :: Indexer Clang.Options
indexer = Clang.indexer `indexerThen` docblocks
  where
  docblocks test backend = do
    DocBlock.runIndexer test backend
    forM_ passes $ \predicate ->
      derivePredicate backend (testRepo test)
        Nothing Nothing predicate Nothing
    where
    passes = map parseRef
      [ "docmarkup.EntityByDocAttrKey"
      ]

main :: IO ()
main = testMain (Clang.driver { driverIndexer = indexer })

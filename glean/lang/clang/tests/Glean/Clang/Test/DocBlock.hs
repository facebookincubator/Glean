{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Clang.Test.DocBlock (indexer) where

import Control.Monad
import Data.Default

import Glean
import qualified Glean.DocBlock.Test as DocBlock (runIndexer)
import Glean.Indexer
import Glean.Derive (derivePredicate)
import Glean.Write (parseRef)

indexer :: RunIndexer
indexer backend repo params = do
  completePredicates backend repo (CompletePredicates_axiom def)
  DocBlock.runIndexer backend repo params
  forM_ passes $ \predicate ->
    derivePredicate backend repo
      Nothing Nothing predicate Nothing
  where
  passes = map parseRef
    [ "docmarkup.EntityByDocAttrKey"
    ]

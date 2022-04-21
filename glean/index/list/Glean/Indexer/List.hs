{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.List (
    SomeIndexer(..),
    indexers,
  ) where

import Glean.Indexer
import qualified Glean.Indexer.External as External
import qualified Glean.Indexer.Flow as Flow
import qualified Glean.Indexer.Hack as Hack

data SomeIndexer = forall opts . SomeIndexer (Indexer opts)

indexers :: [(String, String, SomeIndexer)]
indexers =
  [ ("external",
      "Use a generic external indexer",
      SomeIndexer External.externalIndexer)
  , ("flow",
      "Index JS/Flow code",
      SomeIndexer Flow.indexer)
  , ("hack",
      "Index Hack code",
      SomeIndexer Hack.indexer)
  ]

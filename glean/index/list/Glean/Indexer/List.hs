{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.List (
    SomeIndexer(..),
    indexers,
    cmdLineParser,
  ) where

import Data.Foldable
import Options.Applicative

import Util.OptParse

import Glean.Indexer
import qualified Glean.Indexer.External as External
import qualified Glean.Indexer.Flow as Flow
import qualified Glean.Indexer.Hack as Hack

data SomeIndexer = forall opts . SomeIndexer (Indexer opts)

indexers :: [SomeIndexer]
indexers =
  [ SomeIndexer External.externalIndexer
  , SomeIndexer Flow.indexer
  , SomeIndexer Hack.indexer
  ]

cmdLineParser :: Parser RunIndexer
cmdLineParser = asum langs
  where
  langs = flip map indexers $ \(SomeIndexer indexer) ->
    let desc = indexerDescription indexer in
    commandParser (indexerShortName indexer) (progDesc desc) $ do
      opts <- indexerOptParser indexer
      return (indexerRun indexer opts)

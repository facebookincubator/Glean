{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Go ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.SCIP ( derive )
import Glean.SCIP.Driver as SCIP

newtype Go = Go
  { scipGoBinary :: FilePath
  }

options :: Parser Go
options = do
  scipGoBinary <- strOption $
    long "scip-go" <>
    value "scip-go" <>
    help "path to the scip-go binary"
  return Go{..}

indexer :: Indexer Go
indexer = Indexer {
  indexerShortName = "go",
  indexerDescription = "Index Go code",
  indexerOptParser = options,
  indexerRun = \Go{..} backend repo IndexerParams{..} -> do
    val <- SCIP.runIndexer ScipIndexerParams {
        scipBinary = scipGoBinary,
        scipArgs = \outFile ->
           ["--module-version=glean", "--no-animation", "-o", outFile ],
        scipRoot = indexerRoot,
        scipWritesLocal = False
      }
    sendJsonBatches backend repo (scipGoBinary <> "/scip") val
    derive backend repo
  }

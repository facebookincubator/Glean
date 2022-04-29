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
import Glean.LSIF.Driver as LSIF

newtype Go = Go
  { lsifGoBinary :: FilePath
  }

options :: Parser Go
options = do
  lsifGoBinary <- strOption $
    long "lsif-go" <>
    value "lsif-go" <>
    help "path to the lsif-go binary"
  return Go{..}

indexer :: Indexer Go
indexer = Indexer {
  indexerShortName = "go",
  indexerDescription = "Index Go code",
  indexerOptParser = options,
  indexerRun = \Go{..} backend repo IndexerParams{..} -> do
    let
      params = LsifIndexerParams {
        lsifBinary = lsifGoBinary,
        lsifArgs = \outFile -> [ "--no-animation", "-o", outFile ],
        lsifRoot = indexerRoot,
        lsifStdout = False
      }
    val <- LSIF.runIndexer params
    sendJsonBatches backend repo (lsifGoBinary <> "/lsif") val
  }

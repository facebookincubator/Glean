{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.RustScip ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.SCIP ( derive )
import Glean.SCIP.Driver as SCIP

newtype RustScip = RustScip
  { rustAnalyzerBinary :: FilePath
  }

options :: Parser RustScip
options = do
  rustAnalyzerBinary <- strOption $
    long "rust-analyzer" <>
    value "rust-analyzer" <>
    help "path to the rust-analyzer binary"
  return RustScip{..}

indexer :: Indexer RustScip
indexer = Indexer {
  indexerShortName = "rust-scip",
  indexerDescription = "Index Rust code with `rust-analyzer scip`",
  indexerOptParser = options,
  indexerRun = \RustScip{..} backend repo IndexerParams{..} -> do
    val <- SCIP.runIndexer ScipIndexerParams {
                    scipBinary = rustAnalyzerBinary,
                    scipArgs = const [ "scip", "." ],
                    scipRoot = indexerRoot,
                    scipWritesLocal = True
                 }
    sendJsonBatches backend repo (rustAnalyzerBinary <> "/scip") val
    derive backend repo
  }

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.RustLsif ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.LSIF ( derive )
import Glean.LSIF.Driver as LSIF

newtype RustLsif = RustLsif
  { rustAnalyzerBinary :: FilePath
  }

options :: Parser RustLsif
options = do
  rustAnalyzerBinary <- strOption $
    long "rust-analyzer" <>
    value "rust-analyzer" <>
    help "path to the rust-analyzer binary"
  return RustLsif{..}

indexer :: Indexer RustLsif
indexer = Indexer {
  indexerShortName = "rust-lsif",
  indexerDescription = "Index Rust code via LSIF",
  indexerOptParser = options,
  indexerRun = \RustLsif{..} backend repo IndexerParams{..} -> do
    let
      params = LsifIndexerParams {
        lsifBinary = rustAnalyzerBinary,
        lsifArgs = \_outFile -> [ "lsif", "." ],
        lsifRoot = indexerRoot,
        lsifStdout = True
      }
    val <- LSIF.runIndexer params
    sendJsonBatches backend repo (rustAnalyzerBinary <> "/lsif") val
    derive backend repo
  }

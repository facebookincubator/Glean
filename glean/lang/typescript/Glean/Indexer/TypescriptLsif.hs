{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.TypescriptLsif ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.LSIF ( derive )
import Glean.Indexer.External

import Glean.LSIF.Driver as LSIF

newtype Typescript = Typescript
  { lsifTypescriptBinary :: FilePath
  }

options :: Parser Typescript
options = do
  lsifTypescriptBinary <- strOption $
    long "lsif-tsc" <>
    value "lsif-tsc" <>
    help "path to the lsif-tsc binary"
  return Typescript{..}

indexer :: Indexer Typescript
indexer = Indexer {
  indexerShortName = "typescript-lsif",
  indexerDescription = "Index Typescript code",
  indexerOptParser = options,
  indexerRun = \Typescript{..} backend repo IndexerParams{..} -> do
    let
      params = LsifIndexerParams {
        lsifBinary = lsifTypescriptBinary,
        lsifArgs = \outFile -> [ "-p", ".", "--out", outFile ],
        lsifRoot = indexerRoot,
        lsifStdout = False
      }
    val <- LSIF.runIndexer params
    sendJsonBatches backend repo (lsifTypescriptBinary <> "/lsif") val
    derive backend repo
  }

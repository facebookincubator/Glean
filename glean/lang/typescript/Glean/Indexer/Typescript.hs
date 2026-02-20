{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Typescript ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.SCIP ( derive )
import Glean.Indexer.External

import Glean.SCIP.Driver as SCIP

data Typescript = Typescript
  { scipTypescriptBinary :: FilePath
  , scipToGlean :: Maybe FilePath
  }

options :: Parser Typescript
options = do
  scipTypescriptBinary <- strOption $
    long "scip-typescript" <>
    value "scip-typescript" <>
    help "path to the scip-typescipt binary"
  scipToGlean <- optional (strOption $
    long "scip-to-glean" <>
    help "Path to the scip-to-glean binary. By default, looks for \
         \scip-to-glean on PATH and falls back to the Haskell converter \
         \if not found")
  scipToGleanCompat <- optional (strOption $
    long "rust-indexer" <> hidden)
  return Typescript
    { scipTypescriptBinary
    , scipToGlean = scipToGlean <|> scipToGleanCompat
    }

indexer :: Indexer Typescript
indexer = Indexer {
  indexerShortName = "typescript",
  indexerDescription = "Index Typescript code",
  indexerOptParser = options,
  indexerRun = \Typescript{..} backend repo IndexerParams{..} -> do
    let
      params = ScipIndexerParams {
        scipBinary = scipTypescriptBinary,
        scipArgs = \outFile ->
           [ "index", "--no-progress-bar"
           , "--cwd", indexerRoot, "--output", outFile ],
        scipOutDir = Nothing,
        scipWritesLocal = False,
        scipRoot = indexerRoot,
        scipToGlean = scipToGlean
      }
    val <- SCIP.runIndexer params
    sendJsonBatches backend repo (scipTypescriptBinary <> "/scip") val
    derive backend repo
  }

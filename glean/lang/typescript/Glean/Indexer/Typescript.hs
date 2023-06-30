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

newtype Typescript = Typescript
  { scipTypescriptBinary :: FilePath
  }

options :: Parser Typescript
options = do
  scipTypescriptBinary <- strOption $
    long "scip-typescript" <>
    value "scip-typescript" <>
    help "path to the scip-typescipt binary"
  return Typescript{..}

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
        scipWritesLocal = False,
        scipRoot = indexerRoot,
        scipLanguage = Just SCIP.TypeScript
      }
    val <- SCIP.runIndexer params
    sendJsonBatches backend repo (scipTypescriptBinary <> "/scip") val
    derive backend repo
  }

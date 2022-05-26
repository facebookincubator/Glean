{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Indexer.TypescriptScip ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

import Glean.Indexer.LSIF ( derive )
import Glean.SCIP.Driver as SCIP

newtype TypescriptSCIP = TypescriptSCIP
  { scipTypescriptBinary :: FilePath
  }

options :: Parser TypescriptSCIP
options = do
  scipTypescriptBinary <- strOption $
    long "scip-typescript" <>
    value "scip-typescript" <>
    help "path to the scip-typescript binary"
  return TypescriptSCIP{..}

indexer :: Indexer TypescriptSCIP
indexer = Indexer {
  indexerShortName = "typescript-scip",
  indexerDescription = "Index Typescript code via SCIP",
  indexerOptParser = options,
  indexerRun = \TypescriptSCIP{..} backend repo IndexerParams{..} -> do
    let
      params = ScipIndexerParams {
        scipBinary = scipTypescriptBinary,
        scipArgs = \outFile -> [ "index", "--output", outFile ],
        scipRoot = indexerRoot
      }
    val <- SCIP.runIndexer params
    sendJsonBatches backend repo (scipTypescriptBinary <> "/scip") val
    derive backend repo
  }

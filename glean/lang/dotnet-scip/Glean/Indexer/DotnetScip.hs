{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.DotnetScip ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External (sendJsonBatches)
import Glean.Indexer.SCIP (derive)
import Glean.SCIP.Driver as SCIP

data DotnetScip = DotnetScip
    { dotnetScipBinary :: FilePath
    , scipToGlean :: Maybe FilePath
    }

options :: Parser DotnetScip
options = do
    dotnetScipBinary <- strOption $
        long "scip-dotnet" <>
        value "scip-dotnet" <>
        help "path to scip-dotnet binary"
    scipToGlean <- optional (strOption $
        long "scip-to-glean" <>
        help "Path to the scip-to-glean binary. By default, looks for \
             \scip-to-glean on PATH and falls back to the Haskell converter \
             \if not found")
    scipToGleanCompat <- optional (strOption $
        long "rust-indexer" <> hidden)
    return DotnetScip
      { dotnetScipBinary
      , scipToGlean = scipToGlean <|> scipToGleanCompat
      }

indexer :: Indexer DotnetScip
indexer = Indexer {
        indexerShortName = "dotnet-scip",
        indexerDescription = "Index C# code with `scip-dotnet`",
        indexerOptParser = options,
        indexerRun = \DotnetScip{..} backend repo IndexerParams{..} -> do
            val <- SCIP.runIndexer ScipIndexerParams {
                scipBinary = dotnetScipBinary,
                scipArgs = const [ "index"],
                scipOutDir = Nothing,
                scipRoot = indexerRoot,
                scipWritesLocal = True,
                scipToGlean = scipToGlean
            }
            sendJsonBatches backend repo (dotnetScipBinary <> "/scip") val
            derive backend repo
    }

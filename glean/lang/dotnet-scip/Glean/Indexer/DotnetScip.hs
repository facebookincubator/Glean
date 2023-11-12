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

newtype DotnetScip = DotnetScip { dotnetScipBinary :: FilePath }

options :: Parser DotnetScip
options = do
    dotnetScipBinary <- strOption $
        long "scip-dotnet" <>
        value "scip-dotnet" <>
        help "path to scip-dotnet binary"
    return DotnetScip{..}

indexer :: Indexer DotnetScip
indexer = Indexer {
    indexerShortName = "dotnet-scip",
    indexerDescription = "Index C# code with `scip-dotnet`",
    indexerOptParser = options,
    indexerRun = \DotnetScip{..} backend repo IndexerParams{..} -> do
        val <- SCIP.runIndexer ScipIndexerParams {
            scipBinary = dotnetScipBinary,
            scipArgs = const [ "index"],
            scipRoot = indexerRoot,
            scipWritesLocal = True,
            scipLanguage = Just SCIP.CSharp
        }
        sendJsonBatches backend repo (dotnetScipBinary <> "/scip") val
        derive backend repo
}

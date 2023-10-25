{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{ -# LANGUAGE ApplicativeDo #- }
module Glean.Indexer.PythonScip ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.SCIP
import Glean.Indexer.SCIP (derive)
import Glean.SCIP.Driver as SCIP

newtype PythonScip = PythonScip
    { pythonAnalyzerBinary :: FilePath       
    }

options :: Parser PythonScip
options = do
    pythonAnalyzerBinary <- strOption $
        long "python-analyzer" <>
        value "python-analyzer" <>
        help "path to python-analyzer binary"
    return PythonScip{..}

indexer :: Indexer PythonScip
indexer = Indexer {
    indexerShortName = "python-scip",
    indexerDescription = "Index Python code with `python-analyzer scip`",
    indexerOptParser = options,
    indexerRun = \PythonScip{..} backend repo IndexerParams{..} -> do
        val <- SCIP.runIndexer ScipIndexerParams {
            scipBinary = pythonAnalyzerBinary,
            scipArgs = const [ "scip", "." ],
            scipRoot = indexerRoot,
            scipWritesLocal = True,
            scipLanguage = Just SCIP.Python
        }
        sendJsonBatches backend repo (pythonAnalyzerBinary <> "/scip") val
        derive backend repo
}
{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.PythonScip ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External (sendJsonBatches)
import Glean.Indexer.SCIP (derive)
import Glean.SCIP.Driver as SCIP

newtype PythonScip = PythonScip { pythonScipBinary :: FilePath }

options :: Parser PythonScip
options = do
    pythonScipBinary <- strOption $
        long "scip-python" <>
        value "scip-python" <>
        help "path to scip-python binary"
    return PythonScip{..}

indexer :: Indexer PythonScip
indexer = Indexer {
    indexerShortName = "python-scip",
    indexerDescription = "Index Python code with `scip-python`",
    indexerOptParser = options,
    indexerRun = \PythonScip{..} backend repo IndexerParams{..} -> do
        val <- SCIP.runIndexer ScipIndexerParams {
            scipBinary = pythonScipBinary,
            scipArgs = const [ "index", "--project-version", "test", "."],
            scipRoot = indexerRoot,
            scipWritesLocal = True,
            scipLanguage = Just SCIP.Python
        }
        sendJsonBatches backend repo (pythonScipBinary <> "/scip") val
        derive backend repo
}

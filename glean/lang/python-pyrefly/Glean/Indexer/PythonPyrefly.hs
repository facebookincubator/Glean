{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.PythonPyrefly ( indexer, PythonPyrefly(..) ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

data PythonPyrefly = PythonPyrefly
  { indexerBinary :: FilePath
  }

options :: Parser PythonPyrefly
options = do
  indexerBinary <- strOption $
    long "indexer-bin" <>
    value "pyrefly" <>
    help "path to the pyrefly binary"
  return PythonPyrefly{..}

indexer :: Indexer PythonPyrefly
indexer = Indexer {
  indexerShortName = "python-pyrefly",
  indexerDescription = "Index Python code with pyrefly",
  indexerOptParser = options,
  indexerRun = \PythonPyrefly{..} backend repo params -> do
    let pyreflyRoot = indexerRoot params
        ext = Ext {
          extRunScript = indexerBinary,
          extFlavour = Json,
          extArgs =
            [ "check"
            , "--suppress-errors"
            , "--check-all"
            , "--report-glean=${JSON_BATCH_DIR}/output"
            , pyreflyRoot <> "/**/*.py"
            , "--search-path=" <> pyreflyRoot
            , "--output=/dev/null"
            ],
          extDerivePredicates = []
        }

    indexerRun externalIndexer ext backend repo params
  }

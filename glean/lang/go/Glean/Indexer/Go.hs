{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Go ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.SCIP ( derive )
import qualified Glean.SCIP.Driver as SCIP
import System.Environment (setEnv)
import Control.Monad (forM_)

data Go = Go
  { scipGoBinary :: FilePath
  , goPackagesDriverBinary :: Maybe FilePath
  , scipExtraArgs :: [String]
  }

options :: Parser Go
options = do
  scipGoBinary <- strOption $
    long "scip-go" <>
    value "scip-go" <>
    help "path to the scip-go binary"
  goPackagesDriverBinary <- optional (strOption $
    long "gopackagesdriver" <>
    value "gopackagesdriver" <>
    help "path to the gopackagesdriver binary")
  scipExtraArgs <- many $ strOption $
    long "extra-arg" <>
    help "extra arguments to pass to the indexer"
  return Go{..}

indexer :: Indexer Go
indexer = Indexer {
  indexerShortName = "go",
  indexerDescription = "Index Go code",
  indexerOptParser = options,
  indexerRun = \Go{..} backend repo IndexerParams{..} -> do
    forM_ goPackagesDriverBinary (setEnv "GOPACKAGESDRIVER")
    val <- SCIP.runIndexer SCIP.ScipIndexerParams {
        scipBinary = scipGoBinary,
        scipArgs = \outFile ->
           ["--module-version=glean", "--no-animation", "-o", outFile ]
           ++ scipExtraArgs,
        scipRoot = indexerRoot,
        scipWritesLocal = False,
        scipLanguage = Just SCIP.Go
      }
    sendJsonBatches backend repo (scipGoBinary <> "/scip") val
    derive backend repo
  }

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
  , scipToGlean :: Maybe FilePath
  }

options :: Parser Go
options = do
  scipGoBinary <- strOption $
    long "scip-go" <>
    metavar "PATH" <>
    value "scip-go" <>
    help "path to scip-go binary"
  goPackagesDriverBinary <- optional (strOption $
    long "gopackagesdriver" <>
    help "path to to the gopackagesdriver binary")
  scipExtraArgs <- many $ strOption $
    long "extra-arg" <>
    help "extra arguments to pass to the indexer"
  scipToGlean <- optional (strOption $
    long "scip-to-glean" <>
    help "Path to the scip-to-glean binary. By default, looks for \
         \scip-to-glean on PATH and falls back to the Haskell \
         \converter if not found")
  scipToGleanCompat <- optional (strOption $
    long "rust-indexer" <> hidden)
  return Go
    { scipGoBinary
    , goPackagesDriverBinary
    , scipExtraArgs
    , scipToGlean = scipToGlean <|> scipToGleanCompat
    }

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
        scipOutDir = Nothing,
        scipRoot = indexerRoot,
        scipWritesLocal = False,
        scipToGlean = scipToGlean
      }
    sendJsonBatches backend repo (scipGoBinary <> "/scip") val
    derive backend repo
  }

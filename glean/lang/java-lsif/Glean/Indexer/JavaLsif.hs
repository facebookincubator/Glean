{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.JavaLsif ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.LSIF ( derive )
import Glean.LSIF.Driver as LSIF

newtype JavaLsif = JavaLsif
  { lsifJavaBinary :: FilePath
  }

options :: Parser JavaLsif
options = do
  lsifJavaBinary <- strOption $
    long "lsif-java" <>
    value "lsif-java" <>
    help "path to the lsif-java binary"
  return JavaLsif{..}

indexer :: Indexer JavaLsif
indexer = Indexer {
  indexerShortName = "java-lsif",
  indexerDescription = "Index Java via LSIF",
  indexerOptParser = options,
  indexerRun = \JavaLsif{..} backend repo IndexerParams{..} -> do
    let
      params = LsifIndexerParams {
        lsifBinary = lsifJavaBinary,
        lsifArgs = \outFile -> [ "index", "--output", outFile ],
        lsifRoot = indexerRoot,
        lsifStdout = False
      }
    val <- LSIF.runIndexer params
    sendJsonBatches backend repo (lsifJavaBinary <> "/lsif") val
    derive backend repo
  }

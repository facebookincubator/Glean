{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Swift ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.Indexer.SCIP ( derive )
import qualified Glean.SCIP.Driver as SCIP
import System.FilePath ( (</>), takeDirectory)
import Glean.LocalOrRemote ( serializeInventory )
import qualified Data.ByteString as BS

data Swift = Swift
  { scipGen :: FilePath
  , target :: String
  }

options :: Parser Swift
options = do
  scipGen <- strOption $
    long "scip-gen" <>
    value "scip-gen" <>
    help "path to an executable generating index.scip from swift code"
  target <- strOption $
    long "target" <>
    value "target" <>
    help "target to build and index"
  return Swift{..}

indexer :: Indexer Swift
indexer = Indexer {
  indexerShortName = "swift",
  indexerDescription = "Index Swift code",
  indexerOptParser = options,
  indexerRun = \Swift{..} backend repo IndexerParams{..} -> do
    let tmpDir        = indexerOutput
        inventoryFile = tmpDir </> "inventory.data"
    serializeInventory backend repo >>= BS.writeFile inventoryFile
    val <- SCIP.runIndexer SCIP.ScipIndexerParams {
        scipBinary = scipGen,
        scipArgs = \outFile ->
           let outDir = takeDirectory outFile in
           [ "--output-dir", outDir
            , "--target", target
            , "--swift-only"
            , "--output-type", "scip"
            , "--inventory", inventoryFile
            , "--build-indexer" ],
        scipRoot = indexerRoot,
        scipWritesLocal = False,
        scipLanguage = Just SCIP.Swift
      }
    sendJsonBatches backend repo (scipGen <> "/scip") val
    derive backend repo
  }

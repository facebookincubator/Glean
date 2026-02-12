{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- Utility to convert .scip files to Glean JSON for indexing,
-- acting as an external indexer binary.
-- This is a thin wrapper around scip-to-glean.
--
{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.SCIP.Main ( main ) where

import Options.Applicative
import Control.Monad
import Glean.Init (withOptions)
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Glean.SCIP.Driver as SCIP
import qualified Glean.LSIF.Driver as Util

data SCIP = SCIP
  { scipFile :: FilePath
  , outputFile :: FilePath
  , scipToGleanBin :: Maybe FilePath
  }

options :: Parser SCIP
options = do
  scipFile <- option str $ long "input" <>
    metavar "PATH" <>
    help "Path to a specific SCIP file to convert"
  outputFile <- option str $ long "output"
    <> metavar "PATH"
    <> help "Output filepath to write encoded schema info"
  scipToGleanBin <- optional (strOption $
    long "scip-to-glean" <>
    help "Path to scip-to-glean binary")
  return SCIP{..}

main :: IO ()
main = withOptions (info (helper <*> options) fullDesc) $
  \SCIP{..} -> do
    scipExists <- doesFileExist scipFile
    when (not scipExists) $
      error ("Could not find SCIP file at: " <> scipFile)
    bin <- SCIP.resolveScipToGlean scipToGleanBin
    json <- withSystemTempDirectory "glean-scip" $
      \tmpDir -> SCIP.runRustIndexer bin scipFile tmpDir
    Util.writeJSON outputFile json

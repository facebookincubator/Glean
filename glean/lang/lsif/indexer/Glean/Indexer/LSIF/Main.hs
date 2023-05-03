{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- Utility to convert LSIF to Glean JSON for indexing, acting as an external
-- indexer binary
--
{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.LSIF.Main ( main ) where

import Options.Applicative
import Control.Monad
import Glean.Init (withOptions)
import System.Directory (doesFileExist)
import Glean.LSIF.Driver as LSIF

data LSIF = LSIF
  { lsifFile :: FilePath -- ^ input file
  , projectRoot :: FilePath -- ^ path prefix of repository root
  , outputFile :: FilePath -- ^ output file
  }

options :: Parser LSIF
options = do
  lsifFile <- option str $ long "input" <>
    metavar "PATH" <>
    help "Path to a specific LSIF file to read"
  projectRoot <- option str $ long "root" <>
    metavar "ROOT" <>
    help "Path to repository root"
  outputFile <- option str $ long "output"
    <> metavar "PATH"
    <> help "Output filepath to write encoded schema info"
  return LSIF{..}

main :: IO ()
main = withOptions (info (helper <*> options) fullDesc) $ \LSIF{..} -> do
  lsifExists <- doesFileExist lsifFile
  when (not lsifExists) $ error ("Could not find LSIF file at: " <> lsifFile)
  json <- LSIF.processLSIF projectRoot lsifFile
  LSIF.writeJSON outputFile json

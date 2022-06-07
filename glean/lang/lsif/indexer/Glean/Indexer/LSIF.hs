{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.LSIF (
    indexer,
    derive
  ) where

import Options.Applicative
import Data.Text ( Text )
import Control.Monad ( forM_ )

import Glean.Derive
import Glean.Indexer
import Glean.Indexer.External
import Glean.Write
import Glean.LSIF.Driver as LSIF

import Glean.Backend ( Backend )
import qualified Glean
import System.Directory (doesFileExist)
import Util.OptParse (maybeStrOption)

newtype LSIF = LSIF
  { lsifIndexFile :: Maybe FilePath
  }

options :: Parser LSIF
options = do
  lsifIndexFile <- maybeStrOption (
    long "input" <>
    help "Optional path to a specific lsif index file")
  return LSIF{..}

-- | An indexer that just slurps an existing LSIF file. Usage:
--
--   cli:    glean index lsif <file> --repo name/hash
--   shell:  :index lsif <file>
--
indexer :: Indexer LSIF
indexer = Indexer {
  indexerShortName = "lsif",
  indexerDescription = "Index an LSIF file",
  indexerOptParser = options,
  indexerRun = \LSIF{..} backend repo IndexerParams{..} -> do
    lsifFile <- case lsifIndexFile of
      Just f -> pure f
      Nothing -> do -- then indexerRoot should be an lsif file
        mFile <- doesFileExist indexerRoot
        if mFile
          then pure indexerRoot
          else error "Neither --input nor --root are lsif files"
    val <- LSIF.processLSIF indexerRoot lsifFile
    sendJsonBatches backend repo "lsif" val
    derive backend repo
  }

derive :: Backend b => b -> Glean.Repo -> IO ()
derive backend repo = forM_ lsifDerivedPredicates $ \pred_ ->
  derivePredicate backend repo Nothing Nothing
    (parseRef pred_) Nothing

-- Should correspond to stored predicates in lsif.angle
-- shared amongst all LSIF-based language indexers
lsifDerivedPredicates :: [Text]
lsifDerivedPredicates =
  [ "lsif.MonikerDefinition"
  , "lsif.NameLowerCase"
  , "lsif.NameDefinition"
  ]

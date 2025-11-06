{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.SCIP (
    indexer,
    derive,
  ) where

import Options.Applicative
import Data.Text ( Text )
import Control.Monad ( forM_ )
import Data.Maybe ( fromMaybe )
import System.FilePath ( (</>) )
import System.Process ( callProcess )
import qualified Data.Aeson as Aeson

import Glean.Derive
import Glean.Indexer
import Glean.Indexer.External
import Glean.SCIP.Driver as SCIP
import Glean.Write

import qualified Glean
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Util.OptParse (maybeStrOption)

-- | A generic SCIP indexer, for existing scip files
data SCIP = SCIP
  { scipIndexFile :: Maybe FilePath
  , scipToGlean :: Maybe FilePath
  }
  -- no options currently

options :: Parser SCIP
options =
  SCIP
    <$> maybeStrOption (
      long "input" <>
      help "Optional path to a specific scip index file")
    <*> maybeStrOption (
      long "scip-to-glean" <>
      help "Path to scip-to-glean indexer binary. If not provided, uses \
           \the haskell indexer instead")

-- | An indexer that just slurps an existing SCIP file. Usage:
--
--   cli:    glean index scip <file> --repo name/hash
--   shell:  :index scip <file>
--
indexer :: Indexer SCIP
indexer = Indexer {
  indexerShortName = "scip",
  indexerDescription = "Index a SCIP file",
  indexerOptParser = options,
  indexerRun = \SCIP{..} backend repo IndexerParams{..} -> do
    scipFile <- case scipIndexFile of
      Just f -> pure f
      Nothing -> do -- then indexerRoot should be an lsif file
        mFile <- doesFileExist indexerRoot
        if mFile
          then pure indexerRoot
          else error "Neither --input nor --root are scip files"
    val <- case scipToGlean of
      Nothing -> SCIP.processSCIP Nothing False Nothing Nothing scipFile
      Just rustIndexer -> withSystemTempDirectory "glean-scip" $ \tmpDir -> do
        let jsonPath = tmpDir </> "index.json"
        callProcess rustIndexer
          ["--input", scipFile, "--infer-language", "--output", jsonPath]
        mJson <- Aeson.decodeFileStrict jsonPath
        return $ fromMaybe (error "rust indexer did not produce JSON") mJson
    sendJsonBatches backend repo "scip" val
    derive backend repo
  }

-- | Derive any SCIP stored predicates
derive :: Glean.Backend b => b -> Glean.Repo -> IO ()
derive backend repo = forM_ scipDerivedPredicates $ \pred_ ->
  derivePredicate backend repo Nothing Nothing
    (parseRef pred_) Nothing

-- Should correspond to stored predicates in scip.angle
scipDerivedPredicates :: [Text]
scipDerivedPredicates =
  [ "scip.LowerCaseDisplayNameSymbol"
  , "scip.DefinitionLocation"
  , "scip.ReferenceLocation"
  , "scip.SymbolDisplayName"
  , "scip.IsImplemented"
  ]

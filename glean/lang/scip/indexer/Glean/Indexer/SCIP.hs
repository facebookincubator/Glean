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

import Glean.Derive
import Glean.Indexer
import Glean.Indexer.External
import Glean.SCIP.Driver as SCIP
import Glean.Write

import qualified Glean
import System.Directory (doesFileExist)
import Util.OptParse (maybeStrOption)

data SCIP = SCIP
  { scipFile :: FilePath -- ^ input file
  , outputFile :: FilePath -- ^ output file
  , scipLanguage :: Maybe LanguageId -- ^ a default language if known
  , inferLanguage :: Bool -- ^ default False, infer language using file suffix
  , scipPathPrefix :: Maybe FilePath -- ^ optional path to prefix file paths
  , stripPathPrefix :: Maybe FilePath -- ^ optional prefix to drop from paths
  }

options :: Parser SCIP
options = do
  scipFile <- option str $ long "input" <>
    metavar "PATH" <>
    help "Path to a specific SCIP file to convert"
  outputFile <- option str $ long "output"
    <> metavar "PATH"
    <> help "Output filepath to write encoded schema info"
  scipLanguage <- option (Just <$> readLanguage) $ long "language" <>
    metavar "LANGUAGE" <>
    value Nothing <>
    help "Default language of files in the index"
  inferLanguage <- switch $
    short 'i' <>
    long "infer-language" <>
    help ("Infer symbol language based on file suffix" <>
     "(when set this takes precedence over --language)")
  scipPathPrefix <- option (Just <$> str) $ long "root-prefix" <>
    metavar "PATH" <>
    value Nothing <>
    help "Path to prepend to file path data"
  stripPathPrefix <- option (Just <$> str) $ long "strip-prefix" <>
    metavar "PATH" <>
    value Nothing <>
    help "Path prefix to strip from path data"
  return SCIP{..}

options :: Parser SCIP
options =
  SCIP <$> maybeStrOption (
    long "input" <>
    help "Optional path to a specific scip index file")

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
    val <- SCIP.processSCIP Nothing False Nothing Nothing scipFile
    sendJsonBatches backend repo "scip" val
    derive backend repo
  }
  
-- If the indexer doesn't set the langauge Id of the files, we
-- can assert it here. Otherwise Glean won't know what language the
-- symbols are in
readLanguage ::  ReadM LanguageId
readLanguage = do
  ln <- Text.toLower <$> str
  case ln of
    "typescript" -> return TypeScript
    "rust" -> return Rust
    "go" -> return Go
    "java" -> return Java
    "kotlin" -> return Kotlin
    "csharp" -> return CSharp
    "swift" -> return Swift
    _ -> readerError "Unrecognized SCIP language"

-- | Derive any SCIP stored predicates
derive :: Glean.Backend b => b -> Glean.Repo -> IO ()
derive backend repo = forM_ scipDerivedPredicates $ \pred_ ->
  derivePredicate backend repo Nothing Nothing
    (parseRef pred_) Nothing

-- Should correspond to stored predicates in scip.angle
scipDerivedPredicates :: [Text]
scipDerivedPredicates =
  [ "scip.DefinitionLocation"
  , "scip.ReferenceLocation"
  ]

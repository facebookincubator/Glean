{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

A wrapper over the lsif driver to generate JSON predicates from lsif indexers

-}

{-# LANGUAGE ApplicativeDo #-}

module Main ( main ) where

import Options.Applicative
import System.Directory ( getCurrentDirectory, makeAbsolute )
import System.FilePath
    ( dropTrailingPathSeparator, (</>), takeBaseName )
import qualified Data.Aeson as Aeson

import Glean.Init ( withOptions )
import qualified Glean.LSIF.Driver as LSIF
import Data.Char (toLower)
import Data.List (intercalate)

data Config = Config
  { cfgLanguage :: Either String LSIF.Language
  , cfgRepoDir :: FilePath
  , cfgOutDir :: Maybe FilePath
  , cfgUseDumpFile :: Maybe FilePath
  }

parseLang :: String -> Either String LSIF.Language
parseLang "typescript" = Right LSIF.TypeScript
parseLang "go" = Right LSIF.Go
parseLang s = Left s

options :: ParserInfo Config
options = info (parser <**> helper) fullDesc
  where
    parser = do
      cfgLanguage <- parseLang <$> strOption (
             long "language"
          <> metavar "LANGUAGE"
          <> help "Which language to index (typescript, go, ..)")
      cfgUseDumpFile <- optional (strOption $
             long "dump-file"
          <> metavar "PATH"
          <> help "Optionally use an existing lsif dump file (do not re-index lsif)")
      cfgOutDir <- optional $ strOption $
             long "output-dir"
          <> short 'o'
          <> metavar "PATH"
          <> help "Optional path to JSON output directory (default: `pwd`)"
      cfgRepoDir <- strArgument $
             metavar "PATH"
          <> help "Source repository directory (required)"
      return Config{..}

-- | Run an lsif indexer and convert to json predicates for lsif.angle
main :: IO ()
main = withOptions options $ \Config{..} -> do
  let lang = case cfgLanguage of
        Left err -> error $ "Unsupported language: " <> show err <>
          " should be one of " <> map toLower (intercalate ", " $
            map show [minBound :: LSIF.Language ..])
        Right l -> l

  cwd <- getCurrentDirectory
  repoDir <- makeAbsolute cfgRepoDir
  let name = takeBaseName (dropTrailingPathSeparator repoDir)
  json <- runLsif lang $ case cfgUseDumpFile of
            Nothing -> Right repoDir
            Just f -> Left f
  let outFile = case cfgOutDir of
        Nothing -> cwd </> name <> ".json"
        Just f -> f </> name <> ".json"
  LSIF.writeJSON outFile json

-- | either parse an existing lsif file, or run an indexer and process that
runLsif :: LSIF.Language -> Either FilePath FilePath -> IO Aeson.Value
runLsif lang repo = case repo of
  Left lsifFile -> LSIF.processLSIF lsifFile
  Right repoDir -> LSIF.runIndexer lang repoDir

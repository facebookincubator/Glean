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

import Data.Char (toLower)
import Data.List (intercalate)
import Options.Applicative
import System.Directory ( makeAbsolute, getCurrentDirectory )
import System.FilePath ( (</>), takeBaseName, takeDirectory )
import Data.Maybe ( fromMaybe )
import qualified Data.Aeson as Aeson

import Glean.Init ( withOptions )
import qualified Glean.LSIF.Driver as LSIF

data Config = Config
  { cfgLanguage :: Maybe (Either String LSIF.Language)
  , cfgRepoDir :: Maybe FilePath
  , cfgOutDir :: Maybe FilePath
  , cfgUseDumpFile :: Maybe FilePath
  }

parseLang :: String -> Either String LSIF.Language
parseLang "typescript" = Right LSIF.TypeScript
parseLang "go" = Right LSIF.Go
parseLang "rust" = Right LSIF.Rust
parseLang s = Left s

options :: ParserInfo Config
options = info (parser <**> helper) fullDesc
  where
    parser = do
      cfgUseDumpFile <- optional (strOption $
             long "dump-file"
          <> metavar "PATH"
          <> help ("Use an existing lsif dump file " <>
               "(instead of running lsif indexer)"))
      cfgOutDir <- optional $ strOption $
             long "output-dir"
          <> short 'o'
          <> metavar "PATH"
          <> help ("Optional path to JSON output directory " <>
                    "(default: a single output file in `pwd`)" )
      cfgLanguage <- fmap parseLang <$> optional (strOption (
             long "language"
          <> metavar "LANGUAGE"
          <> help "Which language to index (typescript, go, ..)"))
      cfgRepoDir <- optional $ strArgument $
             metavar "PATH"
          <> help ("Source repository directory " <>
               "(required if not using dump-file)")
      return Config{..}

-- | Run an lsif indexer and convert to json predicates for lsif.angle
main :: IO ()
main = withOptions options $ \Config{..} -> do
  cwd <- getCurrentDirectory
  (json,baseName) <- case cfgUseDumpFile of
    Just lsif -> do
        let name = takeBaseName (takeDirectory lsif)
        val <- runLsif (Left lsif)
        return (val, name)
    Nothing -> case (cfgLanguage, cfgRepoDir) of -- must provide language + repo dir
      (Nothing, _) ->
        error $ "Missing --language: " <>
          " should be one of " <> map toLower (intercalate ", " $
            map show [minBound :: LSIF.Language ..])
      (Just (Left err), _) ->
        error $ "Unsupported language: " <> show err <>
          " should be one of " <> map toLower (intercalate ", " $
            map show [minBound :: LSIF.Language ..])
      (Just (Right lang), Nothing) ->
        error $ "Missing PATH to " <> show lang <> " source repository"
      (Just (Right lang), Just rawRepoDir) -> do
        repoDir <- makeAbsolute rawRepoDir
        let name = takeBaseName (takeDirectory repoDir)
        val <- runLsif (Right (lang,repoDir))
        return (val, name)

  LSIF.writeJSON (fromMaybe cwd cfgOutDir </> baseName <> ".json") json

-- | either parse an existing lsif file, or run an indexer and process that
runLsif :: Either FilePath (LSIF.Language, FilePath) -> IO Aeson.Value
runLsif (Left lsifFile) = LSIF.processLSIF lsifFile
runLsif (Right (lang,repoDir)) = LSIF.runIndexer lang repoDir

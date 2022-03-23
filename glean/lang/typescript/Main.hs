{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

A wrapper over lsif-tsc , the typescript LSIF indexer, to generate Glean
predicates for typescript.

-}

{-# LANGUAGE ApplicativeDo #-}

module Main ( main ) where

import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.Aeson as Aeson

import Glean.Init ( withOptions )
import qualified Glean.LSIF.Driver as LSIF

data Config = Config
  { cfgRepoDir :: FilePath
  , cfgOutFile :: Maybe FilePath
  , cfgUseDumpFile :: Maybe FilePath
  }

options :: ParserInfo Config
options = info (parser <**> helper) fullDesc
  where
    parser = do
      cfgUseDumpFile <- optional (strOption $
             long "dump-file"
          <> metavar "PATH"
          <> help "Optionally use an existing lsif-tsc dump file (do not re-index lsif)")
      cfgOutFile <- optional $ strOption $
             long "output"
          <> short 'o'
          <> metavar "PATH"
          <> help "Optional path to JSON output file (default: `pwd`/lsif.json)"
      cfgRepoDir <- strArgument $
             metavar "PATH"
          <> help "TypeScript repository directory (required)"
      return Config{..}

-- | Run an lsif indexer and convert to json predicates for lsif.angle
main :: IO ()
main = withOptions options $ \Config{..} -> do
  cwd <- getCurrentDirectory
  repoDir <- makeAbsolute cfgRepoDir
  let name = takeBaseName (dropTrailingPathSeparator repoDir)
  json <- runLsif $ case cfgUseDumpFile of
            Nothing -> Right repoDir
            Just f -> Left f
  let outFile = case cfgOutFile of
        Nothing -> cwd </> name <> ".json"
        Just f -> f
  LSIF.writeJSON outFile json

-- | either parse an existing lsif file, or run an indexer and process that
runLsif :: Either FilePath FilePath -> IO Aeson.Value
runLsif repo = case repo of
  Left lsifFile -> LSIF.processLSIF lsifFile
  Right repoDir -> LSIF.runIndexer LSIF.TypeScript repoDir

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module HieIndexer.Options (
    Sources(..),
    HieIndexerOptions(..),
    Mode(..),
    options,
  ) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import Data.Maybe (fromJust)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  short,
  showDefault,
  strOption,
  value, (<|>), some, strArgument
 )
import Glean (Repo (Repo), SchemaId (SchemaId, unSchemaId))
import Glean.Schema.Builtin.Types (schema_id)

-- | A set of paths to recursively search for .hie files
data Sources
  = HieFiles (NonEmpty FilePath)   -- ^ Paths to search recursively for .hie files

data HieIndexerOptions sources = HieIndexerOptions
  { sources :: sources
  , verbosity :: Int
  }

data Mode
  = WriteMode {
      repo :: Repo
    }
  | BinaryMode {
      outputPath :: FilePath,
      schemaId :: SchemaId
    }


sourcesP :: Parser Sources
sourcesP = HieFiles <$> hieFilesP
  where
    hieFilesP = fromJust . nonEmpty <$> some
      (strArgument (metavar "PATH" <> help "Tree containing .hie files"))

options :: ParserInfo (HieIndexerOptions Sources, Mode)
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser (HieIndexerOptions Sources, Mode)
    parser = do

      sources <- sourcesP

      verbosity <-
        option
          auto
          ( long "verbosity"
              <> short 'v'
              <> help "Verbosity level."
              <> showDefault
              <> value 1
              <> metavar "INT"
          )

      mode <- modeParser
      return (HieIndexerOptions {..}, mode)

modeParser :: Parser Mode
modeParser = serviceModeParser <|> binaryModeParser
  where
    serviceModeParser = do
      repoName <-
        strOption
          ( long "repo-name"
              <> metavar "REPO_NAME"
              <> help "Name of the Glean DB to be created."
          )

      repoHash <-
        strOption
          ( long "repo-hash"
              <> metavar "REPO_HASH"
              <> help "Hash of the DB to be created."
          )
      return $ WriteMode (Repo repoName repoHash)

    binaryModeParser = do
        outputPath <- strOption
          ( long "output"
            <> metavar "FILE"
            <> help "Output binary serialized facts instead of writing to DB")
        schemaId <- SchemaId <$> strOption
          ( long "schema-id"
            <> help "Glean schema id to use for encoding the facts (defaults to latest one)"
            <> value (unSchemaId schema_id))
        return $ BinaryMode{..}

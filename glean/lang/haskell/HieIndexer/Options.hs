{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module HieIndexer.Options (
    HieIndexerOptions(..),
    Mode(..),
    options,
  ) where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Data.Maybe
import Data.Text (Text)
import Options.Applicative
import Glean (Repo (Repo), SchemaId (SchemaId, unSchemaId))
import Glean.Schema.Builtin.Types (schema_id)
import Util.OptParse

data HieIndexerOptions = HieIndexerOptions
  { hiePaths :: NonEmpty FilePath
      -- ^ Paths to search recursively for .hie files
  , srcPaths :: NonEmpty Text
      -- ^ Paths to look for source files. May include the string
      -- @$PACKAGE@ which will be replaced by the package name
      -- (@<pkg>-<version>@).
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


options :: ParserInfo (HieIndexerOptions, Mode)
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser (HieIndexerOptions, Mode)
    parser = do

      hiePaths <- fromJust . nonEmpty <$> some
        (strArgument (metavar "PATH" <> help "Tree containing .hie files"))

      srcPaths <- fromMaybe (NonEmpty.singleton ".") . nonEmpty <$>
        many (textOption (
          long "src" <>
          metavar "PATH" <>
          help (
            "Path to search for source files. The string \"$PACKAGE\" is " <>
            "replaced by the package name, e.g. text-2.0.2")))

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

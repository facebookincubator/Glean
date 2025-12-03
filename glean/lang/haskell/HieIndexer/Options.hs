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
    UnitName(..),
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
  , srcPrefix :: Maybe Text
      -- ^ Prefix to add to source paths
  , unitName :: UnitName
      -- ^ How to handle unit names
  , storeSrc :: Bool
      -- ^ Whether to store the indexed source in src.FileContent
  , verbosity :: Int
  }

data UnitName = UnitKey | UnitId

parseUnit :: String -> Maybe UnitName
parseUnit "key" = Just UnitKey
parseUnit "id" = Just UnitId
parseUnit _ = Nothing

showUnit :: UnitName -> String
showUnit UnitKey = "key"
showUnit UnitId = "id"

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

      srcPrefix <- optional (textOption (
        long "prefix" <>
        metavar "PATH" <>
        help "Prefix to add to source paths in the DB"))

      storeSrc <- switch (
        long "store-src" <>
        help "Store the full source file in src.FileContent")

      unitName <- option (maybeReader parseUnit) (
        long "unit" <>
        metavar "key|id" <>
        value UnitKey <>
        showDefaultWith showUnit <>
        help "Whether to store the full unit key (key) or just name-version (id)")

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

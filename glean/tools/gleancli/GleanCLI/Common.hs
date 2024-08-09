{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Common
  ( PageOptions(..)
  , pageOpts
  , maxConcurrencyOpt
  , dbOpts
  , dbSlash
  , dbNameOpt
  , dbInstanceOpt
  , handleOpt
  , FileFormat(..)
  , fileFormatOpt
  ) where

import Data.Text (Text)
import Options.Applicative

import Util.OptParse

import Glean
import Glean.BuildInfo

maxConcurrencyOpt :: Parser Int
maxConcurrencyOpt = option auto
  (  long "maxConcurrency"
  <> short 'j'
  <> metavar "NUMBER"
  <> showDefault
  <> value 20
  )

data PageOptions = PageOptions
  { pageBytes :: Int
  , pageFacts :: Maybe Int
  }

pageOpts :: Parser PageOptions
pageOpts = do
  pageBytes <- option auto
    ( long "page-bytes"
    <> value 1000000
    <> metavar "BYTES"
    <> help "maximum number of bytes per page"
    )
  pageFacts <- optional $ option auto
    ( long "page-facts"
    <> metavar "FACTS"
    <> help "maximum number of facts per page"
    )
  return PageOptions{..}

dbOpts :: Parser Repo
dbOpts = dbSlash <|> dbNameInstance

dbSlash :: Parser Repo
dbSlash = do
  option (maybeReader Glean.parseRepo)
    (  long "db"
    <> metavar "NAME/INSTANCE"
    <> help "identifies the database"
    )
  <|> option (maybeReader Glean.parseRepo)
    (internal <> long "repo")

dbNameInstance :: Parser Repo
dbNameInstance = Repo <$> dbNameOpt <*> dbInstanceOpt

dbNameOpt :: Parser Text
dbNameOpt = textOption
    (  long "db-name"
    <> metavar "NAME"
    <> help "name of the database"
    )
    <|> textOption (internal <> long "repo-name")

dbInstanceOpt :: Parser Text
dbInstanceOpt = textOption
    (  long "db-instance"
    <> metavar "INSTANCE"
    <> help "instance of the database"
    ) <|>
    textOption (internal <> long "repo-hash") -- backwards compat

handleOpt :: Parser Text
handleOpt = textOption
  (  long "handle"
  <> metavar "HANDLE"
  <> value (buildRule <> "@" <> buildRevision)
  )

data FileFormat
  = JsonFormat
  | BinaryFormat
  deriving (Eq)

instance Show FileFormat where
  show ff = case ff of
    JsonFormat -> "json"
    BinaryFormat -> "binary"

fileFormatOpt :: FileFormat -> Parser FileFormat
fileFormatOpt defaultFormat = option (eitherReader parseFileFormat)
  (  long "file-format"
  <> value defaultFormat
  <> showDefault
  <> metavar "(json|binary)"
  <> help "Format of the files with facts (see FILE for more details)"
  )
  where
    parseFileFormat :: String -> Either String FileFormat
    parseFileFormat "json" = Right JsonFormat
    parseFileFormat "binary" = Right BinaryFormat
    parseFileFormat s = Left $ "unknown format: " <> s
      <> ", supported values: (json|binary)"

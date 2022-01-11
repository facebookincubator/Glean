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
  , repoOpts
  , repoSlash
  , repoNameOpt
  , repoHashOpt
  , handleOpt
  , fileArgs
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

repoOpts :: Parser Repo
repoOpts = repoSlash <|> repoNameHash

repoSlash :: Parser Repo
repoSlash = do
  option (maybeReader Glean.parseRepo)
    (  long "repo"
    <> metavar "NAME/HASH"
    <> help "identifies the repository"
    )

repoNameHash :: Parser Repo
repoNameHash = Repo <$> repoNameOpt <*> repoHashOpt

repoNameOpt :: Parser Text
repoNameOpt = textOption
    (  long "repo-name"
    <> metavar "NAME"
    <> help "name of the repository"
    )

repoHashOpt :: Parser Text
repoHashOpt = textOption
    (  long "repo-hash"
    <> metavar "HASH"
    <> help "hash of the repository"
    )

handleOpt :: Parser Text
handleOpt = textOption
  (  long "handle"
  <> metavar "HANDLE"
  <> value (buildRule <> "@" <> buildRevision)
  )

fileArgs :: Parser [FilePath]
fileArgs = many $ strArgument
  (  metavar "FILE"
  <> help "File of facts (JSON)"
  )

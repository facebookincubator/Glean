{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module HieDBIndexer.Options where

-- @lint-ignore-every TODOHACK
-- @lint-ignore-every LINEWRAP

-- @lint-ignore-every TODOHACK
-- @lint-ignore-every LINEWRAP
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Glean (Backend)
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
  switch,
  value, (<|>), some, strArgument
 )

-- | Either a HieDB or a set of folders to recursively search for .hie files
data Sources
  = HieDB FilePath        -- ^ Path to a hiedb
  | HieFiles (NonEmpty FilePath)   -- ^ Paths to search recursively for .hie files

data HieDBIndexerOptions sources = HieDBIndexerOptions
  { sources :: sources
  , verbosity :: Int
  , hiedbTrace :: Bool
  , repoName :: String
  , repoHash :: Text
  , repoPath :: FilePath
  , chunkSize :: Int
  , dontCreateDb :: Bool
  }

data HieDBIndexerEnv b = HieDBIndexerEnv
  { backend :: Glean.Backend b => b
  , cfg :: HieDBIndexerOptions FilePath
  }

sourcesP :: Parser Sources
sourcesP = HieDB <$> hiedbP <|> (HieFiles <$> hieFilesP)
  where
    hiedbP = strOption
          ( long "db-path"
              <> metavar "HIEDB_PATH"
              <> help "Path to the HieDB that will be used to get XReferences"
          )
    hieFilesP = fromJust . nonEmpty <$> some
      (strArgument (metavar "PATH" <> help "Tree containing .hie files"))

options :: ParserInfo (HieDBIndexerOptions Sources)
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser (HieDBIndexerOptions Sources)
    parser = do

      sources <- sourcesP

      -- TODO(T96241762): change this argument name when using it with fbcode.
      -- Also add a new argument for the cache directory name, that will be used
      -- to clean up the file paths and read the source files.
      repoPath <-
        strOption
          ( long "repo-path"
              <> short 'p'
              <> metavar "DIR"
              <> help
                ( unwords
                    [ "Path to the repo containing the original"
                    , "source files."
                    ]
                )
          )

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

      hiedbTrace <-
        switch
          ( long "trace"
              <> help "Enable trace in hiedb connection."
          )

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
      chunkSize <-
        option
          auto
          ( long "chunk-size"
              <> short 'c'
              <> help "Number of vertices in each file batch."
              <> showDefault
              <> value 15000
              <> metavar "INT"
          )
      dontCreateDb <-
        switch
          ( long "dont-create-db"
              <> help
                ( unwords
                    [ "If a Glean DB shouldn't be created, i.e. an "
                    , "existing one should be used."
                    , " Needed for the regression tests."
                    ]
                )
          )
      return HieDBIndexerOptions {..}

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Index (IndexCommand) where

import Options.Applicative
import System.Directory
import System.IO.Temp

import Util.IO
import Util.OptParse

import Glean
import Glean.Indexer
import Glean.Indexer.List
import Glean.Util.Some

import GleanCLI.Common
import GleanCLI.Types

data IndexCommand
  = Index
      { indexCmdRepo :: Repo
      , indexCmdRoot :: FilePath
      , indexCmdRun :: RunIndexer
      }

instance Plugin IndexCommand where
  parseCommand =
    commandParser "index" (progDesc "Index some source code") $ do
      indexCmdRepo <- dbOpts
      indexCmdRun <- cmdLineParser
      indexCmdRoot <- strArgument (metavar "ROOT")
      return Index{..}

  runCommand _evb _cfg backend Index{..} = do
    projectRoot <- getCurrentDirectory
    withSystemTempDirectory "glean-index" $ \tmpdir -> do
    fillDatabase backend Nothing indexCmdRepo "" (die 1 "DB already exists") $
      indexCmdRun (Some backend) indexCmdRepo
        IndexerParams {
          indexerRoot = indexCmdRoot,
          indexerProjectRoot = projectRoot,
          indexerOutput = tmpdir,
          indexerGroup = ""
        }

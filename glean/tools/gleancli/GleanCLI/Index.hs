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

import Glean hiding (options)
import Glean.Regression.Indexer
import Glean.Regression.Indexer.External
import Glean.Util.Some

import GleanCLI.Common
import GleanCLI.Types

data IndexCommand
  = Index
      { indexCmdRepo :: Repo
      , indexCmdRoot :: FilePath
      , indexCmdOpts :: Ext
      }

instance Plugin IndexCommand where
  parseCommand =
    commandParser "index" (progDesc "Index some source code") $ do
      commandParser "external" (progDesc "Use an external indexer") $ do
        indexCmdRepo <- repoOpts
        indexCmdRoot <- strArgument (metavar "ROOT")
        indexCmdOpts <- indexerOptParser externalIndexer
        return Index{..}

  runCommand _evb _cfg backend Index{..} = do
    projectRoot <- getCurrentDirectory
    withSystemTempDirectory "glean-index" $ \tmpdir -> do
    fillDatabase backend Nothing indexCmdRepo "" (die 1 "repo already exists") $
      indexerRun externalIndexer indexCmdOpts (Some backend) indexCmdRepo
        IndexerParams {
          indexerRoot = indexCmdRoot,
          indexerProjectRoot = projectRoot,
          indexerOutput = tmpdir,
          indexerGroup = ""
        }

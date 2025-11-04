{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Index (IndexCommand) where

import Control.Monad
import Data.Either
import Options.Applicative
import System.Directory
import System.IO.Temp

import Util.Control.Exception
import Util.IO
import Util.OptParse

import Glean
import Glean.Indexer
import Glean.Indexer.List
import Glean.Util.Some

import GleanCLI.Common
import GleanCLI.Create
import GleanCLI.Types

data IndexCommand
  = Index
      { indexCmdRepo :: Repo
      , indexCmdRoot :: FilePath
      , indexCmdCreate :: CreateOpts
      , indexCmdRun :: RunIndexer
      }

instance Plugin IndexCommand where
  parseCommand =
    commandParser "index" (progDesc "Index some source code") $ do
      indexCmdRepo <- dbOpts
      indexCmdCreate <- parseCreateOpts
      indexCmdRun <- cmdLineParser
      indexCmdRoot <- strArgument (metavar "ROOT")
      return Index{..}

  runCommand _evb _cfg backend Index{..} = do
    projectRoot <- getCurrentDirectory
    withSystemTempDirectory "glean-index" $ \tmpdir -> do
    tryBracket
      (do
        exists <- createDb backend indexCmdRepo indexCmdCreate
        when exists $ die 1 "DB already exists"
      )
      (\_ ex -> when (isRight ex) $ finish backend indexCmdRepo) $
      \_ -> indexCmdRun (Some backend) indexCmdRepo
        IndexerParams {
          indexerRoot = indexCmdRoot,
          indexerProjectRoot = projectRoot,
          indexerOutput = tmpdir,
          indexerGroup = ""
        }

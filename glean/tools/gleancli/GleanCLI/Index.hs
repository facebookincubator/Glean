{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Index (IndexCommand) where

import Data.Foldable
import Options.Applicative
import System.Directory
import System.IO.Temp

import Util.IO
import Util.OptParse

import Glean hiding (options)
import Glean.Indexer
import qualified Glean.Indexer.External as External
import qualified Glean.Indexer.Flow as Flow
import qualified Glean.Indexer.Hack as Hack
import Glean.Util.Some

import GleanCLI.Common
import GleanCLI.Types

data IndexCommand
  = Index
      { indexCmdRepo :: Repo
      , indexCmdRoot :: FilePath
      , indexCmdRun :: RunIndexer
      }

data SomeIndexer = forall opts . SomeIndexer (Indexer opts)

indexers :: [(String, String, SomeIndexer)]
indexers =
  [ ("external",
      "Use a generic external indexer",
      SomeIndexer External.externalIndexer)
  , ("flow",
      "Index JS/Flow code",
      SomeIndexer Flow.indexer)
  , ("hack",
      "Index Hack code",
      SomeIndexer Hack.indexer)
  ]

instance Plugin IndexCommand where
  parseCommand =
    commandParser "index" (progDesc "Index some source code") $ do
      indexCmdRepo <- repoOpts
      indexCmdRun <- asum langs
      indexCmdRoot <- strArgument (metavar "ROOT")
      return Index{..}
   where
     langs = flip map indexers $ \(cmd, desc, SomeIndexer indexer) ->
       commandParser cmd (progDesc desc) $ do
         opts <- indexerOptParser indexer
         return (indexerRun indexer opts)

  runCommand _evb _cfg backend Index{..} = do
    projectRoot <- getCurrentDirectory
    withSystemTempDirectory "glean-index" $ \tmpdir -> do
    fillDatabase backend Nothing indexCmdRepo "" (die 1 "repo already exists") $
      indexCmdRun (Some backend) indexCmdRepo
        IndexerParams {
          indexerRoot = indexCmdRoot,
          indexerProjectRoot = projectRoot,
          indexerOutput = tmpdir,
          indexerGroup = ""
        }

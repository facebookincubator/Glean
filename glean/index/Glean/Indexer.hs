{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer
  ( Indexer(..)
  , IndexerParams(..)
  , RunIndexer
  , indexerWithNoOptions
  , indexerThen
  ) where

import Options.Applicative

import Glean
import Glean.LocalOrRemote
import Glean.Util.Some


-- | An 'Indexer' knows how to index some source code.
--
-- Indexers have command-line options, and a function to index the
-- code specified by the 'TestConfig' using a given 'Env' as a
-- backend.
--
data Indexer opts = Indexer
  { indexerShortName :: String
  , indexerDescription :: String
  , indexerOptParser :: Parser opts
  , indexerRun :: opts -> RunIndexer
  }

type RunIndexer = Some LocalOrRemote -> Repo -> IndexerParams -> IO ()

data IndexerParams = IndexerParams
  { indexerRoot :: FilePath
    -- ^ path containing source files to index
  , indexerProjectRoot :: FilePath
    -- ^ path to the root of the repository
  , indexerOutput :: FilePath
    -- ^ Where to put temporary files. The caller will choose whether
    -- to create a temporary directory, or to use one specified by the
    -- user in case they want to keep the temporary files for
    -- debugging purposes.
  , indexerGroup :: String
    -- ^ which "group" to use. This is for when we run a set of tests
    -- in multiple different ways. The interpretation of "group" is up
    -- to the indexing backend; indexers that don't need to support
    -- groups can ignore this.
  } deriving Show

-- | An indexer composed of two separate indexing tasks. The left
-- indexer is run before the right indexer.  This is useful when we
-- have a base indexer and we want to add a deriving pass, for
-- example.
--
-- The right indexer doesn't have any options. It's possible to
-- generalise this, but this more restricted form was found to be more
-- convenient when used with 'Driver'.
indexerThen :: Indexer a -> RunIndexer -> Indexer a
indexerThen indexer run2 = indexer
  { indexerRun = \opts backend repo params -> do
      indexerRun indexer opts backend repo params
      run2 backend repo params
  }

indexerWithNoOptions :: String -> RunIndexer -> Indexer ()
indexerWithNoOptions name run = Indexer
  { indexerShortName = name
  , indexerDescription = name
  , indexerOptParser = pure ()
  , indexerRun = const run
  }

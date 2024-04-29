{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Snapshot.Driver
  ( Driver(..)
  , driverFromIndexer
  , externalDriver
  , defaultCreateDatabase
  ) where

import System.Exit

import Glean
import Glean.LocalOrRemote
import Glean.Indexer
import Glean.Indexer.External
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Transform
import Glean.Util.Some

-- | A test driver describes how to run a set of tests. It is passed to
-- 'Glean.Regression.Snapshot.testMain' to make a complete test executable.
data Driver opts = Driver
  { driverIndexer :: Indexer opts
      -- ^ test data generator, for a given test group
  , driverGroups :: opts -> [Group]
      -- ^ groups - Test will be executed once for each group, with
      -- 'testGroup' set appropriately. If empty, test will be
      -- executed once with 'testGroup' set to "".
  , driverTransforms :: Transforms
      -- ^ Additional query result transformers.
  , driverCreateDatabase :: CreateDatabase opts
      -- ^ How to create a DB for this driver
  }

type CreateDatabase opts
  = opts
  -> Some LocalOrRemote
  -> (opts -> RunIndexer)
  -> TestConfig
  -> IO ()

type Group = String

defaultCreateDatabase :: CreateDatabase opts
defaultCreateDatabase opts backend indexer test = do
  let repo = testRepo test
  fillDatabase backend repo "" Nothing (die "repo already exists") $
    runIndexerForTest backend (indexer opts) test

driverFromIndexer :: Indexer opts -> Driver opts
driverFromIndexer indexer = Driver
  { driverIndexer = indexer
  , driverGroups = const []
  , driverTransforms = mempty
  , driverCreateDatabase = defaultCreateDatabase
  }

-- | A 'Driver' using an external 'Indexer'. See
-- "Glean.Indexer.External".
--
-- This driver doesn't support multiple groups; that could be added if
-- necessary.
externalDriver :: Driver Ext
externalDriver = driverFromIndexer externalIndexer

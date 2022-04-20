{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Snapshot.Driver
  ( Driver(..)
  , emptyDriver
  , driverFromIndexer
  , externalDriver
  ) where

import Glean.Indexer
import Glean.Indexer.External
import Glean.Regression.Snapshot.Transform

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
  }

type Group = String

emptyDriver :: Driver ()
emptyDriver = Driver
  { driverIndexer = Indexer (pure ()) (\_ _ _ _ -> return ())
  , driverGroups = const []
  , driverTransforms = mempty
  }

driverFromIndexer :: Indexer opts -> Driver opts
driverFromIndexer indexer = emptyDriver
  { driverIndexer = indexer
  , driverGroups = const []
  }

-- | A 'Driver' using an external 'Indexer'. See
-- "Glean.Indexer.External".
--
-- This driver doesn't support multiple groups; that could be added if
-- necessary.
externalDriver :: Driver Ext
externalDriver = emptyDriver
  { driverIndexer = externalIndexer
  , driverGroups = const []
  }

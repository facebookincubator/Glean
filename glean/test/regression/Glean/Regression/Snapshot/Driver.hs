-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Glean.Regression.Snapshot.Driver
  ( Driver(..)
  , emptyDriver
  , driverFromIndexer
  , externalDriver
  ) where

import Glean.Regression.Indexer
import Glean.Regression.Indexer.External
import Glean.Regression.Snapshot.Transform

-- | A test driver describes how to run a set of tests. It is passed to
-- 'Glean.Regression.Snapshot.testMain' to make a complete test executable.
data Driver opts = Driver
  { driverIndexer :: Indexer opts
      -- ^ test data generator
  , driverGroups :: opts -> [String]
      -- ^ groups - Test will be executed once for each group, with 'testGroup'
      -- set appropriately. If empty, test will be executed once with
      -- 'testGroup' set to "".
  , driverTransforms :: Transforms
      -- ^ Additional query result transformers.
  }

emptyDriver :: Driver ()
emptyDriver = Driver
  { driverIndexer = Indexer (pure ()) (\_ _ _ -> return ())
  , driverGroups = const []
  , driverTransforms = mempty
  }

driverFromIndexer :: Indexer opts -> Driver opts
driverFromIndexer indexer = emptyDriver
  { driverIndexer = indexer
  , driverGroups = const []
  }

-- | A 'Driver' using an external 'Indexer'. See
-- "Glean.Regression.Indexer.External".
externalDriver :: Driver Ext
externalDriver = emptyDriver
  { driverIndexer = externalIndexer
  , driverGroups = extGroups
  }

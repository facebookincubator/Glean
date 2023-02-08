{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Derive.Digest.Test.Driver
  ( main
  ) where

import System.FilePath ((</>))

import Glean.Indexer
import Glean.Indexer.List (cmdLineParser)
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot
import Glean.Derive.Digest.Lib

driver :: Driver RunIndexer
driver = driverFromIndexer $
  Indexer {
    indexerShortName = "multiple",
    indexerDescription = "Choose an indexer to run",
    indexerOptParser = cmdLineParser,
    indexerRun = id
    } `indexerThen` deriveDigests
  where
    deriveDigests backend repo params =
      derive backend repo Config
        { hashFunction = id
        , pathAdaptor = (indexerRoot params </>)
        }

main :: IO ()
main = testMain driver

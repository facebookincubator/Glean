-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

-- | Run a snapshot test with an external indexer

module Glean.Regression.Driver.External (main) where

import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = testMain externalDriver

-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Test.Driver
  ( main
  ) where

import Derive.Env (withEnv)
import Derive.HackDeclarationTarget (deriveHackDeclarationTarget)
import qualified Derive.Types as DT
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Indexer.External
import Glean.Regression.Snapshot (testMain)
import Glean.Regression.Snapshot.Driver

indexer :: Indexer Ext
indexer =
  externalIndexer `indexerThen` \test env ->
    withEnv (DT.defaultConfig (testRepo test)) env
      deriveHackDeclarationTarget

main :: IO ()
main = testMain (externalDriver { driverIndexer = indexer })

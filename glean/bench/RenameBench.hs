{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module RenameBench (main) where

import Criterion.Types

import Glean.Backend.Local (loadDbSchema)
import Glean.Database.Test
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup (withCanLookup)
import Glean.RTS.Types (lowestFid)
import Glean.Database.Schema (DbSchema(..))
import Glean.Util.Benchmark

import TestBatch

main :: IO ()
main = benchmarkMain $ \run ->
  withEmptyTestDB [] $ \env repo -> do
  schema <- loadDbSchema env repo
  batch <- testBatch 500000 env repo
  empty_facts <- FactSet.new lowestFid
  withCanLookup empty_facts $ \empty_lookup -> do

  let rename = FactSet.renameFacts
        (schemaInventory schema)
        empty_lookup
        lowestFid
        batch
        False

  _ <- rename

  run
    [ bench "renameFacts" $ whnfIO rename ]

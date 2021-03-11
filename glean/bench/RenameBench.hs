module RenameBench (main) where

import Criterion.Types

import qualified Glean.Backend as Backend
import Glean.Database.Test
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Lookup (withLookup)
import Glean.RTS.Types (lowestFid)
import Glean.Database.Schema (DbSchema(..))
import Glean.Util.Benchmark

import TestBatch

main :: IO ()
main = benchmarkMain $ \run ->
  withEmptyTestDB [] $ \env repo -> do
  schema <- Backend.loadDbSchema env repo
  batch <- testBatch 500000 env repo
  empty_facts <- FactSet.new lowestFid
  withLookup empty_facts $ \empty_lookup -> do

  let rename = FactSet.renameFacts
        (schemaInventory schema)
        empty_lookup
        lowestFid
        batch

  _ <- rename

  run
    [ bench "renameFacts" $ whnfIO rename ]

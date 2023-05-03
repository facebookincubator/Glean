{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module InventoryTest
  ( main
  ) where

import qualified Data.Map as Map
import Test.HUnit

import TestRunner

import Glean.Database.Config
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Init
import Glean.RTS.Foreign.Inventory (Inventory)
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Internal.Types

serializeTest :: Inventory -> Test
serializeTest inventory = TestCase $ assertBool "roundtrip" $
  Inventory.deserialize (Inventory.serialize inventory) == inventory

main :: IO ()
main = withUnitTest $ do
  schema <- parseSchemaDir schemaSourceDir
  dbSchema <- newDbSchema Nothing schema LatestSchemaAll readWriteContent
  let inventory = schemaInventory dbSchema

      -- inventory serialization/deserialization should work if there
      -- are gaps in the Pid range.
      stored = toStoredSchema dbSchema
      pids = storedSchema_predicateIds stored
      newStored = stored { storedSchema_predicateIds = Map.mapKeys (*2) pids }

  newDbSchema <- fromStoredSchema Nothing newStored readWriteContent
  let newInventory = schemaInventory newDbSchema

  testRunner $ TestList
    [ TestLabel "serialize" $ serializeTest inventory
    , TestLabel "serializeDeleted" $ serializeTest newInventory
    ]

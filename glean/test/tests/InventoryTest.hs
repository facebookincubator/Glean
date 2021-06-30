-- Copyright (c) Facebook, Inc. and its affiliates.

module InventoryTest
  ( main
  ) where

import Test.HUnit

import TestRunner

import Glean.Database.Config
import Glean.Init
import Glean.RTS.Foreign.Inventory (Inventory)
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Schema

serializeTest :: Inventory -> Test
serializeTest inventory = TestCase $ assertBool "roundtrip" $
  Inventory.deserialize (Inventory.serialize inventory) == inventory

main :: IO ()
main = withUnitTest $ do
  (schemaSource, schemas) <- parseSchemaDir schemaSourceDir
  inventory <- schemaInventory <$>
    newDbSchema schemaSource schemas readWriteContent

  testRunner $ TestList
    [ TestLabel "serialize" $ serializeTest inventory
    ]

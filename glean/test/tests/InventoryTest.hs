{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module InventoryTest
  ( main
  ) where

import Control.Monad (forM)
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Map as Map
import Test.HUnit

import TestRunner
import Util.IO (listDirectoryRecursive)

import Glean.Database.Config
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Init
import Glean.Internal.Types (storedSchema_predicateIds)
import Glean.RTS.Foreign.Inventory (Inventory)
import qualified Glean.RTS.Foreign.Inventory as Inventory

serializeTest :: Inventory -> IO ()
serializeTest inventory = assertBool "roundtrip" $
  Inventory.deserialize (Inventory.serialize inventory) == inventory

mkSchema
  :: (forall k v . HashMap k v -> [(k,v)])
  -> SchemaIndex
  -> IO DbSchema
mkSchema toList schemaIndex =
  newDbSchemaForTesting toList Nothing schemaIndex LatestSchemaAll
    readWriteContent def

main :: IO ()
main = withUnitTest $ do
  schema <- parseSchemaDir schemaSourceDir

  testRunner $ TestList
    [ TestLabel "serialize" $ TestCase $ do
        inventory <- schemaInventory <$> mkSchema HM.toList schema
        serializeTest inventory
    , TestLabel "serializeDeleted" $ TestCase $ do
        dbSchema <- mkSchema HM.toList schema
        let -- inventory serialization/deserialization should work if there
            -- are gaps in the Pid range.
            stored = toStoredSchema dbSchema
            pids = storedSchema_predicateIds stored
            newStored = stored {
              storedSchema_predicateIds = Map.mapKeys (*2) pids
              }
        newDbSchema <- fromStoredSchema Nothing newStored readWriteContent def
        serializeTest (schemaInventory newDbSchema)
    , TestLabel "determinism" $ TestList
      [ TestLabel "permuted schema files" $
        deterministicOnFilesTest schemaSourceDir
      , TestLabel "permuted HashMaps" $
        deterministicOnHashMaps schemaSourceDir
      ]
    ]

deterministicOnFilesTest :: FilePath -> Test
deterministicOnFilesTest schemaSourceDir = TestCase $ do
  files <- listDirectoryRecursive schemaSourceDir
  permutedFiles <- mapM catSchemaFiles (permuteList files)
  schemas <-
    mapM
      (either error (mkSchema HM.toList) . processOneSchema mempty)
      permutedFiles
  testDeterminism schemas

deterministicOnHashMaps :: FilePath -> Test
deterministicOnHashMaps schemaSourceDir = TestCase $ do
  files <- catSchemaFiles =<< listDirectoryRecursive schemaSourceDir
  schemas1 <-
    either error (mkSchema HM.toList . (`SchemaIndex` [])) $
      processSchemaForTesting HM.toList mempty files
  schemas2 <-
    either error (mkSchema (reverse . HM.toList) . (`SchemaIndex` [])) $
      processSchemaForTesting (reverse . HM.toList) mempty files
  testDeterminism [schemas1, schemas2]

--------------------------------------------------------------------------------

testDeterminism :: [DbSchema] -> IO ()
testDeterminism permutations = do
  assertBool "Test input is empty or a singleton" (length permutations > 1)
  inventories <- forM permutations $ \s -> do
    let i = schemaInventory s
    let !serial = Inventory.serialize i
    return (serial, s)
  let unique = Map.fromList inventories
      assertMsg = "Inventory is not deterministic: " <> show (length unique)

  assertBool assertMsg (length unique == 1)

permuteList :: [a] -> [[a]]
permuteList list = [list, reverse list]

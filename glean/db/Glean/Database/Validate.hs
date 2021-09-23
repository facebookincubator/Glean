-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Validate
  ( Validate(..)
  , validate
  , computeOwnership
  ) where

import Control.Concurrent.STM

import Glean.Database.Schema
import qualified Glean.Database.Storage as Storage
import Glean.Database.Open (readDatabase, withOpenDatabase)
import Glean.Database.Types (Env, OpenDB(..))
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Types (Repo)

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \odb db ->
  Inventory.validate (schemaInventory (odbSchema odb)) val db

computeOwnership :: Env -> Repo -> IO ()
computeOwnership env repo = withOpenDatabase env repo $ \OpenDB{..} -> do
  own <- Storage.computeOwnership odbHandle (schemaInventory odbSchema)
  Storage.storeOwnership odbHandle own
  newOwn <- Storage.getOwnership odbHandle
  atomically $ writeTVar odbOwnership newOwn

module Glean.Database.Validate
  ( Validate(..)
  , validate
  , computeOwnership
  ) where

import Glean.Database.Repo (Repo)
import qualified Glean.Database.Storage as Storage
import Glean.Database.Stuff (readDatabase, withOpenDatabase)
import Glean.Database.Types (Env, OpenDB(..))
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Schema

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \schema db ->
  Inventory.validate (schemaInventory schema) val db

computeOwnership :: Env -> Repo -> IO ()
computeOwnership env repo = withOpenDatabase env repo $ \OpenDB{..} ->
  Storage.computeOwnership odbHandle (schemaInventory odbSchema)

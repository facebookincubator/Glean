module Glean.Database.Validate
  ( Validate(..)
  , validate
  ) where

import Glean.Database.Repo (Repo)
import Glean.Database.Stuff (readDatabase)
import Glean.Database.Types (Env)
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Schema

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \schema db ->
  Inventory.validate (schemaInventory schema) val db

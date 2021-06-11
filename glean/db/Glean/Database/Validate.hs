module Glean.Database.Validate
  ( Validate(..)
  , validate
  , computeOwnership
  ) where

import Control.Monad

import Glean.Database.Schema
import qualified Glean.Database.Storage as Storage
import Glean.Database.Stuff (readDatabase, withOpenDatabase)
import Glean.Database.Types (Env, OpenDB(..))
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Types (Repo)

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \odb db ->
  Inventory.validate (schemaInventory (odbSchema odb)) val db

computeOwnership :: Env -> Repo -> IO ()
computeOwnership env repo = withOpenDatabase env repo $ \OpenDB{..} ->
  void $ Storage.computeOwnership odbHandle (schemaInventory odbSchema)

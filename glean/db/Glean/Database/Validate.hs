{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Validate
  ( Validate(..)
  , validate
  ) where

import Glean.Database.Open
import Glean.Database.Schema
import Glean.Database.Types
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Types

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \odb db ->
  Inventory.validate (schemaInventory (odbSchema odb)) val db

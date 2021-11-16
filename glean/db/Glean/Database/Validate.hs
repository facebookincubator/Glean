{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Validate
  ( Validate(..)
  , validate
  , computeOwnership
  ) where

import Control.Concurrent.STM
import Control.Exception

import Glean.Database.Schema
import qualified Glean.Database.Storage as Storage
import Glean.Database.Open (readDatabase, withOpenDatabase)
import Glean.Database.Types
import Glean.RTS.Foreign.Inventory (Validate(..))
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Types (Repo, Exception(..))
import Glean.Util.Mutex

validate :: Env -> Repo -> Validate -> IO ()
validate env repo val = readDatabase env repo $ \odb db ->
  Inventory.validate (schemaInventory (odbSchema odb)) val db

computeOwnership :: Env -> Repo -> IO ()
computeOwnership env repo = withOpenDatabase env repo $ \OpenDB{..} -> do
  own <- Storage.computeOwnership odbHandle (schemaInventory odbSchema)
  case odbWriting of
    Nothing -> throwIO $ Exception "computeOwnership: read only"
    Just writing -> withMutex (wrLock writing) $ const $
      Storage.storeOwnership odbHandle own
  -- read the StoredOwnership now to update it with the computed sets
  newOwn <- Storage.getOwnership odbHandle
  atomically $ writeTVar odbOwnership newOwn

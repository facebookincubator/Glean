{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Diff (diff, DiffOptions(..), Result(..)) where

import Control.Monad (when)
import Data.Default (Default(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Numeric.Natural (Natural)

import Util.FFI (invoke)

import Glean (Repo)
import Glean.Database.Schema (schemaInventory)
import Glean.Database.Open (readDatabase)
import Glean.Database.Types (Env, odbSchema)
import Glean.FFI (with)
import Glean.RTS.Foreign.Inventory (Inventory, predicates)
import Glean.RTS.Foreign.Lookup (withLookup, Lookup)

data DiffOptions =  DiffOptions
  { opt_logAdded :: Bool -- ^ log IDs of added facts
  , opt_batchSize :: Natural -- ^ how many facts to dedupe together
  }

instance Default DiffOptions where
  def = DiffOptions
    { opt_logAdded = False
    , opt_batchSize = 10000
    }

data Result = Result
  { kept :: Int
  , added :: Int
  , removed :: Int
  }
  deriving (Show, Eq)

diff :: Env -> DiffOptions -> Repo -> Repo -> IO Result
diff env DiffOptions{..} one two =
  withInventory one $ \one_inventory one_lookup_ptr ->
  withInventory two $ \two_inventory two_lookup_ptr ->
  with one_inventory $ \inventory_ptr -> do
  one_preds <- predicates one_inventory
  two_preds <- predicates two_inventory
  when (one_preds /= two_preds) $ error "Incompatible database inventories"

  (kept, added, removed) <- invoke $ glean_diff
    inventory_ptr
    one_lookup_ptr
    two_lookup_ptr
    opt_logAdded
    (fromIntegral opt_batchSize)
  return $ Result
    (fromIntegral kept)
    (fromIntegral added)
    (fromIntegral removed)
  where
    withInventory repo f =
      readDatabase env repo $ \odb lookup ->
      withLookup lookup $ \lookup_ptr ->
      f (schemaInventory (odbSchema odb)) lookup_ptr

foreign import ccall safe glean_diff
  :: Ptr Inventory
  -> Ptr Lookup
  -> Ptr Lookup
  -> Bool       -- ^ log added
  -> CSize      -- ^ batch_size
  -> Ptr CSize  -- ^ kept
  -> Ptr CSize  -- ^ added
  -> Ptr CSize  -- ^ removed
  -> IO CString

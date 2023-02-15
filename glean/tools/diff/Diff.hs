{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Diff (diff, DiffOptions(..), Result(..)) where

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Data.Int (Int32)

import Util.FFI (invoke)

import Glean (Repo)
import Glean.Database.Schema (schemaInventory)
import Glean.Database.Open (readDatabase)
import Glean.Database.Types (Env, odbSchema)
import Glean.FFI (with)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup (withLookup, Lookup)

newtype DiffOptions =  DiffOptions
  { logAdded :: Bool -- ^ log IDs of added facts
  }

data Result = Result
  { kept :: Int
  , added :: Int
  , removed :: Int
  }
  deriving (Show, Eq)

diff :: Env -> DiffOptions -> Repo -> Repo -> IO Result
diff env DiffOptions{..} one two =
  withInventory one $ \_ original_ptr ->
  withInventory two $ \new_inventory_ptr new_ptr -> do
  (kept, added, removed) <- invoke $ glean_diff
    new_inventory_ptr
    original_ptr
    new_ptr
    logAdded
  return $ Result
    (fromIntegral kept)
    (fromIntegral added)
    (fromIntegral removed)
  where
    withInventory repo f =
      readDatabase env repo $ \odb lookup ->
      with (schemaInventory (odbSchema odb)) $ \inventory_ptr ->
      withLookup lookup $ \lookup_ptr ->
      f inventory_ptr lookup_ptr

foreign import ccall safe glean_diff
  :: Ptr Inventory
  -> Ptr Lookup
  -> Ptr Lookup
  -> Bool       -- ^ log added
  -> Ptr Int32  -- ^ kept
  -> Ptr Int32  -- ^ added
  -> Ptr Int32  -- ^ removed
  -> IO CString

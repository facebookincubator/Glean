{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Diff (diff, DiffOptions(..), Result(..)) where

import Control.Monad (when)
import Data.Default (Default(..))
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Foreign (withArray, fromBool)
import Foreign.C (CBool(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Numeric.Natural (Natural)

import Util.FFI (invoke)

import Glean (Repo)
import Glean.Angle.Types (PredicateId(..))
import Glean.Database.Schema (DbSchema(..), PredicateDetails(..), schemaInventory)
import Glean.Database.Open (readDatabase)
import Glean.Database.Types (Env, odbSchema)
import Glean.FFI (with)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup (withLookup, Lookup)
import Glean.RTS.Types (Pid(..))
import Glean.Types (iNVALID_ID)

data DiffOptions =  DiffOptions
  { opt_logAdded :: Bool -- ^ log IDs of added facts
  , opt_batchSize :: Natural -- ^ how many facts to dedupe together
  , opt_allowDifferentSchemas :: Bool
    -- ^ diff dbs with different schemas.
    -- Facts will be compared based on their PredicateId.
  }

instance Default DiffOptions where
  def = DiffOptions
    { opt_logAdded = False
    , opt_batchSize = 10000
    , opt_allowDifferentSchemas = False
    }

data Result = Result
  { kept :: Int
  , added :: Int
  , removed :: Int
  }
  deriving (Show, Eq)

diff :: Env -> DiffOptions -> Repo -> Repo -> IO Result
diff env DiffOptions{..} one two =
  withSchema one $ \one_schema one_lookup_ptr ->
  withSchema two $ \two_schema two_lookup_ptr -> do
  let (lowestPid, pidSubst) = pidMappings two_schema one_schema
      inventoryMismatch = byPredId one_schema /= byPredId two_schema

  when (inventoryMismatch && not opt_allowDifferentSchemas) $
    error "Databases use different schemas"

  with (schemaInventory two_schema) $ \inventory_ptr -> do
  withArray pidSubst $ \pidSubst_ptr -> do
    (kept, added, removed) <- invoke $ glean_diff
      (fromIntegral $ length pidSubst)
      lowestPid
      pidSubst_ptr
      (fromBool inventoryMismatch)
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
    -- Mapping from Pid -> Pid
    pidMappings :: DbSchema -> DbSchema -> (Pid, [Pid])
    pidMappings from to = (lowest, list)
      where
        list = fmap
          (\pid -> Map.findWithDefault (Pid iNVALID_ID) pid mapping)
          [lowest .. highest]
        lowest  = fst $ Map.findMin mapping
        highest = fst $ Map.findMax mapping

        mapping :: Map Pid Pid
        mapping = Map.fromList
          $ HashMap.elems
          $ HashMap.intersectionWith (,) (byPredId from) (byPredId to)

    byPredId :: DbSchema -> HashMap PredicateId Pid
    byPredId schema = fmap predicatePid $ predicatesById schema

    withSchema repo f =
      readDatabase env repo $ \odb lookup ->
      withLookup lookup $ \lookup_ptr ->
      f (odbSchema odb) lookup_ptr

foreign import ccall safe glean_diff
  :: CSize
  -> Pid
  -> Ptr Pid
  -> CBool
  -> Ptr Inventory
  -> Ptr Lookup
  -> Ptr Lookup
  -> Bool       -- ^ log added
  -> CSize      -- ^ batch_size
  -> Ptr CSize  -- ^ kept
  -> Ptr CSize  -- ^ added
  -> Ptr CSize  -- ^ removed
  -> IO CString

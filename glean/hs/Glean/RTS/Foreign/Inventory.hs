{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Inventory
  ( Inventory
  , CompiledPredicate(..)
  , new
  , predicates
  , serialize
  , deserialize
  , Validate(..)
  , validate
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Default
import Data.List (unzip6)
import Foreign hiding (with, withMany, new)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Glean.FFI
import Glean.RTS.Typecheck
import Glean.RTS.Traverse
import Glean.RTS.Foreign.Bytecode (Subroutine)
import Glean.RTS.Foreign.Lookup (Lookup(..), CanLookup(..))
import Glean.RTS.Types (Pid(..))
import Glean.Types (PredicateRef(..))

-- | Information about predicates in an open DB
newtype Inventory = Inventory (ForeignPtr Inventory)

instance Object Inventory where
  wrap = Inventory
  unwrap (Inventory p) = p
  destroy = glean_inventory_free

-- | A compiled predicate definition
data CompiledPredicate = CompiledPredicate
  { compiledPid :: Pid
  , compiledRef :: PredicateRef
  , compiledTypecheck :: Subroutine CompiledTypecheck
  , compiledTraversal :: Subroutine CompiledTraversal
  }

-- | Create a new 'Inventory' from a list of predicate specs. Predicates will be
-- assigned consecutive ids starting with first. Each predicate is accompanied
-- by a bytecode subrouting for fact typechecking (from
-- 'Glean.RTS.Typecheck.checkSignature').
new :: [CompiledPredicate] -> Inventory
-- NOTE: This is pure because inventories are immutable.
new ps = unsafePerformIO $ withMany predicate ps $ \qs ->
  let (ids,
       name_ptrs,
       name_sizes,
       versions,
       tcs,
       trs) = unzip6 qs
  in
  withArray ids $ \p_ids ->
  withArray name_ptrs $ \p_name_ptrs ->
  withArray name_sizes $ \p_name_sizes ->
  withArray versions $ \p_versions ->
  withArray tcs $ \p_tcs ->
  withArray trs $ \p_trs ->
  construct $ invoke $ glean_inventory_new
    (fromIntegral n)
    p_ids
    p_name_ptrs
    p_name_sizes
    p_versions
    p_tcs
    p_trs
  where
    !n = length ps
    predicate CompiledPredicate{..} f =
      withUTF8Text (predicateRef_name compiledRef) $ \name_ptr name_size ->
      with compiledTypecheck $ \tc_ptr ->
      with compiledTraversal $ \tr_ptr ->
      f ( compiledPid
        , name_ptr
        , name_size
        , fromIntegral $ predicateRef_version compiledRef
        , tc_ptr
        , tr_ptr )

instance Eq Inventory where
  a == b = unsafePerformIO $
    with a $ \a_ptr ->
    with b $ \b_ptr -> do
      r <- invoke $ glean_inventory_equal a_ptr b_ptr
      return $ r /= 0

newtype PredicatePtr = PredicatePtr (Ptr PredicatePtr)
  deriving(Storable)

-- | Return all predicates from an 'Inventory'
predicates :: Inventory -> IO [(Pid, PredicateRef)]
predicates inventory = with inventory $ \inventory_ptr -> do
  (count, preds) <- invoke $ glean_inventory_predicates inventory_ptr
  usingMalloced preds $ forM [1 .. count] $ \i -> do
    p <- peekElemOff preds $ fromIntegral i - 1
    (pid, name_ptr, name_size, version) <- invoke $ glean_predicate_unpack p
    ref <- PredicateRef
      <$> fromUTF8 name_ptr name_size
      <*> pure (fromIntegral version)
    return (pid, ref)

serialize :: Inventory -> ByteString
serialize inventory = unsafePerformIO $
  with inventory $ \inventory_ptr -> do
    (bytes, size) <- invoke $ glean_inventory_serialize inventory_ptr
    unsafeMallocedByteString bytes size

deserialize :: ByteString -> Inventory
deserialize s = unsafePerformIO $
  unsafeWithBytes s $ \p n ->
  construct $ invoke $ glean_inventory_deserialize p n

data Validate = Validate
  { validateTypecheck :: Bool
  , validateKeys :: Bool
  , validateLimit :: Maybe Int
  }

instance Default Validate where
  def = Validate
    { validateTypecheck = True
    , validateKeys = True
    , validateLimit = Nothing
    }

validate :: CanLookup a => Inventory -> Validate -> a -> IO ()
validate inventory Validate{..} lookupable =
  with inventory $ \inventory_ptr ->
  withLookup lookupable $ \lookup ->
  invoke $ glean_validate
    inventory_ptr
    (if validateTypecheck then 1 else 0)
    (if validateKeys then 1 else 0)
    (maybe maxBound fromIntegral validateLimit)
    lookup

foreign import ccall unsafe glean_inventory_new
  :: CSize
  -> Ptr Pid
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr Int32
  -> Ptr (Ptr (Subroutine CompiledTypecheck))
  -> Ptr (Ptr (Subroutine CompiledTraversal))
  -> Ptr (Ptr Inventory)
  -> IO CString
foreign import ccall unsafe "&glean_inventory_free" glean_inventory_free
  :: Destroy Inventory

foreign import ccall unsafe glean_inventory_predicates
  :: Ptr Inventory
  -> Ptr CSize
  -> Ptr (Ptr PredicatePtr)
  -> IO CString

foreign import ccall unsafe glean_inventory_serialize
  :: Ptr Inventory
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

foreign import ccall unsafe glean_inventory_deserialize
  :: Ptr ()
  -> CSize
  -> Ptr (Ptr Inventory)
  -> IO CString

foreign import ccall unsafe glean_inventory_equal
  :: Ptr Inventory
  -> Ptr Inventory
  -> Ptr CBool
  -> IO CString

foreign import ccall unsafe glean_predicate_unpack
  :: PredicatePtr
  -> Ptr Pid
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr Int32
  -> IO CString

foreign import ccall safe glean_validate
  :: Ptr Inventory
  -> CChar
  -> CChar
  -> CSize
  -> Lookup
  -> IO CString

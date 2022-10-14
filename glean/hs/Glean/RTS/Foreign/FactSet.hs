{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.FactSet
  ( FactSet
  , new
  , factCount
  , factMemory
  , allocatedMemory
  , predicateStats
  , firstFreeId
  , serialize
  , serializeReorder
  , append
  , rebase
  , renameFacts
  ) where

import Control.Exception
import Data.Int
import Data.Vector.Storable as Vector
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Util.FFI

import Glean.FFI
import Glean.RTS.Foreign.Define
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup (Lookup(..), CanLookup(..))
import Glean.RTS.Foreign.LookupCache (LookupCache)
import Glean.RTS.Foreign.Stacked (stacked)
import Glean.RTS.Foreign.Stats (marshalPredicateStats)
import Glean.RTS.Foreign.Subst (Subst)
import Glean.RTS.Types (Fid(..), Pid(..))
import qualified Glean.Types as Thrift

-- An environment for defining facts
newtype FactSet = FactSet (ForeignPtr FactSet)

instance Object FactSet where
  wrap = FactSet
  unwrap (FactSet p) = p
  destroy = glean_factset_free

instance CanLookup FactSet where
  lookupName _ = "factset"
  withLookup x f = with x $ f . glean_factset_lookup

instance CanDefine FactSet where
  withDefine x f = with x $ f . glean_factset_define

new :: Fid -> IO FactSet
new next_id = construct $ invoke $ glean_factset_new next_id

factCount :: FactSet -> IO Int
factCount facts = fromIntegral <$> with facts glean_factset_fact_count

factMemory :: FactSet -> IO Int
factMemory facts = fromIntegral <$> with facts glean_factset_fact_memory

allocatedMemory :: FactSet -> IO Int
allocatedMemory facts =
  fromIntegral <$> with facts glean_factset_allocated_memory

predicateStats :: FactSet -> IO [(Pid, Thrift.PredicateStats)]
predicateStats facts = with facts
    $ marshalPredicateStats . glean_factset_predicateStats

firstFreeId :: FactSet -> IO Fid
firstFreeId facts = with facts glean_factset_first_free_id

mkBatch :: IO (Fid, CSize, Ptr (), CSize) -> IO Thrift.Batch
mkBatch fn = mask_ $ do
  (Fid first_id, count, facts_data, facts_size) <- fn
  Thrift.Batch first_id (fromIntegral count)
    <$> unsafeMallocedByteString facts_data facts_size
    <*> pure Nothing
    <*> pure mempty

serialize :: FactSet -> IO Thrift.Batch
serialize facts =
  with facts $ \facts_ptr -> do
  mkBatch $ invoke $ glean_factset_serialize facts_ptr

serializeReorder :: FactSet -> Vector Int64 -> IO Thrift.Batch
serializeReorder facts order =
  with facts $ \facts_ptr -> do
  unsafeWith order $ \order_ptr -> do
    mkBatch $ invoke $ glean_factset_serializeReorder
      facts_ptr
      order_ptr
      (fromIntegral (Vector.length order))

rebase :: Inventory -> Thrift.Subst -> LookupCache -> FactSet -> IO FactSet
rebase inventory Thrift.Subst{..} cache facts =
  with inventory $ \inventory_ptr ->
  unsafeWith subst_ids $ \ids_ptr ->
  with cache $ \cache_ptr ->
  with facts $ \facts_ptr ->
  construct $ invoke $
    glean_factset_rebase
      facts_ptr
      inventory_ptr
      (Fid subst_firstId)
      (fromIntegral $ Vector.length subst_ids)
      ids_ptr
      cache_ptr

append :: FactSet -> FactSet -> IO ()
append target source =
  with target $ \target_ptr ->
  with source $ \source_ptr ->
  invoke $ glean_factset_append target_ptr source_ptr

-- Prepare a Thrift batch for writing into the database by renaming and
-- deduplicating facts.
renameFacts
  :: CanLookup l
  => Inventory          -- ^ where to lookup predicates
  -> l                  -- ^ where to lookup facts
  -> Fid                -- ^ first free fact id in the database
  -> Thrift.Batch       -- ^ batch to rename
  -> IO (FactSet, Subst)
                        -- ^ resulting facts and substitution
renameFacts inventory base next batch = do
  added <- new next
  subst <- defineUntrustedBatch (stacked base added) inventory batch
  return (added, subst)

foreign import ccall unsafe glean_factset_new
  :: Fid -> Ptr (Ptr FactSet) -> IO CString
foreign import ccall unsafe "&glean_factset_free" glean_factset_free
  :: FunPtr (Ptr FactSet -> IO ())

foreign import ccall unsafe glean_factset_fact_count
  :: Ptr FactSet -> IO CSize

foreign import ccall unsafe glean_factset_fact_memory
  :: Ptr FactSet -> IO CSize

foreign import ccall unsafe glean_factset_allocated_memory
  :: Ptr FactSet -> IO CSize

foreign import ccall safe glean_factset_predicateStats
  :: Ptr FactSet
  -> Ptr CSize
  -> Ptr (Ptr Int64)
  -> Ptr (Ptr Word64)
  -> Ptr (Ptr Word64)
  -> IO CString

foreign import ccall unsafe glean_factset_first_free_id
  :: Ptr FactSet -> IO Fid

foreign import ccall unsafe glean_factset_lookup
  :: Ptr FactSet -> Ptr Lookup

foreign import ccall unsafe glean_factset_define
  :: Ptr FactSet -> Define

foreign import ccall unsafe glean_factset_serialize
  :: Ptr FactSet
  -> Ptr Fid
  -> Ptr CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

foreign import ccall unsafe glean_factset_serializeReorder
  :: Ptr FactSet
  -> Ptr Int64
  -> CSize
  -> Ptr Fid
  -> Ptr CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

foreign import ccall unsafe glean_factset_rebase
  :: Ptr FactSet
  -> Ptr Inventory
  -> Fid
  -> CSize
  -> Ptr Int64
  -> Ptr LookupCache
  -> Ptr (Ptr FactSet)
  -> IO CString

foreign import ccall unsafe glean_factset_append
  :: Ptr FactSet -> Ptr FactSet -> IO CString

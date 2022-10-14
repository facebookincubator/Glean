{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Benchmarking
  ( FactBlock
  , createFactBlock
  , factCount
  , factMemory

  , defineEach
  , lookupEachType
  , lookupEachById
  , lookupEachByKey
  , seekToEach
  , seekCount
  ) where

import qualified Data.Vector.Storable as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Util.FFI

import Glean.FFI
import Glean.RTS.Foreign.Define (Define(..), CanDefine(..))
import Glean.RTS.Foreign.Lookup (Lookup(..), CanLookup(..))
import Glean.RTS.Types (Pid(..))

newtype FactBlock = FactBlock (ForeignPtr FactBlock)

instance Object FactBlock where
  wrap = FactBlock
  unwrap (FactBlock p) = p
  destroy = glean_benchmarking_factblock_free

createFactBlock :: CanLookup l => l -> IO FactBlock
createFactBlock l = withLookup  l $ \look ->
  construct $ invoke $ glean_benchmarking_factblock_create look

factCount :: FactBlock -> IO Int
factCount block =
  fromIntegral <$> with block glean_benchmarking_factblock_fact_count

factMemory :: FactBlock -> IO Int
factMemory block =
  fromIntegral <$> with block glean_benchmarking_factblock_fact_memory

-- | Define each fact from the 'FactBlock'. Fail if the ids of the defined
-- facts are different from their ids in the 'FactBlock'
defineEach :: CanDefine d => d -> FactBlock -> IO Bool
defineEach d block =
  withDefine d $ \def ->
  with block $ \ptr -> do
    r <- invoke $ glean_benchmarking_define_each def ptr
    return $ r /= 0

-- | Look up the type of each fact. Fail if the types are different from those
-- in the 'FactBlock'.
lookupEachType :: CanLookup l => l -> FactBlock -> IO Bool
lookupEachType l block =
  withLookup l $ \look ->
  with block $ \ptr -> do
    r <- invoke $ glean_benchmarking_lookup_each_type look ptr
    return $ r /= 0

-- | Look up each fact by id. Fail if a fact doesn't exist or if its type or key
-- are different.
lookupEachById :: CanLookup l => l -> FactBlock -> IO Bool
lookupEachById l block =
  withLookup l $ \look ->
  with block $ \ptr -> do
    r <- invoke $ glean_benchmarking_lookup_each_by_id look ptr
    return $ r /= 0

-- | Look up each fact by key. Fail if a fact doesn't exist or if its id or type
-- are different.
lookupEachByKey :: CanLookup l => l -> FactBlock -> IO Bool
lookupEachByKey l block =
  withLookup l $ \look ->
  with block $ \ptr -> do
    r <- invoke $ glean_benchmarking_lookup_each_by_key look ptr
    return $ r /= 0

-- | Seek to each fact by key. Fail if a fact doesn't exist or if its id or type
-- are different.
seekToEach ::  CanLookup l => l -> FactBlock -> IO Bool
seekToEach l block =
  withLookup l $ \look ->
  with block $ \ptr -> do
    r <- invoke $ glean_benchmarking_seek_to_each look ptr
    return $ r /= 0

-- | For each 'Pid', iterate through all its facts via seek. Return the total
-- number of facts iterated over.
seekCount :: CanLookup a => a -> V.Vector Pid -> IO Int
seekCount look pids =
  withLookup look $ \look_ptr ->
  V.unsafeWith pids $ \pids_ptr ->
  fmap fromIntegral $ invoke $ glean_benchmarking_seek_count
    look_ptr
    pids_ptr
    (fromIntegral $ V.length pids)

foreign import ccall safe glean_benchmarking_factblock_create
  :: Ptr Lookup
  -> Ptr (Ptr FactBlock)
  -> IO CString

foreign import ccall unsafe "&glean_benchmarking_factblock_free"
  glean_benchmarking_factblock_free
  :: FunPtr (Ptr FactBlock -> IO ())

foreign import ccall unsafe glean_benchmarking_factblock_fact_count
  :: Ptr FactBlock
  -> IO CSize

foreign import ccall unsafe glean_benchmarking_factblock_fact_memory
  :: Ptr FactBlock
  -> IO CSize

foreign import ccall safe glean_benchmarking_define_each
  :: Define
  -> Ptr FactBlock
  -> Ptr CBool
  -> IO CString

foreign import ccall safe glean_benchmarking_lookup_each_type
  :: Ptr Lookup
  -> Ptr FactBlock
  -> Ptr CBool
  -> IO CString

foreign import ccall safe glean_benchmarking_lookup_each_by_id
  :: Ptr Lookup
  -> Ptr FactBlock
  -> Ptr CBool
  -> IO CString

foreign import ccall safe glean_benchmarking_lookup_each_by_key
  :: Ptr Lookup
  -> Ptr FactBlock
  -> Ptr CBool
  -> IO CString

foreign import ccall safe glean_benchmarking_seek_to_each
  :: Ptr Lookup
  -> Ptr FactBlock
  -> Ptr CBool
  -> IO CString

foreign import ccall safe glean_benchmarking_seek_count
  :: Ptr Lookup
  -> Ptr Pid
  -> CSize
  -> Ptr CSize
  -> IO CString

-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.Define
  ( Define(..)
  , CanDefine(..)
  , InvalidRedefinition(..)
  , defineFact
  , defineUntrustedBatch
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce (coerce)
import Data.Typeable
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Glean.FFI
import Glean.RTS.Builder
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Subst (Subst)
import Glean.RTS.Types (Fid(..), Pid(..), invalidFid)
import qualified Glean.Types as Thrift

-- | A reference to a thing we can define facts in
newtype Define = Define (Ptr Define)
  deriving(Storable)

-- | Class of things we can define facts in
class CanDefine a where
  withDefine :: a -> (Define -> IO b) -> IO b

instance CanDefine Define where
  withDefine l f = f l

newtype InvalidRedefinition = InvalidRedefinition String
  deriving(Show, Typeable)

instance Exception InvalidRedefinition

defineFact :: CanDefine a => a -> Pid -> Builder -> CSize -> IO Fid
defineFact facts pred clause key_size =
  withDefine facts $ \facts_ptr -> do
    id <- invoke $ glean_define_fact facts_ptr pred clause key_size
    when (id == invalidFid) $
      throwIO $ InvalidRedefinition "invalid fact redefinition"
    return id

-- Prepare a Thrift batch for writing into the database by renaming and
-- deduplicating facts.
defineUntrustedBatch
  :: CanDefine a
  => a                  -- ^ where to define facts
  -> Inventory          -- ^ inventory
  -> Thrift.Batch       -- ^ batch to rename
  -> IO Subst           -- ^ resulting substitution
defineUntrustedBatch facts inventory batch =
  withDefine facts $ \p_facts ->
  with inventory $ \p_inventory ->
  withIds $ \ids_ptr ->
  unsafeWithBytes (Thrift.batch_facts batch) $ \facts_ptr facts_size ->
    construct $ invoke $ glean_define_untrusted_batch
      p_facts
      p_inventory
      (Fid $ Thrift.batch_firstId batch)
      ids_ptr
      (fromIntegral $ Thrift.batch_count batch)
      facts_ptr
      facts_size
  where
    withIds f
      | Just ids <- Thrift.batch_ids batch =
          if fromIntegral (VS.length ids) == Thrift.batch_count batch
            then VS.unsafeWith (coerce ids) f
            else throwIO $
              Thrift.Exception "mismatch between count and ids.size in batch"
      | otherwise = f nullPtr

foreign import ccall unsafe glean_define_fact
  :: Define
  -> Pid
  -> Builder
  -> CSize
  -> Ptr Fid
  -> IO CString

foreign import ccall safe glean_define_untrusted_batch
  :: Define
  -> Ptr Inventory
  -> Fid
  -> Ptr Fid
  -> CSize
  -> Ptr ()
  -> CSize
  -> Ptr (Ptr Subst)
  -> IO CString

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Define
  ( Define(..)
  , CanDefine(..)
  , InvalidRedefinition(..)
  , DefineFlags(..)
  , defineFact
  , defineBatch
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce (coerce)
import Data.Default
import Data.Typeable
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils (fromBool)

import Util.FFI

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

data DefineFlags = DefineFlags
  { trustRefs :: Bool
    -- ^ Whether to trust that fact references have the right type.
    -- If the facts have already been typechecked then this can be set
    -- to True to improve performance.
  , ignoreRedef :: Bool
    -- ^ When True, if we see a redefinition of a key/value fact where
    -- the value is different, just discard the fact. When False,
    -- redefinition is an error. Redefinition may safely arise in
    -- cases where we have a cache of remote facts and the values have
    -- been rebased with respect to different caches: the values may
    -- be literally different but semantically identical.
  }

instance Default DefineFlags where
  def = DefineFlags
    { trustRefs = False
    , ignoreRedef = False
    }

-- Prepare a Thrift batch for writing into the database by renaming and
-- deduplicating facts.
defineBatch
  :: CanDefine a
  => a                  -- ^ where to define facts
  -> Inventory          -- ^ inventory
  -> Thrift.Batch       -- ^ batch to rename
  -> DefineFlags        -- ^ flags
  -> IO Subst           -- ^ resulting substitution
defineBatch facts inventory batch DefineFlags{..} =
  withDefine facts $ \p_facts ->
  with inventory $ \p_inventory ->
  withIds $ \ids_ptr ->
  unsafeWithBytes (Thrift.batch_facts batch) $ \facts_ptr facts_size ->
    construct $ invoke $ glean_define_batch
      p_facts
      p_inventory
      (Fid $ Thrift.batch_firstId batch)
      ids_ptr
      (fromIntegral $ Thrift.batch_count batch)
      facts_ptr
      facts_size
      (fromBool trustRefs)
      (fromBool ignoreRedef)
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

foreign import ccall safe glean_define_batch
  :: Define
  -> Ptr Inventory
  -> Fid
  -> Ptr Fid
  -> CSize
  -> Ptr ()
  -> CSize
  -> CBool
  -> CBool
  -> Ptr (Ptr Subst)
  -> IO CString

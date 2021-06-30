-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.Lookup
  ( Lookup(..)
  , CanLookup(..)
  , EmptyLookup(..)
  , startingId
  , firstFreeId
  , lookupFact
  , withSnapshot
) where

import Control.Exception (bracket)
import Data.Int
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Glean.FFI
import Glean.RTS.Types (Fid(..))
import qualified Glean.Types as Thrift

-- | A reference to a thing we can look up facts in
newtype Lookup = Lookup (Ptr Lookup)
  deriving(Storable)

-- | Class of things we can look up facts in
class CanLookup a where
  withLookup :: a -> (Lookup -> IO b) -> IO b

instance CanLookup Lookup where
  withLookup l f = f l

startingId :: CanLookup a => a -> IO Fid
startingId x = withLookup x $ \l -> invoke $ glean_lookup_starting_id l

firstFreeId :: CanLookup a => a -> IO Fid
firstFreeId x = withLookup x $ \l -> invoke $ glean_lookup_first_free_id l

data EmptyLookup = EmptyLookup

instance CanLookup EmptyLookup where
  withLookup EmptyLookup f =
    bracket
      (invoke glean_lookup_empty)
      glean_lookup_free
      f

lookupFact :: CanLookup a => a -> Fid -> IO (Maybe Thrift.Fact)
lookupFact look fid =
  withLookup look $ \look_ptr -> do
    (pid, key_ptr, key_size, val_ptr, val_size) <-
      invoke $ glean_lookup_fact look_ptr fid
    if pid == 0
      then return Nothing
      else fmap Just $ Thrift.Fact pid
            <$> unsafeMallocedByteString key_ptr key_size
            <*> unsafeMallocedByteString val_ptr val_size

-- | Restrict the Lookup to facts up to the specified fact id
withSnapshot :: CanLookup a => a -> Fid -> (Lookup -> IO b) -> IO b
withSnapshot base boundary f =
  withLookup base $ \base_ptr ->
  bracket
    (invoke $ glean_snapshot_new base_ptr boundary)
    glean_lookup_free
    f

foreign import ccall unsafe glean_lookup_empty
  :: Ptr Lookup -> IO CString

foreign import ccall unsafe glean_lookup_free
  :: Lookup -> IO ()

foreign import ccall unsafe glean_snapshot_new
  :: Lookup -> Fid -> Ptr Lookup -> IO CString

foreign import ccall unsafe glean_lookup_starting_id
  :: Lookup -> Ptr Fid -> IO CString
foreign import ccall unsafe glean_lookup_first_free_id
  :: Lookup -> Ptr Fid -> IO CString

foreign import ccall safe glean_lookup_fact
  :: Lookup
  -> Fid
  -> Ptr Int64
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

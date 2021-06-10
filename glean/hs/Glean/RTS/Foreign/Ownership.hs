module Glean.RTS.Foreign.Ownership
  ( UnitIterator
  , compute
  , UnitId(..)
  , Ownership
  ) where

import Foreign hiding (with)
import Foreign.C

import Glean.FFI
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup

newtype UnitIterator = UnitIterator (Ptr UnitIterator)
  deriving(Storable)

instance Static UnitIterator where
  destroyStatic = glean_ownership_unit_iterator_free

newtype UnitId = UnitId Word32

newtype Ownership = Ownership (ForeignPtr Ownership)

instance Object Ownership where
  wrap = Ownership
  unwrap (Ownership p) = p
  destroy = glean_ownership_free

compute
  :: CanLookup a
  => Inventory
  -> a
  -> UnitIterator
  -> IO Ownership
compute inv l iter =
  with inv $ \inv_ptr ->
  withLookup l $ \lookup ->
  construct $ invoke $ glean_ownership_compute inv_ptr lookup iter

foreign import ccall unsafe glean_ownership_unit_iterator_free
  :: UnitIterator -> IO ()

foreign import ccall safe glean_ownership_compute
  :: Ptr Inventory
  -> Lookup
  -> UnitIterator
  -> Ptr (Ptr Ownership)
  -> IO CString

foreign import ccall unsafe "&glean_ownership_free"
   glean_ownership_free :: FunPtr (Ptr Ownership -> IO ())

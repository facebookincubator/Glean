module Glean.RTS.Foreign.Ownership
  ( UnitIterator
  , compute
  , UnitId(..)
  ) where

import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

import Glean.FFI
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup

newtype UnitIterator = UnitIterator (Ptr UnitIterator)
  deriving(Storable)

instance Static UnitIterator where
  destroyStatic = glean_ownership_unit_iterator_free

newtype UnitId = UnitId Word32

compute
  :: CanLookup a
  => Inventory
  -> a
  -> UnitIterator
  -> IO ()
compute inv l iter =
  with inv $ \inv_ptr ->
  withLookup l $ \lookup ->
  invoke $ glean_ownership_compute inv_ptr lookup iter

foreign import ccall unsafe glean_ownership_unit_iterator_free
  :: UnitIterator -> IO ()

foreign import ccall safe glean_ownership_compute
  :: Ptr Inventory
  -> Lookup
  -> UnitIterator
  -> IO CString

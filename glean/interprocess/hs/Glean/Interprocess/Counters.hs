-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Interprocess.Counters (
  Counters, withTemp, get, set
) where

import Glean.FFI (invoke)

import Control.Exception (bracket)
import Data.Word (Word64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

newtype Counters = Counters (Ptr Counters)
  deriving(Storable)

withTemp :: Int -> (FilePath -> Counters -> IO a) -> IO a
withTemp n f = withSystemTempFile ".glean-counters" $ \path h -> do
  hClose h
  withCString path $ \cpath -> do
    invoke $ glean_interprocess_counters_create
      cpath
      (fromIntegral n)
    bracket
      (invoke $ glean_interprocess_counters_open cpath $ fromIntegral n)
      glean_interprocess_counters_close
      (f path)

get :: Counters -> Int -> IO Word64
get c i = invoke $ glean_interprocess_counters_get c (fromIntegral i)

set :: Counters -> Int -> Word64 -> IO ()
set c i x = invoke $ glean_interprocess_counters_set c (fromIntegral i) x

foreign import ccall unsafe glean_interprocess_counters_create
  :: CString -> CSize -> IO CString

foreign import ccall unsafe glean_interprocess_counters_open
  :: CString -> CSize -> Ptr Counters -> IO CString

foreign import ccall unsafe glean_interprocess_counters_close
  :: Counters -> IO ()

foreign import ccall unsafe glean_interprocess_counters_get
  :: Counters -> CSize -> Ptr Word64 -> IO CString

foreign import ccall unsafe glean_interprocess_counters_set
  :: Counters -> CSize -> Word64 -> IO CString

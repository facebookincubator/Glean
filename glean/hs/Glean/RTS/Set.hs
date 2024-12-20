{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Set
  ( RtsSet
  , withRtsSet
  , insertBuilder
  , buildSet
  , WordRtsSet
  , withWordRtsSet
  , insertWordRtsSet
  , insertBytesRtsSet
  , buildWordSet
  , buildWordSetBytes
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Foreign
import Foreign.C

import qualified Util.FFI as FFI

import Glean.RTS.Builder

newtype RtsSet = RtsSet (Ptr RtsSet)
  deriving(Storable)

withRtsSet ::  (MonadMask m, MonadIO m) => (RtsSet -> m a) -> m a
withRtsSet = bracket (liftIO newRtsSet) (liftIO . freeRtsSet)

newRtsSet :: IO RtsSet
newRtsSet = FFI.invoke glean_rtsset_new

insertBuilder :: RtsSet -> Builder -> IO ()
insertBuilder set builder = FFI.invoke $ glean_rtsset_insert set builder

buildSet :: RtsSet -> Builder -> IO ()
buildSet set builder = FFI.invoke $ glean_rtsset_build set builder

newtype WordRtsSet = WordRtsSet (Ptr WordRtsSet)
  deriving (Storable)

withWordRtsSet ::  (MonadMask m, MonadIO m) => (WordRtsSet -> m a) -> m a
withWordRtsSet = bracket (liftIO newWordRtsSet) (liftIO . freeWordRtsSet)

newWordRtsSet :: IO WordRtsSet
newWordRtsSet = FFI.invoke glean_wordrtsset_new

insertWordRtsSet :: Integral n => WordRtsSet -> n -> IO ()
insertWordRtsSet set elem =
  FFI.invoke $ glean_wordrtsset_insert set (fromIntegral elem)

insertBytesRtsSet :: Integral n => WordRtsSet -> Ptr Word8 -> n -> IO ()
insertBytesRtsSet set bytes size =
  FFI.invoke $ glean_wordrtsset_insert_bytes set bytes (fromIntegral size)

buildWordSet :: WordRtsSet -> Builder -> IO ()
buildWordSet set builder = FFI.invoke $ glean_wordrtsset_build set builder

buildWordSetBytes :: WordRtsSet -> Builder -> IO ()
buildWordSetBytes set builder =
  FFI.invoke $ glean_wordrtsset_build_bytes set builder

foreign import ccall unsafe glean_rtsset_new
  :: Ptr RtsSet -> IO CString

foreign import ccall unsafe glean_rtsset_insert
  :: RtsSet -> Builder -> IO CString

foreign import ccall unsafe glean_rtsset_build
  :: RtsSet -> Builder -> IO CString

foreign import ccall unsafe "glean_rtsset_free" freeRtsSet
  :: RtsSet -> IO ()

foreign import ccall unsafe glean_wordrtsset_new
  :: Ptr WordRtsSet -> IO CString

foreign import ccall unsafe glean_wordrtsset_insert
  :: WordRtsSet -> CULong -> IO CString

foreign import ccall unsafe glean_wordrtsset_insert_bytes
  :: WordRtsSet -> Ptr Word8 -> CSize -> IO CString

foreign import ccall unsafe glean_wordrtsset_build
  :: WordRtsSet -> Builder -> IO CString

foreign import ccall unsafe glean_wordrtsset_build_bytes
  :: WordRtsSet -> Builder -> IO CString

foreign import ccall unsafe "glean_wordrtsset_free" freeWordRtsSet
  :: WordRtsSet -> IO ()

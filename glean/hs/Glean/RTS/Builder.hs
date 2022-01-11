{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Builder
  ( Builder(..)
  , withBuilder
  , finishBuilder
  , newBuilder
  , sizeOfBuilder
  , freeBuilder
  , resetBuilder
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Foreign.C

import qualified Glean.FFI as FFI

newtype Builder = Builder (Ptr Builder)
  deriving(Storable)

withBuilder :: (MonadMask m, MonadIO m) => (Builder -> m a) -> m a
withBuilder = bracket (liftIO newBuilder) (liftIO . freeBuilder)

finishBuilder :: Builder -> IO ByteString
finishBuilder b = do
  (p,n) <- FFI.invoke $ glean_finish_builder b
  BS.unsafePackMallocCStringLen (castPtr p, fromIntegral n)

sizeOfBuilder :: Builder -> IO CSize
sizeOfBuilder = glean_builder_size

newBuilder :: IO Builder
newBuilder = FFI.invoke glean_new_builder

-- | release the current contents of the Builder and reinitialise it
-- to empty.  A bit cheaper than freeing and allocating a new Builder,
resetBuilder :: Builder -> IO ()
resetBuilder b = FFI.invoke $ glean_reset_builder b

foreign import ccall unsafe glean_new_builder
  :: Ptr Builder -> IO CString
foreign import ccall unsafe "glean_free_builder" freeBuilder
  :: Builder -> IO ()
foreign import ccall unsafe glean_builder_size
  :: Builder -> IO CSize
foreign import ccall unsafe glean_finish_builder
  :: Builder -> Ptr (Ptr Word8) -> Ptr CSize -> IO CString
foreign import ccall unsafe glean_reset_builder
  :: Builder -> IO ()

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Thrift
  ( encodeVarint, encodeZigZag
  )
where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Int (Int64)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

import qualified Util.Buffer as Buffer

encodeVarint :: Word64 -> Buffer.Fill s ()
encodeVarint !x = Buffer.alloc 10 $
  unsafeIOToST . fmap fromIntegral . glean_thrift_encode_varint x

encodeZigZag :: Int64 -> Buffer.Fill s ()
encodeZigZag !x = Buffer.alloc 10 $
  unsafeIOToST . fmap fromIntegral . glean_thrift_encode_zigzag x

foreign import ccall unsafe glean_thrift_encode_varint
  :: Word64 -> Ptr Word8 -> IO CSize
foreign import ccall unsafe glean_thrift_encode_zigzag
  :: Int64 -> Ptr Word8 -> IO CSize

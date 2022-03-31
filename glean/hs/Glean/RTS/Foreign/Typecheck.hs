{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Typecheck
  ( invokeTypechecker
  ) where

import Data.ByteString (ByteString)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Util.FFI

import Glean.FFI
import Glean.RTS.Foreign.Bytecode
import Glean.RTS.Typecheck (CompiledTypecheck)

-- | Invoke a typecheck subroutine on the given (encoded) type. Only used for
-- testing.
invokeTypechecker
  :: Subroutine CompiledTypecheck
  -> ByteString
  -> ByteString
invokeTypechecker sub val = unsafePerformIO $
  with sub $ \sub_ptr ->
  unsafeWithBytes val $ \val_ptr val_size -> do
    (res_ptr, res_size) <- invoke $ glean_invoke_typechecker
      sub_ptr
      val_ptr
      val_size
    unsafeMallocedByteString res_ptr res_size

foreign import ccall unsafe glean_invoke_typechecker
  :: Ptr (Subroutine CompiledTypecheck)
  -> Ptr ()
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO CString

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Bytecode
  ( Subroutine
  , SubroutineCode(..)
  , subroutine
  , inspect
  , size
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as V
import Data.Word (Word64)
import Foreign (withArray, withArrayLen)
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable (peekElemOff)
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

import Glean.FFI
import Util.FFI

data SubroutineCode = SubroutineCode
  { -- | Instructions
    subInsns :: !(V.Vector Word64)

    -- | Number of inputs
  , subInputs :: {-# UNPACK #-} !Word64

    -- | Number of outputs
  , subOutputs :: {-# UNPACK #-} !Word64

    -- | Number of local registers
  , subLocals :: {-# UNPACK #-} !Word64

    -- | Constants (copied into local registers at beginning)
  , subConstants :: !(V.Vector Word64)

    -- | Literals
  , subLiterals :: [ByteString]
  }

-- | A bytecode subroutine. The s parameter describes the kind of
-- subroutine this is (e.g. CompiledTypecheck, CompiledQuery), to
-- avoid us mixing up the two.
newtype Subroutine s = Subroutine (ForeignPtr (Subroutine s))

instance Object (Subroutine s) where
  wrap = Subroutine
  unwrap (Subroutine p) = p
  destroy = glean_subroutine_free

-- | Create a subroutine
subroutine
  :: V.Vector Word64 -- ^ instructions
  -> Word64 -- ^ number of inputs
  -> Word64 -- ^ number of outputs
  -> Word64 -- ^ number of local registers
  -> [Word64] -- ^ constants
  -> [ByteString] -- ^ literals
  -> IO (Subroutine s)
subroutine code inputs outputs locals consts lits =
  V.unsafeWith code $ \code_ptr ->
  withArrayLen consts $ \consts_len consts_ptr ->
  withMany (\s -> unsafeWithBytes s . curry) lits $ \ps ->
  let (lit_ptrs, lit_sizes) = unzip ps
  in
  withArray lit_ptrs $ \p_lit_ptrs ->
  withArray lit_sizes $ \p_lit_sizes ->
  construct $ invoke $ glean_subroutine_new
    code_ptr
    (fromIntegral $ V.length code)
    (fromIntegral inputs)
    (fromIntegral outputs)
    (fromIntegral locals)
    consts_ptr
    (fromIntegral consts_len)
    p_lit_ptrs
    p_lit_sizes
    (fromIntegral $ length ps)

size :: Subroutine s -> Int
size sub = unsafeDupablePerformIO $ with sub $ \p_sub -> do
  fromIntegral <$> glean_subroutine_size p_sub

inspect :: Subroutine s -> SubroutineCode
inspect sub = unsafePerformIO $ with sub $ \p_sub -> do
  ( insns_ptr, insns_size
    , inputs, outputs, locals
    , consts_ptr, consts_size
    , lit_count) <- invoke $ glean_subroutine_inspect p_sub
  insns <- V.generateM (fromIntegral insns_size) $ peekElemOff insns_ptr
  consts <- V.generateM (fromIntegral consts_size) $ peekElemOff consts_ptr
  lits <- forM [1 .. lit_count] $ \i -> do
    (lit_ptr, lit_size) <- invoke $ glean_subroutine_literal p_sub (i-1)
    copyByteString lit_ptr lit_size
  return SubroutineCode
    { subInsns = insns
    , subInputs = inputs
    , subOutputs = outputs
    , subLocals = locals
    , subConstants = consts
    , subLiterals = lits
    }

foreign import ccall unsafe glean_subroutine_new
  :: Ptr Word64
  -> CSize
  -> CSize
  -> CSize
  -> CSize
  -> Ptr Word64
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> CSize
  -> Ptr (Ptr (Subroutine s))
  -> IO CString
foreign import ccall unsafe "&glean_subroutine_free" glean_subroutine_free
  :: FunPtr (Ptr (Subroutine s) -> IO ())

foreign import ccall unsafe glean_subroutine_inspect
  :: Ptr (Subroutine s)
  -> Ptr (Ptr Word64)
  -> Ptr CSize
  -> Ptr Word64
  -> Ptr Word64
  -> Ptr Word64
  -> Ptr (Ptr Word64)
  -> Ptr CSize
  -> Ptr CSize
  -> IO ()

foreign import ccall unsafe glean_subroutine_size
  :: Ptr (Subroutine s)
  -> IO CSize

foreign import ccall unsafe glean_subroutine_literal
  :: Ptr (Subroutine s)
  -> CSize
  -> Ptr (Ptr ())
  -> Ptr CSize
  -> IO ()

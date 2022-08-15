{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Types
  ( Ty(..)
  , Register(..)
  , Label(..)
  , castRegister
  ) where

import Data.Coerce (coerce)
import Data.Word (Word64)

-- | Instruction argument types
data Ty
  = Word -- ^ 64 bit word
  | WordPtr -- ^ pointer to 64 bit word
  | Literal -- ^ index into the (string) literal table
  | Offset -- ^ jump offset (relative to start of next instruction)
  | Offsets -- ^ array of jump offsets (length + array in the insn stream)
  | DataPtr -- ^ a void pointer
  | BinaryOutputPtr -- ^ pointer to binary::Output (temporary, will be removed)
  | Fun [Ty] -- ^ pointer to syscall (temporary, will be removed)
  | Regs [Ty] -- ^ list of registers (array without length in the insn stream)
  deriving(Eq, Show)

-- | Typed registers
newtype Register (ty :: Ty) = Register { fromRegister :: Word64 }
  deriving(Eq,Ord,Enum,Show)

castRegister :: Register a -> Register b
castRegister = coerce

-- | Labels
newtype Label = Label { fromLabel :: Int }
  deriving(Eq,Ord,Enum,Show)

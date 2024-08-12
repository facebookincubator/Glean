{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Types
  ( Ty(..)
  , Ordered
  , Addable
  , Register(..)
  , Label(..)
  , Literal(..)
  , castRegister
  ) where

import Data.Coerce (coerce)
import Data.Word (Word64)

-- | Instruction argument types
data Ty
  = Word -- ^ 64 bit word
  | WordPtr -- ^ pointer to 64 bit word
  | Lit -- ^ index into the (string) literal table
  | Offset -- ^ jump offset (relative to start of next instruction)
  | DataPtr -- ^ a void pointer
  | BinaryOutputPtr -- ^ pointer to binary::Output (temporary, will be removed)
  | SetPtr  -- ^  pointer to a set representation
  | Fun [Ty] -- ^ pointer to syscall (temporary, will be removed)
  deriving(Eq, Show)

-- | Types which can be compared
class Ordered (t :: Ty)
instance Ordered 'Word
instance Ordered 'DataPtr

-- | Types which can be added
class Addable (t :: Ty) (u :: Ty)
instance Addable 'Word 'Word
instance Addable 'DataPtr 'Word

-- | Typed registers
newtype Register (ty :: Ty) = Register { fromRegister :: Word64 }
  deriving(Eq,Ord,Enum,Show)

castRegister :: Register a -> Register b
castRegister = coerce

-- | Labels
newtype Label = Label { fromLabel :: Int }
  deriving(Eq,Ord,Enum,Show)

newtype Literal = Literal { fromLiteral :: Word64 }
  deriving(Eq,Ord,Show)

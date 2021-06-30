-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Bytecode.MonadInsn (MonadInsn(..))
where

import Data.ByteString (ByteString)
import Data.Word (Word64)

import Glean.Bytecode.Types
import Glean.RTS.Bytecode.Gen.Instruction (Insn)

-- | Monad which can issue bytecode instructions
class Monad m => MonadInsn m where
  -- | Label type
  type Lbl m

  -- | Register type
  type Reg m :: Ty -> *

  -- | Add a literal to the literal table and yield its index
  literal :: ByteString -> m Word64

  -- | Yield a label for the current position in the code
  label :: m (Lbl m)

  -- | Issue an instruction
  issue :: Insn (Reg m) (Lbl m) -> m ()

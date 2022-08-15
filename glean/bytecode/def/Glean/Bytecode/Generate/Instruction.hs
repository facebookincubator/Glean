{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Generate.Instruction
  ( Insn(..)
  , Effect(..)
  , Arg(..)
  , Usage(..)
  , instructions
  , version
  , lowestSupportedVersion
  ) where

import Data.Text (Text)

import Glean.Bytecode.Types

-- This is an instruction set for a really simple register machine. The
-- instructions themselves haven't been designed in any sense, we just added
-- whatever seemed necessary to implement typechecking via bytecode. The
-- instruction stream is a sequence of 64 bit words, with each instruction
-- having one word for the opcode and one word per argument. We will revisit
-- both the instruction set and the representation once this is actually used.

-- | Definition of a bytecode instruction.
data Insn = Insn
  { insnName :: Text
  , insnEffects :: [Effect]
  , insnArgs :: [Arg]
  }

-- | Effects of an instruction
data Effect
  = EndBlock  -- ^ end the current block (typically for jumps)
  | Return    -- ^ return from the evaluator
  deriving(Eq, Show)

-- | Instruction argument.
data Arg = Arg
  { argName :: Text
  , argTy :: Ty
  , argUsage :: Usage
  }

-- | How an argument is provided/used
data Usage
  = Imm -- ^ immediate value in the instruction stream
  | Load -- ^ register, read-only
  | Store -- ^ register, write-only
  | Update -- ^ register, read and write
  deriving(Eq)

-- | Current bytecode version
--
-- BUMP THIS WHENEVER YOU CHANGE THE BYTECODE EVEN IF YOU JUST ADD INSTRUCTIONS
version :: Int
version = 7

-- | Lowest bytecode version supported by the current engine.
--
-- SET THIS TO THE SAME VALUE AS 'version' UNLESS YOU ONLY ADD NEW INSTRUCTIONS
-- TO THE END OF THE LIST (in which case the new engine can still execute
-- old bytecode)
lowestSupportedVersion :: Int
lowestSupportedVersion = 7

-- | Definitions of all bytecode instructions
instructions :: [Insn]
instructions =
  [
    -- Decode a Nat from memory into a register.
    Insn "InputNat" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "dst" Word Store ]

    -- Advance begin by size bytes, and bounds-check against end
  , Insn "InputBytes" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "size" Word Load ]

    -- Validate and skip over an encoded UTF8 string
  , Insn "InputSkipUntrustedString" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Check that the input starts with the given literal, and then skip past it
  , Insn "InputShiftLit" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "lit" Literal Imm
      , Arg "match" Word Store ]

    -- Check that the input starts with the given byte sequence, and
    -- then skip past it
  , Insn "InputShiftBytes" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "ptr" DataPtr Load
      , Arg "ptrend" DataPtr Load
      , Arg "match" Word Store ]

    -- Decode a Nat from memory
  , Insn "InputSkipNat" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Skip over an encoded UTF8 string in a binary::Input. The string must be
    -- valid (this is not checked).
  , Insn "InputSkipTrustedString" []
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Reset a binary::Output
  , Insn "ResetOutput" []
      [ Arg "output" BinaryOutputPtr Load ]

    -- Encode a Nat in a register and store it in a binary::Output.
  , Insn "OutputNat" []
      [ Arg "src" Word Load
      , Arg "output" BinaryOutputPtr Load ]

    -- Encode an immediate Nat and store it in a binary::Output.
  , Insn "OutputNatImm" []
      [ Arg "src" Word Imm
      , Arg "output" BinaryOutputPtr Load ]

    -- Encode a byte in a binary::Output
  , Insn "OutputByteImm" []
      [ Arg "src" Word Imm
      , Arg "output" BinaryOutputPtr Load ]

    -- Write a sequence of bytes to a binary::Output.
  , Insn "OutputBytes" []
      [ Arg "ptr" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "output" BinaryOutputPtr Load ]

    -- String toLower
  , Insn "OutputStringToLower" []
      [ Arg "begin" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "dst" BinaryOutputPtr Load ]

    -- converts [RelByteSpan] to [ByteSpan]
  , Insn "OutputRelToAbsByteSpans" []
      [ Arg "begin" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "dst" BinaryOutputPtr Load ]

    -- Get the contents of a binary::Output as a pointer and
    -- length. Note that these are only valid until the next operation
    -- on the binary::Output
  , Insn "GetOutput" []
      [ Arg "output" BinaryOutputPtr Load
      , Arg "ptr" DataPtr Store
      , Arg "end" DataPtr Store ]

    -- Get the number of bytes in the output
  , Insn "GetOutputSize" []
      [ Arg "output" BinaryOutputPtr Load
      , Arg "dst" Word Store ]

    -- Write a constant into a register.
  , Insn "LoadConst" []
      [ Arg "imm" Word Imm
      , Arg "dst" Word Store ]

    -- Load the address and size of a literal
  , Insn "LoadLiteral" []
      [ Arg "lit" Literal Imm
      , Arg "ptr" DataPtr Store
      , Arg "end" DataPtr Store ]

    -- Copy a register into another one.
  , Insn "LoadReg" []
      [ Arg "src" Word Load
      , Arg "dst" Word Store ]

    -- Subtract a constant from a register.
  , Insn "SubConst" []
      [ Arg "imm" Word Imm
      , Arg "dst" Word Update ]

    -- Subtract a register from a register.
  , Insn "Sub" []
      [ Arg "src" Word Load
      , Arg "dst" Word Update ]

    -- Add a constant to a register.
  , Insn "AddConst" []
      [ Arg "imm" Word Imm
      , Arg "dst" Word Update ]

    -- Add a register to another register
  , Insn "Add" []
      [ Arg "src" Word Load
      , Arg "dst" Word Update ]

    -- Subtract pointers
  , Insn "PtrDiff" []
      [ Arg "src1" DataPtr Load
      , Arg "src2" DataPtr Load
      , Arg "dst" Word Store ]

  , Insn "LoadLabel" []
      [ Arg "lbl" Offset Imm
      , Arg "dst" Offset Store ]

    -- Unconditional jump.
  , Insn "Jump" [EndBlock]
      [ Arg "tgt" Offset Imm ]

  , Insn "JumpReg" [EndBlock]
      [ Arg "tgt" Offset Load ]

    -- Jump if a register is 0.
  , Insn "JumpIf0" []
      [ Arg "reg" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a register is not 0.
  , Insn "JumpIfNot0" []
      [ Arg "reg" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if two registers are equal.
  , Insn "JumpIfEq" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if two registers are not equal.
  , Insn "JumpIfNe" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a > b.
  , Insn "JumpIfGt" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a >= b.
  , Insn "JumpIfGe" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a < b.
  , Insn "JumpIfLt" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a <= b.
  , Insn "JumpIfLe" []
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Decrement the value in a register and jump if it isn't 0.
  , Insn "DecrAndJumpIfNot0" []
      [ Arg "reg" Word Update
      , Arg "tgt" Offset Imm ]

    -- Decrement the value in a register and jump if it is 0.
  , Insn "DecrAndJumpIf0" []
      [ Arg "reg" Word Update
      , Arg "tgt" Offset Imm ]

  , Insn "CallFun_0_1" []
      [ Arg "fun" (Fun [WordPtr]) Load
      , Arg "args" (Regs [Word]) Update ]

  , Insn "CallFun_0_2" []
      [ Arg "fun" (Fun [WordPtr, WordPtr]) Load
      , Arg "args" (Regs [Word,Word]) Update ]

  , Insn "CallFun_1_1" []
      [ Arg "fun" (Fun [Word, WordPtr]) Load
      , Arg "args" (Regs [Word,Word]) Update ]

  , Insn "CallFun_1_0" []
      [ Arg "fun" (Fun [Word]) Load
      , Arg "args" (Regs [Word]) Update ]

    -- Call an std::function which takes two 64-bit arguments and returns
    -- one 64-bit result.
  , Insn "CallFun_2_1" []
      [ Arg "fun" (Fun [Word,Word,WordPtr]) Load
      , Arg "args" (Regs [Word,Word,Word]) Update ]

  , Insn "CallFun_2_0" []
      [ Arg "fun" (Fun [Word,Word]) Load
      , Arg "args" (Regs [Word,Word]) Update ]

  , Insn "CallFun_3_0" []
      [ Arg "fun" (Fun [Word,Word,Word]) Load
      , Arg "args" (Regs [Word,Word,Word]) Update ]

  , Insn "CallFun_4_0" []
      [ Arg "fun" (Fun [Word,Word,Word,Word]) Load
      , Arg "args" (Regs [Word,Word,Word,Word]) Update ]

  , Insn "CallFun_3_1" []
      [ Arg "fun" (Fun [Word,Word,Word,WordPtr]) Load
      , Arg "args" (Regs [Word,Word,Word,Word]) Update ]

  , Insn "CallFun_5_0" []
      [ Arg "fun" (Fun [Word,Word,Word,Word,Word]) Load
      , Arg "args" (Regs [Word,Word,Word,Word,Word]) Update ]

  , Insn "CallFun_5_1" []
      [ Arg "fun" (Fun [Word,Word,Word,Word,Word,WordPtr]) Load
      , Arg "args" (Regs [Word,Word,Word,Word,Word,Word]) Update ]

  , Insn "CallFun_2_2" []
      [ Arg "fun" (Fun [Word,Word,WordPtr,WordPtr]) Load
      , Arg "args" (Regs [Word,Word,Word,Word]) Update ]

  , Insn "CallFun_2_5" []
      [ Arg "fun"
          (Fun [Word,Word,WordPtr,WordPtr,WordPtr,WordPtr,WordPtr])
          Load
      , Arg "args" (Regs [Word,Word,Word,Word,Word,Word,Word]) Update ]

    -- Indexed jump - the register contains an index into the array of
    -- offsets. Does nothing if the index is out of range.
  , Insn "Select" []
      [ Arg "sel" Word Load
      , Arg "tgts" Offsets Imm ]

    -- Raise an exception.
  , Insn "Raise" [EndBlock]
      [ Arg "msg" Literal Imm ]

    -- For debugging
  , Insn "Trace" []
      [ Arg "msg" Literal Imm ]

  , Insn "TraceReg" []
      [ Arg "msg" Literal Imm
      , Arg "reg" Word Load ]

    -- Adjust PC to point to 'cont' and suspend execution. The first argument
    -- is a temporary, unused left-over for backwards compatibility.
  , Insn "Suspend" [EndBlock, Return]
      [ Arg "unused" Word Load
      , Arg "cont" Offset Imm
      ]

    -- Return from a subroutine.
  , Insn "Ret" [EndBlock, Return] []

    -- Load a word from a memory location pointed to by a register
  , Insn "LoadWord" []
      [ Arg "src" WordPtr Load
      , Arg "dst" Word Store
      ]

    -- Store a word into a memory location pointed to by a register
  , Insn "StoreWord" []
      [ Arg "src" Word Load
      , Arg "dst" WordPtr Load
      ]
  ]

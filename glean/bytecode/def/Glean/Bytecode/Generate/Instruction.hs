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
  , ArgTy(..)
  , ImmTy(..)
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
  , insnContext :: [Text]
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
  , argTy :: ArgTy
  }

-- | Argument type
data ArgTy
  = Imm ImmTy -- ^ immediate value in instruction stream
  | Reg (Maybe Text) Ty Usage -- ^ register argument
  | Offsets -- ^ array of jump offsets (length + array in the insn stream)
  | Regs [Ty] -- ^ list of registers (array without length in the insn stream)

-- | Type of an immediate value
data ImmTy
  = ImmWord
  | ImmOffset
  | ImmLit
  deriving(Eq,Ord,Show)

-- | Register argument of a particular type
reg :: Ty -> Usage -> ArgTy
reg = Reg Nothing

-- | Polymorphic register argument - `Text` is the type variable and `Ty` the
-- type actually stored in the instruction (since we don't really need
-- existentials there)
polyReg :: Text -> Ty -> Usage -> ArgTy
polyReg = Reg . Just

-- | How an register is used
data Usage
  = Load -- ^ read-only
  | Store -- ^ write-only
  | Update -- ^ read and write
  deriving(Eq)

-- | Current bytecode version
--
-- BUMP THIS WHENEVER YOU CHANGE THE BYTECODE EVEN IF YOU JUST ADD INSTRUCTIONS
version :: Int
version = 8

-- | Lowest bytecode version supported by the current engine.
--
-- SET THIS TO THE SAME VALUE AS 'version' UNLESS YOU ONLY ADD NEW INSTRUCTIONS
-- TO THE END OF THE LIST (in which case the new engine can still execute
-- old bytecode)
lowestSupportedVersion :: Int
lowestSupportedVersion = 8

-- | Definitions of all bytecode instructions
instructions :: [Insn]
instructions =
  [
    -- Decode a Nat from memory into a register.
    Insn "InputNat" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load
      , Arg "dst" $ reg Word Store ]

    -- Advance begin by size bytes, and bounds-check against end
  , Insn "InputBytes" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load
      , Arg "size" $ reg Word Load ]

    -- Validate and skip over an encoded UTF8 string
  , Insn "InputSkipUntrustedString" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load ]

    -- Check that the input starts with the given literal, and then skip past it
  , Insn "InputShiftLit" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load
      , Arg "lit" $ Imm ImmLit
      , Arg "match" $ reg Word Store ]

    -- Check that the input starts with the given byte sequence, and
    -- then skip past it
  , Insn "InputShiftBytes" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load
      , Arg "ptr" $ reg DataPtr Load
      , Arg "ptrend" $ reg DataPtr Load
      , Arg "match" $ reg Word Store ]

    -- Decode a Nat from memory
  , Insn "InputSkipNat" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load ]

    -- Skip over an encoded UTF8 string in a binary::Input. The string must be
    -- valid (this is not checked).
  , Insn "InputSkipTrustedString" [] []
      [ Arg "begin" $ reg DataPtr Update
      , Arg "end" $ reg DataPtr Load ]

    -- Reset a binary::Output
  , Insn "ResetOutput" [] []
      [ Arg "output" $ reg BinaryOutputPtr Load ]

    -- Encode a Nat in a register and store it in a binary::Output.
  , Insn "OutputNat" [] []
      [ Arg "src" $ reg Word Load
      , Arg "output" $ reg BinaryOutputPtr Load ]

    -- Encode an immediate Nat and store it in a binary::Output.
  , Insn "OutputNatImm" [] []
      [ Arg "src" $ Imm ImmWord
      , Arg "output" $ reg BinaryOutputPtr Load ]

    -- Encode a byte in a binary::Output
  , Insn "OutputByteImm" [] []
      [ Arg "src" $ Imm ImmWord
      , Arg "output" $ reg BinaryOutputPtr Load ]

    -- Write a sequence of bytes to a binary::Output.
  , Insn "OutputBytes" [] []
      [ Arg "ptr" $ reg DataPtr Load
      , Arg "end" $ reg DataPtr Load
      , Arg "output" $ reg BinaryOutputPtr Load ]

    -- String toLower
  , Insn "OutputStringToLower" [] []
      [ Arg "begin" $ reg DataPtr Load
      , Arg "end" $ reg DataPtr Load
      , Arg "dst" $ reg BinaryOutputPtr Load ]

    -- converts [RelByteSpan] to [ByteSpan]
  , Insn "OutputRelToAbsByteSpans" [] []
      [ Arg "begin" $ reg DataPtr Load
      , Arg "end" $ reg DataPtr Load
      , Arg "dst" $ reg BinaryOutputPtr Load ]

    -- Get the contents of a binary::Output as a pointer and
    -- length. Note that these are only valid until the next operation
    -- on the binary::Output
  , Insn "GetOutput" [] []
      [ Arg "output" $ reg BinaryOutputPtr Load
      , Arg "ptr" $ reg DataPtr Store
      , Arg "end" $ reg DataPtr Store ]

    -- Get the number of bytes in the output
  , Insn "GetOutputSize" [] []
      [ Arg "output" $ reg BinaryOutputPtr Load
      , Arg "dst" $ reg Word Store ]

    -- Write a constant into a register.
  , Insn "LoadConst" [] []
      [ Arg "imm" $ Imm ImmWord
      , Arg "dst" $ reg Word Store ]

    -- Load the address and size of a literal
  , Insn "LoadLiteral" [] []
      [ Arg "lit" $ Imm ImmLit
      , Arg "ptr" $ reg DataPtr Store
      , Arg "end" $ reg DataPtr Store ]

    -- Copy a register into another one.
  , Insn "Move" [] []
      [ Arg "src" $ polyReg "a" Word Load
      , Arg "dst" $ polyReg "a" Word Store ]

    -- Subtract a constant from a register.
  , Insn "SubConst" [] []
      [ Arg "imm" $ Imm ImmWord
      , Arg "dst" $ reg Word Update ]

    -- Subtract a register from a register.
  , Insn "Sub" [] ["Addable a b"]
      [ Arg "src" $ polyReg "b" Word Load
      , Arg "dst" $ polyReg "a" Word Update ]

    -- Add a constant to a register.
  , Insn "AddConst" [] ["Addable a 'Word"]
      [ Arg "imm" $ Imm ImmWord
      , Arg "dst" $ polyReg "a" Word Update ]

    -- Add a register to another register
  , Insn "Add" [] ["Addable a b"]
      [ Arg "src" $ polyReg "b" Word Load
      , Arg "dst" $ polyReg "a" Word Update ]

    -- Subtract pointers
  , Insn "PtrDiff" [] []
      [ Arg "src1" $ reg DataPtr Load
      , Arg "src2" $ reg DataPtr Load
      , Arg "dst" $ reg Word Store ]

  , Insn "LoadLabel" [] []
      [ Arg "lbl" $ Imm ImmOffset
      , Arg "dst" $ reg Offset Store ]

    -- Unconditional jump.
  , Insn "Jump" [EndBlock] []
      [ Arg "tgt" $ Imm ImmOffset ]

  , Insn "JumpReg" [EndBlock] []
      [ Arg "tgt" $ reg Offset Load ]

    -- Jump if a register is 0.
  , Insn "JumpIf0" [] []
      [ Arg "reg" $ reg Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if a register is not 0.
  , Insn "JumpIfNot0" [] []
      [ Arg "reg" $ reg Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if two registers are equal.
  , Insn "JumpIfEq" [] []
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if two registers are not equal.
  , Insn "JumpIfNe" [] []
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if a > b.
  , Insn "JumpIfGt" [] ["Ordered a"]
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if a >= b.
  , Insn "JumpIfGe" [] ["Ordered a"]
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if a < b.
  , Insn "JumpIfLt" [] ["Ordered a"]
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Jump if a <= b.
  , Insn "JumpIfLe" [] ["Ordered a"]
      [ Arg "reg1" $ polyReg "a" Word Load
      , Arg "reg2" $ polyReg "a" Word Load
      , Arg "tgt" $ Imm ImmOffset ]

    -- Decrement the value in a register and jump if it isn't 0.
  , Insn "DecrAndJumpIfNot0" [] []
      [ Arg "reg" $ reg Word Update
      , Arg "tgt" $ Imm ImmOffset ]

    -- Decrement the value in a register and jump if it is 0.
  , Insn "DecrAndJumpIf0" [] []
      [ Arg "reg" $ reg Word Update
      , Arg "tgt" $ Imm ImmOffset ]

  , Insn "CallFun_0_1" [] []
      [ Arg "fun" $ reg (Fun [WordPtr]) Load
      , Arg "args" $ Regs [Word] ]

  , Insn "CallFun_0_2" [] []
      [ Arg "fun" $ reg (Fun [WordPtr, WordPtr]) Load
      , Arg "args" $ Regs [Word,Word] ]

  , Insn "CallFun_1_1" [] []
      [ Arg "fun" $ reg (Fun [Word, WordPtr]) Load
      , Arg "args" $ Regs [Word,Word] ]

  , Insn "CallFun_1_0" [] []
      [ Arg "fun" $ reg (Fun [Word]) Load
      , Arg "args" $ Regs [Word] ]

    -- Call an std::function which takes two 64-bit arguments and returns
    -- one 64-bit result.
  , Insn "CallFun_2_1" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,WordPtr]) Load
      , Arg "args" $ Regs [Word,Word,Word] ]

  , Insn "CallFun_2_0" [] []
      [ Arg "fun" $ reg (Fun [Word,Word]) Load
      , Arg "args" $ Regs [Word,Word] ]

  , Insn "CallFun_3_0" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,Word]) Load
      , Arg "args" $ Regs [Word,Word,Word] ]

  , Insn "CallFun_4_0" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,Word,Word]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word] ]

  , Insn "CallFun_3_1" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,Word,WordPtr]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word] ]

  , Insn "CallFun_5_0" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,Word,Word,Word]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word,Word] ]

  , Insn "CallFun_5_1" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,Word,Word,Word,WordPtr]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word,Word,Word] ]

  , Insn "CallFun_2_2" [] []
      [ Arg "fun" $ reg (Fun [Word,Word,WordPtr,WordPtr]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word] ]

  , Insn "CallFun_2_5" [] []
      [ Arg "fun"
          $ reg (Fun [Word,Word,WordPtr,WordPtr,WordPtr,WordPtr,WordPtr]) Load
      , Arg "args" $ Regs [Word,Word,Word,Word,Word,Word,Word] ]

    -- Indexed jump - the register contains an index into the array of
    -- offsets. Does nothing if the index is out of range.
  , Insn "Select" [] []
      [ Arg "sel" $ reg Word Load
      , Arg "tgts" Offsets ]

    -- Raise an exception.
  , Insn "Raise" [EndBlock] []
      [ Arg "msg" $ Imm ImmLit ]

    -- For debugging
  , Insn "Trace" [] []
      [ Arg "msg" $ Imm ImmLit ]

  , Insn "TraceReg" [] []
      [ Arg "msg" $ Imm ImmLit
      , Arg "reg" $ reg Word Load ]

    -- Adjust PC to point to 'cont' and suspend execution. The first argument
    -- is a temporary, unused left-over for backwards compatibility.
  , Insn "Suspend" [EndBlock, Return] []
      [ Arg "unused" $ reg Word Load
      , Arg "cont" $ Imm ImmOffset
      ]

    -- Return from a subroutine.
  , Insn "Ret" [EndBlock, Return] [] []

  ]

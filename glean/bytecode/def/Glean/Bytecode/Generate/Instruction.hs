module Glean.Bytecode.Generate.Instruction
  ( Insn(..)
  , Arg(..), Usage(..)
  , instructions
  , returns
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
  , insnControl :: Control
  , insnArgs :: [Arg]
  }

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
version = 4

-- | Lowest bytecode version supported by the current engine.
--
-- SET THIS TO THE SAME VALUE AS 'version' UNLESS YOU ONLY ADD NEW INSTRUCTIONS
-- TO THE END OF THE LIST (in which case the new engine can still execute
-- old bytecode)
lowestSupportedVersion :: Int
lowestSupportedVersion = 4

-- | Definitions of all bytecode instructions
instructions :: [Insn]
instructions =
  [
    -- Decode a Nat from memory into a register.
    Insn "InputNat" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "dst" Word Store ]

    -- Advance begin by size bytes, and bounds-check against end
  , Insn "InputBytes" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "size" Word Load ]

    -- Validate and skip over an encoded UTF8 string
  , Insn "InputSkipUntrustedString" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Check that the input starts with the given literal, and then skip past it
  , Insn "InputShiftLit" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "lit" Literal Imm
      , Arg "match" Word Store ]

    -- Check that the input starts with the given byte sequence, and
    -- then skip past it
  , Insn "InputShiftBytes" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load
      , Arg "ptr" DataPtr Load
      , Arg "ptrend" DataPtr Load
      , Arg "match" Word Store ]

    -- Decode a Nat from memory
  , Insn "InputSkipNat" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Skip over an encoded UTF8 string in a binary::Input. The string must be
    -- valid (this is not checked).
  , Insn "InputSkipTrustedString" FallThrough
      [ Arg "begin" DataPtr Update
      , Arg "end" DataPtr Load ]

    -- Reset a binary::Output
  , Insn "ResetOutput" FallThrough
      [ Arg "output" BinaryOutputPtr Load ]

    -- Encode a Nat in a register and store it in a binary::Output.
  , Insn "OutputNat" FallThrough
      [ Arg "src" Word Load
      , Arg "output" BinaryOutputPtr Load ]

    -- Encode an immediate Nat and store it in a binary::Output.
  , Insn "OutputNatImm" FallThrough
      [ Arg "src" Word Imm
      , Arg "output" BinaryOutputPtr Load ]

    -- Encode a byte in a binary::Output
  , Insn "OutputByteImm" FallThrough
      [ Arg "src" Word Imm
      , Arg "output" BinaryOutputPtr Load ]

    -- Write a sequence of bytes to a binary::Output.
  , Insn "OutputBytes" FallThrough
      [ Arg "ptr" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "output" BinaryOutputPtr Load ]

    -- String toLower
  , Insn "OutputStringToLower" FallThrough
      [ Arg "begin" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "dst" BinaryOutputPtr Load ]

    -- converts [RelByteSpan] to [ByteSpan]
  , Insn "OutputRelToAbsByteSpans" FallThrough
      [ Arg "begin" DataPtr Load
      , Arg "end" DataPtr Load
      , Arg "dst" BinaryOutputPtr Load ]

    -- Get the contents of a binary::Output as a pointer and
    -- length. Note that these are only valid until the next operation
    -- on the binary::Output
  , Insn "GetOutput" FallThrough
      [ Arg "output" BinaryOutputPtr Load
      , Arg "ptr" DataPtr Store
      , Arg "end" DataPtr Store ]

    -- Get the number of bytes in the output
  , Insn "GetOutputSize" FallThrough
      [ Arg "output" BinaryOutputPtr Load
      , Arg "dst" Word Store ]

    -- Write a constant into a register.
  , Insn "LoadConst" FallThrough
      [ Arg "imm" Word Imm
      , Arg "dst" Word Store ]

    -- Load the address and size of a literal
  , Insn "LoadLiteral" FallThrough
      [ Arg "lit" Literal Imm
      , Arg "ptr" DataPtr Store
      , Arg "end" DataPtr Store ]

    -- Copy a register into another one.
  , Insn "LoadReg" FallThrough
      [ Arg "src" Word Load
      , Arg "dst" Word Store ]

    -- Subtract a constant from a register.
  , Insn "SubConst" FallThrough
      [ Arg "imm" Word Imm
      , Arg "dst" Word Update ]

    -- Subtract a register from a register.
  , Insn "Sub" FallThrough
      [ Arg "src" Word Load
      , Arg "dst" Word Update ]

    -- Add a constant to a register.
  , Insn "AddConst" FallThrough
      [ Arg "imm" Word Imm
      , Arg "dst" Word Update ]

    -- Add a register to another register
  , Insn "Add" FallThrough
      [ Arg "src" Word Load
      , Arg "dst" Word Update ]

    -- Subtract pointers
  , Insn "PtrDiff" FallThrough
      [ Arg "src1" DataPtr Load
      , Arg "src2" DataPtr Load
      , Arg "dst" Word Store ]

  , Insn "LoadLabel" FallThrough
      [ Arg "lbl" Offset Imm
      , Arg "dst" Offset Store ]

    -- Unconditional jump.
  , Insn "Jump" UncondJump
      [ Arg "tgt" Offset Imm ]

  , Insn "JumpReg" UncondJump
      [ Arg "tgt" Offset Load ]

    -- Jump if a register is 0.
  , Insn "JumpIf0" CondJump
      [ Arg "reg" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a register is not 0.
  , Insn "JumpIfNot0" CondJump
      [ Arg "reg" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if two registers are equal.
  , Insn "JumpIfEq" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if two registers are not equal.
  , Insn "JumpIfNe" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a > b.
  , Insn "JumpIfGt" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a >= b.
  , Insn "JumpIfGe" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a < b.
  , Insn "JumpIfLt" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Jump if a <= b.
  , Insn "JumpIfLe" CondJump
      [ Arg "reg1" Word Load
      , Arg "reg2" Word Load
      , Arg "tgt" Offset Imm ]

    -- Decrement the value in a register and jump if it isn't 0.
  , Insn "DecrAndJumpIfNot0" CondJump
      [ Arg "reg" Word Update
      , Arg "tgt" Offset Imm ]

    -- Decrement the value in a register and jump if it is 0.
  , Insn "DecrAndJumpIf0" CondJump
      [ Arg "reg" Word Update
      , Arg "tgt" Offset Imm ]

  , Insn "CallFun_0_1" FallThrough
      [ Arg "fun" (Fun [] Word) Load
      , Arg "dst" Word Store ]

  , Insn "CallFun_0_2" FallThrough
      [ Arg "fun" (Fun [WordPtr] Word) Load
      , Arg "dst1" Word Store
      , Arg "dst2" Word Store ]

  , Insn "CallFun_1_1" FallThrough
      [ Arg "fun" (Fun [Word] Word) Load
      , Arg "src" Word Load
      , Arg "dst" Word Store ]

  , Insn "CallFun_1_0" FallThrough
      [ Arg "fun" (Fun [Word] Void) Load
      , Arg "src" Word Load ]

    -- Call an std::function which takes two 64-bit arguments and returns
    -- one 64-bit result.
  , Insn "CallFun_2_1" FallThrough
      [ Arg "fun" (Fun [Word,Word] Word) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "dst" Word Store ]

  , Insn "CallFun_2_0" FallThrough
      [ Arg "fun" (Fun [Word,Word] Void) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load ]

  , Insn "CallFun_3_0" FallThrough
      [ Arg "fun" (Fun [Word,Word,Word] Void) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "src3" Word Load ]

  , Insn "CallFun_4_0" FallThrough
      [ Arg "fun" (Fun [Word,Word,Word,Word] Void) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "src3" Word Load
      , Arg "src4" Word Load ]

  , Insn "CallFun_3_1" FallThrough
      [ Arg "fun" (Fun [Word,Word,Word] Word) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "src3" Word Load
      , Arg "dst" Word Store ]

  , Insn "CallFun_5_0" FallThrough
      [ Arg "fun" (Fun [Word,Word,Word,Word,Word] Word) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "src3" Word Load
      , Arg "src4" Word Load
      , Arg "src5" Word Load ]

  , Insn "CallFun_2_2" FallThrough
      [ Arg "fun" (Fun [Word,Word,DataPtr] Word) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "dst1" Word Store
      , Arg "dst2" Word Store ]

  , Insn "CallFun_2_5" FallThrough
      [ Arg "fun" (Fun [Word,Word,DataPtr,DataPtr,DataPtr,DataPtr] Word) Load
      , Arg "src1" Word Load
      , Arg "src2" Word Load
      , Arg "dst1" Word Store
      , Arg "dst2" Word Store
      , Arg "dst3" Word Store
      , Arg "dst4" Word Store
      , Arg "dst5" Word Store ]

    -- Indexed jump - the register contains an index into the array of
    -- offsets. Does nothing if the index is out of range.
  , Insn "Select" CondJump
      [ Arg "sel" Word Load
      , Arg "tgts" Offsets Imm ]

    -- Raise an exception.
  , Insn "Raise" UncondJump
      [ Arg "msg" Literal Imm ]

    -- For debugging
  , Insn "Trace" FallThrough
      [ Arg "msg" Literal Imm ]

  , Insn "TraceReg" FallThrough
      [ Arg "msg" Literal Imm
      , Arg "reg" Word Load ]

    -- Adjust PC to point to 'cont', invoke an external function that will be
    -- passed
    --    (uint64_t* pc, uint64_t* frame)
    -- i.e. the current state of the evaluator and then return from the
    -- subroutine. The intention is to resume execution later, starting from
    -- 'cont'.
  , Insn "Suspend" UncondJump
      [ Arg "fun" (Fun [WordPtr,WordPtr] Void) Load
      , Arg "cont" Offset Imm
      ]

    -- Return from a subroutine.
  , Insn "Ret" UncondJump []

    -- Load a word from a memory location pointed to by a register
  , Insn "LoadWord" FallThrough
      [ Arg "src" WordPtr Load
      , Arg "dst" Word Store
      ]

    -- Store a word into a memory location pointed to by a register
  , Insn "StoreWord" FallThrough
      [ Arg "src" Word Load
      , Arg "dst" WordPtr Load
      ]
  ]

returns :: Insn -> Bool
returns (Insn "Ret" _ _) = True
returns (Insn "Suspend" _ _) = True
returns _ = False

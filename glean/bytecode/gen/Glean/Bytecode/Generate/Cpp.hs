{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Generate.Cpp (main)
where

import Data.List (intercalate, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment
import System.Exit (die)
import System.FilePath

import Glean.Bytecode.Generate.Instruction
import Glean.Bytecode.Types

-- We generate 3 files:
--
-- instruction.h has the enum with all opcodes
--
-- evaluator.h defines functions for decoding instructions and two evaluators -
-- one based on switch and the other token-threaded. It is intended to be
-- included as part of the definition of an evaluator class.

indent :: Text -> Text
indent x = "  " <> x

main :: IO ()
main = do
  args <- getArgs
  dir <- case args of
    [arg] | Just dir <- stripPrefix "--install_dir=" arg ->
      return $ dir </> "bytecode/gen"
    _ -> die "invalid arguments"
  createDirectoryIfMissing True dir
  genHeader (dir </> "instruction.h")
    ["#include <cstdint>"]
    genOpEnum
  genFile (dir </> "evaluate.h")
    genEvaluator

-- | Generate a file.
genFile :: FilePath -> [Text] -> IO ()
genFile path ls = Text.writeFile path
  $ Text.unlines
  $ ("// @" <> "generated") : "" : ls

-- | Generate a header file (with #pragma once and namespaces).
genHeader :: FilePath -> [Text] -> [Text] -> IO ()
genHeader path pre ls = genFile path $
  [ "#pragma once"
  , "" ]
  ++ pre ++
  [ ""
  , "namespace facebook {"
  , "namespace glean {"
  , "namespace rts {"
  , "" ]
  ++ ls ++
  [ ""
  , "}"
  , "}"
  , "}" ]

unusedOps :: [Text]
unusedOps = ["Unused" <> Text.pack (show n) | n <- [length instructions .. 255]]

-- | Generate the enum with all opcodes.
genOpEnum :: [Text]
genOpEnum =
  "enum class Op : uint8_t {"
  : [indent $ insnName insn <> "," | insn <- instructions]
  ++ [indent $ op <> "," | op <- unusedOps]
  ++ ["};"]

genEvaluator :: [Text]
genEvaluator =
  intercalate [""] (map (map indent) $
    genEvalSwitch : genEvalIndirect : map genInsnEval instructions)

-- | Generate a method which decodes and then executes (via a function which
-- we expect to be defined) an instruction. For each instruction, we generate
-- a struct containing its decoded arguments and adjust PC to point to the next
-- instruction in the stream.
genInsnEval :: Insn -> [Text]
genInsnEval Insn{..} =
  [ "struct " <> insnName <> " {" ]
  ++ map indent (concatMap declare insnArgs) ++
  [ "};"
  , ""
  , "FOLLY_ALWAYS_INLINE " <> retType <> " eval_" <> insnName <> "() {"
  , "  " <> insnName <> " args;" ]
  ++ map indent (concatMap decode insnArgs) ++
  [ "  return execute(args);"
  , "}" ]
  where
    retType
      | Return `elem` insnEffects = "const uint64_t * FOLLY_NULLABLE "
      | otherwise = "void"

    declare (Arg name (Imm ty)) =
      [ immType ty <> " " <> name <> ";" ]
    declare (Arg name (Reg _ ty Load)) =
      [ cppType ty <> " " <> name <> ";" ]
    -- just make a pointer to the register for Store and Update for now
    declare (Arg name (Reg _ ty _)) =
      [ "Reg<" <> cppType ty <> "> " <> name <> ";" ]
    declare (Arg name Offsets) =
      [ "uint64_t " <> name <> "_size;"
      , "const uint64_t *" <> name <> ";" ]
    declare (Arg name (Regs tys))  =
      [ "static constexpr uint64_t " <> name <> "_arity = "
          <> Text.pack (show (length tys) <> ";")
      , "const uint64_t *" <> name <> ";" ]

    decode (Arg name (Imm ImmLit)) =
      [ "args." <> name <> " = &literals[*pc++];" ]
    decode (Arg name (Imm _)) =
      [ "args." <> name <> " = *pc++;" ]
    decode (Arg name (Reg _ ty Load)) =
      [ "args." <> name <> " = Reg<" <> cppType ty <> ">(&frame[*pc++]).get();" ]
    decode (Arg name (Reg _ ty _)) =
      [ "args." <> name <> " = Reg<" <> cppType ty <> ">(&frame[*pc++]);" ]
    decode (Arg name Offsets) =
      [ "args." <> name <> "_size = *pc++;"
      , "args." <> name <> " = pc;"
      , "pc += args." <> name <> "_size;" ]
    decode (Arg name (Regs _)) =
      [ "args." <> name <> " = pc;"
      , "pc += args." <> name <> "_arity;" ]

cppType :: Ty -> Text
cppType DataPtr = "const unsigned char *"
cppType Lit = "const std::string *"
cppType WordPtr = "uint64_t *"
cppType BinaryOutputPtr = "binary::Output *"
cppType (Fun _) = "SysFun"
cppType _ = "uint64_t"

immType :: ImmTy -> Text
immType ImmWord = "uint64_t"
immType ImmOffset = "uint64_t"
immType ImmLit = "const std::string *"

-- | Generate a switch-based evaluator.
--
-- while(true) {
--   switch (static_cast<Op>(*pc++)) {
--     case Op::Name:
--       eval_Name();
--       break;
--      ...
--
--     case Op::Unused42:
--     ...
--     case Op::Unused255:
--       rts::error("invalid opcode");
--    }
-- }
--
-- Note that we generate alternatives for each possible value of the opcode byte
-- rather than using `default` because this results in better code (jump via
-- table at end of each alternative rather than bounds check + jump via table in
-- a common loop).
--
genEvalSwitch :: [Text]
genEvalSwitch =
  [ "FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE evalSwitch() {"
  , "  while (true) {"
  , "    switch (static_cast<Op>(*pc++)) {" ]
  ++ intercalate [""] (map genAlt instructions)
  ++
  [ "" ]
  ++ map genUnusedAlt unusedOps ++
  [ "        rts::error(\"invalid opcode\");"
  , "    }"
  , "  }"
  , "}" ]
  where
    genAlt insn =
      "      case Op::" <> insnName insn <> ":"
      : makeCall insn "        break;"

    genUnusedAlt op =
      "      case Op::" <> op <> ":"

-- | Generate a token-threaded interpreter (opcode are indices into a table
-- of labels, dispatch via compute goto). Note dispatch is repeated for each
-- instruction for (perhaps) better branch prediction.
--
-- static const void * const labels[] = { &&label_Name, ... };
--
-- goto *labels[*pc++];
--
-- label_Name:
--   eval_Name();
--   goto *labels[*pc++];
--
genEvalIndirect :: [Text]
genEvalIndirect =
  [ "FOLLY_ALWAYS_INLINE const uint64_t * FOLLY_NULLABLE evalIndirect() {"
  , "  static const void * const labels[] = {" ]
  ++
  [ "    &&label_" <> insnName insn <> "," | insn <- instructions ]
  ++
  [ "  };"
  , ""
  , dispatch
  , "" ]
  ++ intercalate [""] (map genAlt instructions) ++
  ["}"]
  where
    dispatch = "  goto *labels[*pc++];"

    genAlt insn =
      "label_" <> insnName insn <> ":"
      : makeCall insn dispatch

makeCall :: Insn -> Text -> [Text]
makeCall insn cont
  | Return `elem` insnEffects insn =
      [ "        return " <> call ]
  | otherwise =
      [ "        " <> call
      , cont ]
  where
    call = "eval_" <> insnName insn <> "();"

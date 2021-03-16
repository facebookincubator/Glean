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

-- | Generate the enum with all opcodes.
genOpEnum :: [Text]
genOpEnum =
  "enum class Op : uint64_t {"
  : [indent $ insnName insn <> "," | insn <- instructions]
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
  , "FOLLY_ALWAYS_INLINE void eval_" <> insnName <> "() {"
  , "  " <> insnName <> " args;" ]
  ++ map indent (concatMap decode insnArgs) ++
  [ "  execute(args);"
  , "}" ]
  where
    declare (Arg name Offsets Imm) =
      [ "uint64_t " <> name <> "_size;"
      , "const uint64_t *" <> name <> ";" ]
    declare (Arg name ty Imm) =
      [ cppType ty <> " " <> name <> ";" ]
    declare (Arg name ty Load) =
      [ cppType ty <> " " <> name <> ";" ]
    -- just make a pointer to the register for Store and Update for now
    declare (Arg name ty _) =
      [ cppType ty <> "* " <> name <> ";" ]

    decode (Arg name Literal Imm) =
      [ "args." <> name <> " = &literals[*pc++];" ]
    decode (Arg name Offsets Imm) =
      [ "args." <> name <> "_size = *pc++;"
      , "args." <> name <> " = pc;"
      , "pc += args." <> name <> "_size;" ]
    decode (Arg name ty Imm) =
      [ "args." <> name <> " = " <> cppCast ty "*pc++" <> ";" ]
    decode (Arg name ty Load) =
      [ "args." <> name <> " = " <> cppCast ty "frame[*pc++]" <> ";" ]
    decode (Arg name ty _) =
      [ "args." <> name <> " = " <> cppCastPtr ty "&frame[*pc++]" <> ";" ]

    cppCast ty s
      | cppType ty /= "uint64_t" =
          "reinterpret_cast<" <> cppType ty <> ">(" <> s <> ")"
      | otherwise = s

    cppCastPtr ty s
      | cppType ty /= "uint64_t" =
          "reinterpret_cast<" <> cppType ty <> "*>(" <> s <> ")"
      | otherwise = s

cppType :: Ty -> Text
cppType Void = "void"
cppType DataPtr = "void *"
cppType Literal = "const std::string *"
cppType WordPtr = "uint64_t *"
cppType BinaryOutputPtr = "binary::Output *"
cppType (Fun arg_tys res_ty) =
  "std::function<" <> cppType res_ty
  <> "(" <> Text.intercalate ", " (map cppType arg_tys) <> ")> *"
cppType _ = "uint64_t"

-- | Generate a switch-based evaluator.
--
-- while(true) {
--   switch (static_cast<Op>(*pc++)) {
--     case Op::Name:
--       eval_Name();
--       break;
--      ...
--    }
-- }
genEvalSwitch :: [Text]
genEvalSwitch =
  [ "FOLLY_ALWAYS_INLINE void evalSwitch() {"
  , "  while (true) {"
  , "    switch (static_cast<Op>(*pc++)) {" ]
  ++ intercalate [""] (map genAlt instructions) ++
  [ ""
  , "      default:"
  , "        rts::error(\"invalid opcode\");"
  , "    }"
  , "  }"
  , "}" ]
  where
    genAlt insn =
      [ "      case Op::" <> insnName insn <> ":"
      , "        eval_" <> insnName insn <> "();" ]
      ++
      if insnControl insn == UncondReturn
      then
      [ "        return;"]
      else
      [ "        break;"]

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
  [ "FOLLY_ALWAYS_INLINE void evalIndirect() {"
  , "  static const void * const labels[] = {" ]
  ++
  [ "    &&label_" <> insnName insn <> "," | insn <- instructions ]
  ++
  [ "  };"
  , "" ]
  ++
  dispatch
  ++
  [ "" ]
  ++ intercalate [""] (map genAlt instructions) ++
  ["}"]
  where
    dispatch = [ "  goto *labels[*pc++];"]

    genAlt insn =
      [ "label_" <> insnName insn <> ":"
      , "  eval_" <> insnName insn <> "();" ]
      ++
      if insnControl insn == UncondReturn then [ "  return;"] else dispatch

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Generate.Haskell (main)
where

import Data.Char (toLower)
import Data.List (intercalate, intersperse, stripPrefix)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.Environment
import System.Exit (die)
import System.FilePath

import Glean.Bytecode.Generate.Instruction
import Glean.Bytecode.Types

-- We generate two modules:
--
-- Instruction: contains the Insn type and some utility functions
--
-- Issue: a typed interface for generating instructions based on the MonadInsn
-- class from Glean.RTS.Bytecode.MonadInsn.

list :: Text -> Text -> [Text] -> [Text]
list first others (x:xs) = first <> x : map (others <>) xs
list _ _ [] = []

modPfx :: Text
modPfx = "Glean.RTS.Bytecode.Gen"

modPath :: FilePath
modPath = Text.unpack $ Text.replace "." "/" modPfx

main :: IO ()
main = do
  args <- getArgs
  dir <- case args of
    [arg] | Just dir <- stripPrefix "--install_dir=" arg ->
      return $ dir </> modPath
    _ -> die "invalid arguments"
  createDirectoryIfMissing True dir
  genModule dir "Instruction"
    []
    [ "Insn(..)"
    , "mapLabels"
    , "insnLabels"
    , "insnSize"
    , "insnWords"
    , "insnShow" ] $
    intercalate [""]
    [ [ "import Data.Word (Word64)"
      , "import Text.Show (showListWith)"
      , "import qualified Glean.Bytecode.Decode as D"
      , "import Glean.Bytecode.Types" ]
    , genInsnType
    , genMapLabels
    , genInsnLabels
    , genInsnSize
    , genInsnWords
    , genDecodable
    , genInsnShow ]

  genModule dir "Issue" [] (map (varName . insnName) instructions) $
    [ "import Data.ByteString (ByteString)"
    , "import Data.Word (Word64)"
    , "import Glean.RTS.Bytecode.Gen.Instruction (Insn(..))"
    , "import Glean.RTS.Bytecode.Code"
    , "import Glean.Bytecode.Types"
    , "" ]
    ++ genIssue

  genModule dir "Version" [] ["version", "lowestSupportedVersion"]
    [ "version :: Int"
    , "version = " <> Text.pack (show version)
    , ""
    , "lowestSupportedVersion :: Int"
    , "lowestSupportedVersion = " <> Text.pack (show lowestSupportedVersion)
    ]

genModule :: FilePath -> String -> [Text] -> [Text] -> [Text] -> IO ()
genModule path name exts es ls =
  Text.writeFile (path </> name <.> "hs")
  $ Text.unlines $
  [ "-- @" <> "generated" ]
  ++
  [ "{-# LANGUAGE " <> Text.intercalate ", " exts <> " #-}" | not (null exts)]
  ++
  [ "{-# OPTIONS_GHC -Wno-unused-matches #-}"
  , "module " <> modPfx <> "." <> Text.pack name ]
  ++ list "  ( " "  , " es ++
  [ "  ) where"
  , "" ]
  ++ ls

-- | Generate the Insn type. We parametrise over the types of registers and
-- labels.
genInsnType :: [Text]
genInsnType = "data Insn" : list "  = " "  | " (map genInsn instructions)
  where
    genInsn Insn{..} = Text.unwords $ insnName : map (genArgTy . argTy)  insnArgs

    genArgTy (Imm Offset) = "{-# UNPACK #-} !Label"
    genArgTy Imm{} = "{-# UNPACK #-} !Word64"
    genArgTy (Reg ty _) = "{-# UNPACK #-} !(Register " <> showTy ty <> ")"
    genArgTy Offsets = "[Label]"
    genArgTy Regs = "[Register 'Word]"

showTy :: Ty -> Text
showTy ty = "'" <> Text.pack (show ty)

genMapLabels :: [Text]
genMapLabels =
  "mapLabels :: (Label -> Label) -> Insn -> Insn" :
  [ "mapLabels f ("
      <> Text.unwords (insnName insn : map argName (insnArgs insn))
      <> ") = "
      <> Text.unwords (insnName insn : exprs)
    | insn <- instructions
    , let exprs = map mkMap (insnArgs insn)
    , any (\(expr,arg) -> expr /= argName arg) $ zip exprs $ insnArgs insn ]
  ++
  [ "mapLabels _ insn = insn"]
  where
    mkMap (Arg name (Imm Offset)) = "(f " <> name <> ")"
    mkMap (Arg name Offsets) = "(map f " <> name <> ")"
    mkMap (Arg name _) = name

genInsnLabels :: [Text]
genInsnLabels =
  "insnLabels :: Insn -> [Label]" :
  [ "insnLabels ("
      <> Text.unwords (insnName insn : map argName (insnArgs insn))
      <> ") = "
      <> Text.intercalate " ++ " exprs
    | insn <- instructions
    , let exprs = mapMaybe mkList (insnArgs insn)
    , not $ null exprs ]
  ++
  [ "insnLabels _ = []"]
  where
    mkList (Arg name (Imm Offset)) = Just $ "[" <> name <> "]"
    mkList (Arg name Offsets) = Just name
    mkList Arg{} = Nothing

-- | Generates a function that yields the size of an instruction in words.
genInsnSize :: [Text]
genInsnSize =
  "insnSize :: Insn -> Word64" :
  [ "insnSize ("
      <> Text.unwords (insnName insn : map mkArg (insnArgs insn))
      <> ") = 1"
      <> mconcat [" + " <> size arg | arg <- insnArgs insn]
    | insn <- instructions ]
  where
    mkArg (Arg name Offsets) = name
    mkArg (Arg name Regs) = name
    mkArg _ = "_"

    size (Arg name Offsets) = "fromIntegral (length " <> name <> ") + 1"
    size (Arg name Regs) = "fromIntegral (length " <> name <> ") + 1"
    size _ = "1"

-- | Generates a function that encodes an instruction as a list of words.
genInsnWords :: [Text]
genInsnWords =
   [ "insnWords"
   , "  :: (forall ty. Register ty -> Word64) -> (Label -> Word64) -> Insn -> [Word64]" ]
   ++
   [ "insnWords fromReg fromLabel ("
      <> insnPattern id insn
      <> ") = concat ["
      <> Text.intercalate ", "
          ("[" <> Text.pack (show op) <> "]" : map argWords (insnArgs insn))
      <> "]" | (op, insn) <- zip [0 :: Int ..] instructions ]
    where
      argWords (Arg name (Imm Offset)) = "[fromLabel " <> name <> "]"
      argWords (Arg name Imm{}) = "[" <> name <> "]"
      argWords (Arg name Reg{}) = "[fromReg " <> name <> "]"
      argWords (Arg name Offsets) =
        "fromIntegral (length " <> name <> ") : map fromLabel " <> name
      argWords (Arg name Regs) =
        "fromIntegral (length " <> name <> ") : map fromReg " <> name

insnPattern :: (Text -> Text) -> Insn -> Text
insnPattern cname Insn{..} =
  Text.unwords $ cname insnName : map argName insnArgs

varName :: Text -> Text
varName name
  | Just (c,rest) <- Text.uncons name = Text.cons (toLower c) rest
  | otherwise = name

genIssue :: [Text]
genIssue = intercalate [""]
  [ [ varName insnName
        <> " :: "
        <> Text.unwords
            (intersperse "->"
              $ map (genArgType . argTy) insnArgs ++ ["Code ()"])
    , insnPattern varName insn <> " = do"
    ] ++ map ("  " <>)
      (mapMaybe literal insnArgs ++
      [ issue insnEffects <> " $ " <> Text.unwords (insnName : map genArgRef insnArgs) ])
    | insn@Insn{..} <- instructions ]
  where
    genArgType (Imm Literal) = "ByteString"
    genArgType (Imm Offset) = "Label"
    genArgType Imm{} = "Word64"
    genArgType (Reg ty _) = "Register " <> showTy ty
    genArgType Offsets = "[Label]"
    genArgType Regs = "[Register 'Word]"

    genArgRef (Arg name (Imm Literal)) = name <> "_i"
    genArgRef (Arg name _) = name

    literal (Arg name (Imm Literal)) = Just $
      name <> "_i <- literal " <> name
    literal _ = Nothing

    issue effects
      | EndBlock `elem` effects = "issueEndBlock"
      | otherwise = "issue"

genDecodable :: [Text]
genDecodable =
  [ "instance D.Decodable Insn where"
  , "  decode = do"
  , "    op <- D.decode"
  , "    case (op :: Word64) of" ]
  ++
  [ Text.concat $ [ "      ", Text.pack (show i), " -> " ] ++
    case length insnArgs of
      0 -> ["pure ", insnName]
      n -> insnName : " <$> D.decode" : replicate (n-1) " <*> D.decode"
    | (i, Insn{..}) <- zip [0 :: Int ..] instructions ]
  ++
  [ "      _ -> fail $ \"invalid opcode \" ++ show op"]

genInsnShow :: [Text]
genInsnShow =
  [ "insnShow"
  , "  :: (Label -> String)"
  , "  -> (forall t. Register t -> String)"
  , "  -> Insn"
  , "  -> String" ]
  ++
  [ "insnShow showLabel showReg ("
      <> insnPattern id insn
      <> ") = concat [\"" <> insnName <> "\""
      <> Text.intercalate ", \",\""
          [", \' \' : " <> showArg arg | arg <- insnArgs]
      <> "]" | insn@Insn{..} <- instructions ]
    where
      showArg (Arg name (Imm Offset)) = "showLabel " <> name
      showArg (Arg name Imm{}) = "show " <> name
      showArg (Arg name Reg{}) = "showReg " <> name
      showArg (Arg name Offsets) =
        "showListWith (showString . showLabel) " <> name <> " \"\""
      showArg (Arg name Regs) =
        "showListWith (showString . showReg) " <> name <> " \"\""

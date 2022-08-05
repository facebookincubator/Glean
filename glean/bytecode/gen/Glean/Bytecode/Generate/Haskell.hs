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
    [ "DeriveFunctor", "DeriveFoldable", "DeriveTraversable" ]
    [ "Insn(..)"
    , "insnSize"
    , "insnWords"
    , "insnControl"
    , "insnShow" ] $
    intercalate [""]
    [ [ "import Data.Word (Word64)"
      , "import Text.Show (showListWith)"
      , "import qualified Glean.Bytecode.Decode as D"
      , "import Glean.Bytecode.Types" ]
    , genInsnType
    , genInsnSize
    , genInsnWords
    , genInsnControl
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
genInsnType =
  [ "data Insn reg label" ]
  ++ list "  = " "  | " (map genInsn instructions)
  ++ [ "  deriving(Functor, Foldable, Traversable)" ]
--  ++
--  [ " deriving(Show)" ]
  where
    genInsn Insn{..} = Text.unwords $ insnName : map genArg insnArgs

    genArg (Arg _ Offset Imm) = "label"
    genArg (Arg _ Offsets Imm) = "[label]"
    genArg (Arg _ _ Imm) = "{-# UNPACK #-} !Word64"
    genArg (Arg _ ty _)  = "(reg " <> showTy ty <> ")"

showTy :: Ty -> Text
showTy ty = case ty of
  Fun args ->
    "('Fun '[ " <> Text.intercalate "," (map showTy args) <> " ])"
  _ -> "'" <> Text.pack (show ty)

-- | Generates a function that yields the size of an instruction in words.
genInsnSize :: [Text]
genInsnSize =
  "insnSize :: Insn reg label -> Word64" :
  [ "insnSize ("
      <> Text.unwords (insnName insn : map mkArg (insnArgs insn))
      <> ") = 1"
      <> mconcat [" + " <> size arg | arg <- insnArgs insn]
    | insn <- instructions ]
  where
    mkArg (Arg name Offsets Imm) = name
    mkArg _ = "_"

    size (Arg name Offsets Imm) = "fromIntegral (length " <> name <> ") + 1"
    size _ = "1"

-- | Generates a function that encodes an instruction as a list of words.
genInsnWords :: [Text]
genInsnWords =
   [ "insnWords"
   , "  :: (forall ty. reg ty -> Word64) -> (label -> Word64) -> Insn reg label -> [Word64]" ]
   ++
   [ "insnWords fromReg fromLabel ("
      <> Text.unwords (insnName insn : map argName (insnArgs insn))
      <> ") = concat ["
      <> Text.intercalate ", "
          ("[" <> Text.pack (show op) <> "]" : map argWords (insnArgs insn))
      <> "]" | (op, insn) <- zip [0 :: Int ..] instructions ]
    where
      argWords (Arg name Offset Imm) = "[fromLabel " <> name <> "]"
      argWords (Arg name Offsets Imm) =
        "fromIntegral (length " <> name <> ") : map fromLabel " <> name
      argWords (Arg name _ Imm) = "[" <> name <> "]"
      argWords (Arg name _ _) = "[fromReg " <> name <> "]"

varName :: Text -> Text
varName name
  | Just (c,rest) <- Text.uncons name = Text.cons (toLower c) rest
  | otherwise = name

genIssue :: [Text]
genIssue = intercalate [""]
  [ [ varName insnName
        <> " :: "
        <> Text.unwords
            (intersperse "->" $ map genArgType insnArgs ++ ["Code ()"])
    , Text.unwords (varName insnName : map argName insnArgs) <> " = do"
    ] ++ map ("  " <>)
      (mapMaybe literal insnArgs ++
      [ "issue $ " <> Text.unwords (insnName : map genArgRef insnArgs) ])
    | Insn{..} <- instructions ]
  where
    genArgType (Arg _ Literal Imm) = "ByteString"
    genArgType (Arg _ Offset Imm) = "Label"
    genArgType (Arg _ Offsets Imm) = "[Label]"
    genArgType (Arg _ _ Imm) = "Word64"
    genArgType (Arg _ ty _) = "Register " <> showTy ty

    genArgRef (Arg name Literal Imm) = name <> "_i"
    genArgRef (Arg name _ _) = name

    literal (Arg name Literal Imm) = Just $
      name <> "_i <- literal " <> name
    literal _ = Nothing

genInsnControl :: [Text]
genInsnControl =
 "insnControl :: Insn reg label -> Control" :
  [ "insnControl " <> insnName <> "{} = " <> Text.pack (show insnControl)
    | Insn{..} <- instructions ]

genDecodable :: [Text]
genDecodable =
  [ "instance D.Decodable (Insn D.Reg D.Offset) where"
  , "  decode = do"
  , "    op <- D.decode"
  , "    case (op :: Word64) of" ]
  ++
  [ Text.concat $ [ "      ", Text.pack (show i), " -> " ] ++
    case insnArgs of
      [] -> ["pure ", insnName]
      _ : xs -> [insnName, " <$> D.decode"] ++ map (const " <*> D.decode") xs
    | (i, Insn{..}) <- zip [0 :: Int ..] instructions ]
  ++
  [ "      _ -> fail $ \"invalid opcode \" ++ show op"]

genInsnShow :: [Text]
genInsnShow =
  [ "insnShow"
  , "  :: (label -> String)"
  , "  -> (forall t. reg t -> String)"
  , "  -> Insn reg label"
  , "  -> String" ]
  ++
  [ "insnShow showLabel showReg ("
      <> Text.unwords (insnName : map argName insnArgs)
      <> ") = concat [\"" <> insnName <> "\""
      <> Text.intercalate ", \",\""
          [", \' \' : " <> showArg arg | arg <- insnArgs]
      <> "]" | Insn{..} <- instructions ]
    where
      showArg (Arg name Offset Imm) = "showLabel " <> name
      showArg (Arg name Offsets Imm) =
        "showListWith (showString . showLabel) " <> name <> " \"\""
      showArg (Arg name _ Imm) = "show " <> name
      showArg (Arg name _ _) = "showReg " <> name

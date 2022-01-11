{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Bytecode.Disassemble
where

import qualified Data.Foldable as F
import Data.Functor
import Data.Int (Int64)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl', mapAccumL)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import Glean.Bytecode.Decode as D
import Glean.RTS.Bytecode.Gen.Instruction
import Glean.RTS.Foreign.Bytecode

disassemble :: Text -> Subroutine s -> [Text]
disassemble name sub =
  [ "Subroutine " <> name
  , "  #inputs = " <> Text.pack (show subInputs)
  , "  #outputs = " <> Text.pack (show subOutputs)
  , "  #locals = " <> Text.pack (show subLocals)
  , "  #instrs = " <> Text.pack (show (length instructions)) ]
  ++
  [ "  #" <> Text.pack (show i) <> " = " <> Text.pack (show lit)
    | (i,lit) <- zip [0 :: Int ..] subLiterals ]
  ++
  [ "  %" <> Text.pack (show i) <> " = " <> Text.pack (show c)
    | (i,c) <- zip [subInputs..] (V.toList subConstants) ]
  ++
  [ "" ]
  ++ code ++
  [ "*** ERROR ***" | not $ null rest ]
  where
    SubroutineCode{..} = inspect sub

    instructions :: [Insn D.Reg D.Offset]
    (instructions, rest) = D.decodeAll $ V.toList subInsns

    labels = snd
      $ IntMap.mapAccum
          (\ !i _ -> (i+1, '$' : show i))
          (0 :: Int)
      $ IntMap.fromSet (const ())
      $ snd
      $ foldl'
          (\(m, ts) insn ->
            let !n = m + insnSize insn
            in
            ( n
            , F.foldr
                (\o -> IntSet.insert $ fromIntegral $ n + D.fromOffset o)
                ts
                insn ))
          (0, IntSet.empty)
          instructions

    show_label o (D.Offset k) =
      fromMaybe (show (fromIntegral k :: Int64))
      $ IntMap.lookup (fromIntegral (o+k)) labels

    show_reg (D.Reg n) = '%' : show n

    code :: [Text]
    code = concat
      $ snd
      $ mapAccumL
        (\ !o insn ->
          let !o' = o + insnSize insn
          in
          ( o'
          , catMaybes
              [IntMap.lookup (fromIntegral o) labels
                <&> \s -> Text.pack s <> ":"]
            ++
            [ "  " <> Text.pack (insnShow (show_label o') show_reg insn) ] ))
        0
        instructions

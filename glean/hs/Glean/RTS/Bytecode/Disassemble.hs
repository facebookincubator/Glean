{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Bytecode.Disassemble
where

import qualified Data.ByteString as BS
import Data.Functor
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl', mapAccumL)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import Glean.Bytecode.Decode as D
import Glean.Bytecode.Types
import Glean.RTS.Bytecode.Gen.Instruction
import Glean.RTS.Foreign.Bytecode

disassemble :: Text -> [String] -> Subroutine s -> [Text]
disassemble name syscalls sub =
  [ "Subroutine " <> name
  , "  // Code: "
      <> Text.pack (show (length instructions)) <> " insns, "
      <> Text.pack (show (V.length subInsns)) <> " bytes"
  , "  // Literals: "
            <> Text.pack (show $ sum $ map BS.length subLiterals)
            <> " bytes"
  , "  #inputs = " <> Text.pack (show subInputs)
  , "  #outputs = " <> Text.pack (show subOutputs)
  , "  #locals = " <> Text.pack (show subLocals) ]
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

    instructions :: [Insn]
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
            , foldr
                (\o -> IntSet.insert $ fromIntegral n + fromLabel o)
                ts
                $ insnLabels insn ))
          (0, IntSet.empty)
          instructions

    show_label o (Label k) =
      fromMaybe (show k) $ IntMap.lookup (fromIntegral o + k) labels

    show_reg (Register n) = '%' : show n

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
            [ "  " <> Text.pack (insnShow syscalls (show_label o') show_reg insn) ] ))
        0
        instructions

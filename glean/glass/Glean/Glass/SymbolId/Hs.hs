{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Hs ({- instances -}) where
import qualified Data.Text as Text
import Glean.Glass.SymbolId.Class
    ( toSymbolPredicate, ToAngle(..), Symbol(..), mkKey )

import qualified Glean
import Glean.Angle ( alt )

import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src.Types as Src

import Glean.Schema.CodeHs.Types as Hs (Entity (..))

instance Symbol Hs.Definition_key where
  toSymbol (Hs.Definition_key name _) = do
    n <- Glean.keyOf name
    return (Text.splitOn "." n)

instance Symbol Hs.FunctionDefinition_key where
  toSymbol (Hs.FunctionDefinition_key fnName Src.Range{..}) = do
    name <- Glean.keyOf fnName
    fname <- Glean.keyOf range_file
    return (fname : Text.splitOn "." name)

instance Symbol Hs.Class_key where
  toSymbol (Hs.Class_key clsName Src.Range{..}) = do
    name <- Glean.keyOf clsName
    fname <- Glean.keyOf range_file
    return (fname : Text.splitOn "." name)

instance Symbol Hs.Entity where
  toSymbol (Hs.Entity_definition d) = toSymbolPredicate d
  toSymbol (Hs.Entity_function_ d) = toSymbolPredicate d
  toSymbol (Hs.Entity_class_ d) = toSymbolPredicate d
  toSymbol Hs.Entity_EMPTY = return []

instance ToAngle Hs.Entity where
  toAngle e = case e of
    Hs.Entity_class_ x -> alt @"class_" (mkKey x)
    Hs.Entity_definition x -> alt @"definition" (mkKey x)
    Hs.Entity_function_ x -> alt @"function_" (mkKey x)
    Hs.Entity_EMPTY -> error "unknown Entity"

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Hs ({- instances -}) where

import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import Glean.Schema.CodeHs.Types as Hs (Entity (..))
import qualified Glean
import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src.Types as Src

instance Symbol Hs.Entity where
  toSymbol (Hs.Entity_definition d) = toSymbolPredicate d
  toSymbol (Hs.Entity_function_ d) = toSymbolPredicate d
  toSymbol (Hs.Entity_class_ d) = toSymbolPredicate d
  toSymbol Hs.Entity_EMPTY = return []

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

instance ToQName Hs.Entity where
  toQName (Hs.Entity_definition d) = Glean.keyOf d >>= toQName
  toQName (Hs.Entity_function_ d) = Glean.keyOf d >>= toQName
  toQName (Hs.Entity_class_ d) = Glean.keyOf d >>= toQName
  toQName Hs.Entity_EMPTY =
    return $ Left "toQName: Haskell: empty qname"

instance ToQName Hs.Definition_key where
  toQName (Hs.Definition_key name _) = do
    n <- Glean.keyOf name
    return $ case reverse (Text.splitOn "." n) of
      [] -> Left "toQName: Haskell: empty function qname"
      [x] -> Right (Name x, Name "")
      (x:xs) -> Right (Name x, joinDotted xs)

instance ToQName Hs.FunctionDefinition_key where
  toQName (Hs.FunctionDefinition_key fnName _) = do
    name <- Glean.keyOf fnName
    return $ case reverse (Text.splitOn "." name) of
      [] -> Left "toQName: Haskell: empty function qname"
      [x] -> Right (Name x, Name "")
      (x:xs) -> Right (Name x, joinDotted xs)


instance ToQName Hs.Class_key where
  toQName (Hs.Class_key clsName _) = do
    name <- Glean.keyOf clsName
    return $ case reverse (Text.splitOn "." name) of
      [] -> Left "toQName: Haskell: empty class qname"
      [x] -> Right (Name x, Name "")
      (x:xs) -> Right (Name x, joinDotted xs)

joinDotted :: [Text] -> Name
joinDotted = Name . Text.intercalate "." . reverse

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Pp (
    {- instances -}
    ppEntityKind
  ) where

import Data.Text ( Text )

import Glean.Glass.SymbolId.Class

import qualified Glean
import qualified Glean.Haxl.Repos as Glean
import Glean.Glass.Utils
import Glean.Glass.Types as Glass

import qualified Glean.Schema.CodePp.Types as Pp
import qualified Glean.Schema.Pp1.Types as Pp1
import qualified Glean.Schema.Src.Types as Src

instance Symbol Pp.Entity where
  toSymbol e = case e of
    Pp.Entity_define e -> toSymbolPredicate e
    Pp.Entity_undef e -> toSymbolPredicate e
    Pp.Entity_include_ f -> toSymbol f
    Pp.Entity_EMPTY -> return []

instance Symbol Pp1.Define_key where
  toSymbol (Pp1.Define_key macro source) =
    (\x -> "define" : x) <$> toSymbolMacro macro source

instance Symbol Pp1.Undef_key where
  toSymbol (Pp1.Undef_key macro source) =
    (\x -> "undef" : x) <$> toSymbolMacro macro source

instance Symbol Src.File where
  toSymbol k = pathFragments <$> Glean.keyOf k

toSymbolMacro :: Pp1.Macro -> Src.Range -> Glean.RepoHaxl u w [Text]
toSymbolMacro macro source = do
  name <- Glean.keyOf macro
  path <- pathFragments <$> Glean.keyOf (Src.range_file source)
  return $ path ++ [name]

instance ToSymbolParent Pp1.Define where
  toSymbolParent _ = return Nothing

instance ToSymbolParent Pp.Entity where
  toSymbolParent _ = return Nothing

ppEntityKind ::  Pp.Entity -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
ppEntityKind e = case e of
  Pp.Entity_define{} -> return (Just SymbolKind_Macro)
  Pp.Entity_undef{} -> return (Just SymbolKind_Macro)
  Pp.Entity_include_{} -> return (Just SymbolKind_File)
  Pp.Entity_EMPTY -> return Nothing

instance ToQName Pp.Entity where
  toQName e = case e of
    Pp.Entity_define e -> do
      Pp1.Define_key macro source <- Glean.keyOf e
      macroQName macro source
    Pp.Entity_undef e -> do
      Pp1.Undef_key macro source <- Glean.keyOf e
      macroQName macro source
    Pp.Entity_include_ f -> do -- Not sure we need #include entities
      path <- Glean.keyOf f
      return $ case reverse (pathFragments path) of
        [] -> Left "QName not supported for empty #include path"
        (h:t) -> Right (Name h, Name (joinFragments (reverse t)))
    Pp.Entity_EMPTY -> return $ Left "unknown code.pp.Entity"

macroQName
  :: Pp1.Macro -> Src.Range
  -> Glean.RepoHaxl u w (Either Text (Name, Name))
macroQName macro source = do
  toks <- toSymbolMacro macro source
  return $ case reverse toks of
    [] -> Left "macroQName: empty"
    [name] -> Right (Name name, Name "")
    (name : base : _) -> Right (Name name, Name base)

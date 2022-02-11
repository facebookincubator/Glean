{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Rust ({- instances -}) where

import Glean.Glass.SymbolId.Class

import qualified Glean
-- import Glean.Angle

import qualified Glean.Schema.Rust.Types as Rust
import Glean.Schema.CodeRust.Types as Rust ( Entity(..) )

instance Symbol Rust.Entity where
  toSymbol (Rust.Entity_definition defn) = toSymbol defn

instance Symbol Rust.Def where
  toSymbol (Rust.Def_const_ d) = toSymbolPredicate d
  toSymbol (Rust.Def_enum_ d) = toSymbolPredicate d
  toSymbol (Rust.Def_field d) = toSymbolPredicate d
  toSymbol (Rust.Def_foreign_function d) = toSymbolPredicate d
  toSymbol (Rust.Def_foreign_static d) = toSymbolPredicate d
  toSymbol (Rust.Def_function_ d) = toSymbolPredicate d
  toSymbol (Rust.Def_local d) = toSymbolPredicate d
  toSymbol (Rust.Def_method d) = toSymbolPredicate d
  toSymbol (Rust.Def_module d) = toSymbolPredicate d
  toSymbol (Rust.Def_static_ d) = toSymbolPredicate d
  toSymbol (Rust.Def_struct_ d) = toSymbolPredicate d
  toSymbol (Rust.Def_struct_variant d) = toSymbolPredicate d
  toSymbol (Rust.Def_trait d) = toSymbolPredicate d
  toSymbol (Rust.Def_tuple_variant d) = toSymbolPredicate d
  toSymbol (Rust.Def_type d) = toSymbolPredicate d
  toSymbol (Rust.Def_union_ d) = toSymbolPredicate d

instance Symbol Rust.ConstDef_key where
  toSymbol (Rust.ConstDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.EnumDef_key where
  toSymbol (Rust.EnumDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.FieldDef_key where
  toSymbol (Rust.FieldDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.ForeignFunctionDef_key where
  toSymbol (Rust.ForeignFunctionDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.ForeignStaticDef_key where
  toSymbol (Rust.ForeignStaticDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.LocalDef_key where
  toSymbol (Rust.LocalDef_key qname _ty) =
    ("local":) <$> toSymbolPredicate qname

instance Symbol Rust.MethodDef_key where
  toSymbol (Rust.MethodDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.FunctionDef_key where
  toSymbol (Rust.FunctionDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.ModuleDef_key where
  toSymbol (Rust.ModuleDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.StaticDef_key where
  toSymbol (Rust.StaticDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.StructDef_key where
  toSymbol (Rust.StructDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.StructVariantDef_key where
  toSymbol (Rust.StructVariantDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.TraitDef_key where
  toSymbol (Rust.TraitDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.TupleVariantDef_key where
  toSymbol (Rust.TupleVariantDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.TypeDef_key where
  toSymbol (Rust.TypeDef_key qname _ty) = toSymbolPredicate qname

instance Symbol Rust.UnionDef_key where
  toSymbol (Rust.UnionDef_key qname) = toSymbolPredicate qname

instance Symbol Rust.QName where
  toSymbol qname = toSymbolPredicate qname

instance Symbol Rust.QName_key where
  toSymbol (Rust.QName_key name parent) = parent <:> name

instance Symbol Rust.Name where
  toSymbol name = do
    n <- Glean.keyOf name
    return [n]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Thrift
  ({- instances -})
  where

import Glean.Glass.SymbolId.Class
    ( toSymbolPredicate, Symbol(..), (<:>) )

import qualified Glean.Schema.Thrift.Types as Thrift
import qualified Glean

import Glean.Schema.CodeThrift.Types as CodeThrift
    ( Entity(..) )

import Glean.Glass.Utils ( pathFragments )
import qualified Glean.Schema.Src.Types as Src

instance Symbol CodeThrift.Entity where
  toSymbol e = case e of
    CodeThrift.Entity_include_ thrift_file -> toSymbolPredicate thrift_file
    CodeThrift.Entity_named named -> toSymbolPredicate named
    CodeThrift.Entity_exception_ exception_ -> toSymbolPredicate exception_
    CodeThrift.Entity_service_ service_ -> toSymbolPredicate service_
    CodeThrift.Entity_constant constant -> toSymbolPredicate constant
    CodeThrift.Entity_enumValue enumvalue -> toSymbolPredicate enumvalue
    CodeThrift.Entity_EMPTY -> return []

instance Symbol Src.File where
  toSymbol k = pathFragments <$> Glean.keyOf k

instance Symbol Thrift.File where
  toSymbol f = toSymbolPredicate f

instance Symbol Thrift.NamedDecl_key where
  toSymbol (Thrift.NamedDecl_key (Thrift.NamedType qualname _kind) _locName) =
    toSymbolPredicate qualname

instance Symbol Thrift.EnumValue_key where
  toSymbol (Thrift.EnumValue_key (Thrift.NamedType qualname _kind) name _loc) =
    qualname <:> name

instance Symbol Thrift.Constant_key where
  toSymbol (Thrift.Constant_key qualname _loc) =
    toSymbolPredicate qualname

instance Symbol Thrift.ServiceName_key where
  toSymbol (Thrift.ServiceName_key qualname _loc) =
    toSymbolPredicate qualname

instance Symbol Thrift.ExceptionName_key where
  toSymbol (Thrift.ExceptionName_key qualname _loc) =
    toSymbolPredicate qualname

instance Symbol Thrift.QualName_key where
  toSymbol (Thrift.QualName_key file name) = file <:> name

instance Symbol Thrift.QualName where
  toSymbol = toSymbolPredicate

instance Symbol Thrift.Identifier where
  toSymbol identifier = do
    n <- Glean.keyOf identifier
    return [n]

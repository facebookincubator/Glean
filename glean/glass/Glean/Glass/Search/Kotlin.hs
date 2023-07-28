{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Kotlin
  ( {- instances -}
  ) where

import Data.Text as Text ( Text )
import Data.List.NonEmpty  ( NonEmpty((:|)) )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Glass.Search.Java as JavaKotlin

import qualified Glean.Schema.CodeKotlin.Types as Kotlin
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.KotlinAlpha.Types as Kotlin
import qualified Glean.Schema.JavakotlinAlpha.Types as JavaKotlin
import qualified Glean.Schema.SymbolidKotlin.Types as Kotlin
import qualified Glean.Schema.Src.Types as Src

instance Search Kotlin.Entity where
  symbolSearch skot = case toks of
      [] -> return $ None "Kotlin.symbolSearch: empty"
      [_] -> return $ None "Kotlin.symbolSearch: singleton: not a symbolid"
      (name:base:rest) -> do
        result <- searchSymbolId skot $
          searchByQName (JavaKotlin.toQName name (base :| rest))
        case result of
          None{}  -> searchSymbolId skot $
            searchByMName (JavaKotlin.toMName name (base :| rest) Nothing)
          x -> return x
    where
      toks = reverse skot

searchByQName
  :: Angle JavaKotlin.QName -> Angle (ResultLocation Kotlin.Entity)
searchByQName qname =
  vars $ \(entity :: Angle Kotlin.Entity) (file :: Angle Src.File)
    (decl :: Angle Kotlin.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Kotlin.LookupDeclaration (
        rec $
          field @"qname" (asPredicate qname) $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"kotlin" entity) file rangespan lname
    ]

-- methods
searchByMName
  :: Angle JavaKotlin.MethodName -> Angle (ResultLocation Kotlin.Entity)
searchByMName mname =
  vars $ \(entity :: Angle Kotlin.Entity) (file :: Angle Src.File)
    (decl :: Angle Kotlin.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Kotlin.LookupMethodDeclaration (
        rec $
          field @"mname" (asPredicate mname) $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"kotlin" entity) file rangespan lname
    ]

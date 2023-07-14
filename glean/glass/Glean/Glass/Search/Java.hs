{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Java
  ( {- instances -}
    toQName,
    toMName
  ) where

import Data.Text as Text ( Text )
import Data.List.NonEmpty  ( NonEmpty((:|)) )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeJava.Types as Java
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as Java
import qualified Glean.Schema.SymbolidJava.Types as Java
import qualified Glean.Schema.Src.Types as Src

instance Search Java.Entity where
  symbolSearch skot = case toks of
      [] -> return $ None "Java.symbolSearch: empty"
      [_] -> return $ None "Java.symbolSearch: singleton: not a symbolid"
      (name:base:rest) -> do
        result <- searchSymbolId toks $
          searchByQName (toQName name (base :| rest))
        case result of
          None{}  -> searchSymbolId toks $
            searchByMName (toMName name (base :| rest))
          x -> return x
    where
      toks = reverse skot

searchByQName :: Angle Java.QName_key -> Angle (ResultLocation Java.Entity)
searchByQName qname =
  vars $ \(entity :: Angle Java.Entity) (file :: Angle Src.File)
    (decl :: Angle Java.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Java.LookupDeclaration (
        rec $
          field @"qname" qname $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"java" entity) file rangespan lname
    ]

-- methods and cosntructors. todo: overloading
searchByMName :: Angle Java.MethodName_key -> Angle (ResultLocation Java.Entity)
searchByMName mname =
  vars $ \(entity :: Angle Java.Entity) (file :: Angle Src.File)
    (decl :: Angle Java.Declaration) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      wild .= predicate @Java.LookupMethodDeclaration (
        rec $
          field @"mname" mname $
          field @"decl" decl
        end),
      alt @"decl" decl .= sig entity,
      entityLocation (alt @"java" entity) file rangespan lname
    ]

--
-- Generic to Java and Kotlin
--

toMName :: Text -> NonEmpty Text -> Angle Java.MethodName_key
toMName name (base :| rest) =
  rec $
    field @"name" (toQName name (base :| rest))
  end

toQName :: Text -> NonEmpty Text -> Angle Java.QName_key
toQName name (base :| rest) =
  rec $
    field @"name" (string name) $
    field @"context" (toPath base rest)
  end

toPath :: Text -> [Text] -> Angle Java.Path_key
toPath base [] =
  rec $
    field @"base" (string base) $
    field @"container" nothing
  end
toPath base (x:xs) =
  rec $
    field @"base" (string base) $
    field @"container" (just (predicate (toPath x xs)))
  end

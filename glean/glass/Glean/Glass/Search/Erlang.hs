{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Glean.Glass.Search.Erlang
  ( {- instances -}
  ) where

import Data.Text ( Text, unpack )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeErlang.Types as Erlang
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Erlang.Types as ErlangSchema
import qualified Glean.Schema.SearchErlang.Types as Erlang
import qualified Glean.Schema.Src.Types as Src
import Text.Read ( readMaybe )
import Data.Word ( Word64 )

instance Search (ResultLocation Erlang.Entity) where
  symbolSearch toks
    | ["define", _app, _module, name, _arity] <- toks =
        searchSymbolId toks $ searchByNameAndKind "define" name
    | ["record", _app, _module, name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "record" name
    | ["type", app, module_, name, arity] <- toks
    , Just arityNum <- readMaybe $ unpack arity =
        searchSymbolId toks $
          searchTypeByFQN (Just app) module_ name (Just arityNum)
    | ["header", _app, name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "header" name
    | ["module", _app, name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "module" name
    | ["callback", _app, _module, name, _arity] <- toks =
        searchSymbolId toks $ searchByNameAndKind "callback" name
    | ["record_field", _app, _module, _recName, fieldName] <- toks =
        searchSymbolId toks $ searchByNameAndKind "record_field" fieldName
    | ["macro_usage", _app, _module, name, _arity, _contentHash] <- toks =
        searchSymbolId toks $ searchByNameAndKind "macro_usage" name
    | ["var", app, module_, name, spanStartStr] <- toks
    , Just spanStart <- readMaybe $ unpack spanStartStr =
        searchSymbolId toks $ searchVarByKey name module_ app spanStart
    | ["var", _app, _module, name, _spanStart] <- toks =
        searchSymbolId toks $ searchByNameAndKind "var" name
    | ["func", _app, module_, name, arity] <- toks
    , Just arityNum <- readMaybe $ unpack arity =
        searchSymbolId toks $ searchByFQN module_ name arityNum
    -- Legacy patterns (no leading tag)
    | [_app, module_, name, arity] <- toks
    , Just arityNum <- readMaybe $ unpack arity =
        searchSymbolId toks $ searchByFQN module_ name arityNum
    | ["define", _module, name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "define" name
    | ["record", _module, name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "record" name
    | ["type", module_, name] <- toks =
        searchSymbolId toks $ searchTypeByFQN Nothing module_ name Nothing
    | ["header", name] <- toks =
        searchSymbolId toks $ searchByNameAndKind "header" name
    | [module_, name, arity] <- toks
    , Just arityNum <- readMaybe $ unpack arity =
        searchSymbolId toks $ searchByFQN module_ name arityNum
    | otherwise = return $ None "Erlang.symbolSearch: invalid query"

searchByFQN :: Text -> Text -> Word64 -> Angle (ResultLocation Erlang.Entity)
searchByFQN module_ name arity =
  vars $ \(ent :: Angle Erlang.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Erlang.SearchByFQN (
      rec $
        field @"module" (string module_) $
        field @"name" (string name) $
        field @"arity" (nat arity) $
        field @"entity" ent
      end),
    entityLocation (alt @"erlang" ent) file rangespan lname
  ]

searchByNameAndKind :: Text -> Text -> Angle (ResultLocation Erlang.Entity)
searchByNameAndKind kind name =
  vars $ \(ent :: Angle Erlang.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Erlang.SearchByNameAndKind (
      rec $
        field @"kind" (string kind) $
        field @"name" (string name) $
        field @"entity" ent
      end),
    entityLocation (alt @"erlang" ent) file rangespan lname
  ]

searchTypeByFQN
  :: Maybe Text -> Text -> Text -> Maybe Word64
  -> Angle (ResultLocation Erlang.Entity)
searchTypeByFQN app module_ name arity =
  vars $ \(ent :: Angle Erlang.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
    (decl :: Angle ErlangSchema.TypeDeclaration) ->
  tuple (ent, file, rangespan, lname) `where_` [
    decl .= predicate @ErlangSchema.TypeDeclaration (
      rec $
        field @"name" (string name) $
        field @"arity" (maybe wild nat arity) $
        field @"module" (string module_) $
        field @"app" (maybe wild string app)
      end),
    ent .= alt @"decl" (alt @"type_" (asPredicate decl)),
    entityLocation (alt @"erlang" ent) file rangespan lname
  ]

searchVarByKey
  :: Text -> Text -> Text -> Word64 -> Angle (ResultLocation Erlang.Entity)
searchVarByKey name module_ app spanStart =
  vars $ \(ent :: Angle Erlang.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
    (decl :: Angle ErlangSchema.VarDeclaration) ->
  tuple (ent, file, rangespan, lname) `where_` [
    decl .= predicate @ErlangSchema.VarDeclaration (
      rec $
        field @"name" (string name) $
        field @"module" (string module_) $
        field @"app" (string app) $
        field @"span_start" (nat spanStart)
      end),
    ent .= alt @"var_" (asPredicate decl),
    entityLocation (alt @"erlang" ent) file rangespan lname
  ]

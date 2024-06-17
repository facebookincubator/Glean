{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Flow
  ( {- instances -}
  ) where

import Data.Text as Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodeFlow.Types as Flow
import qualified Glean.Schema.Flow.Types as Flow
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code

instance Search (ResultLocation Flow.Entity) where
  symbolSearch [] = return $ None "Flow.symbolSearch: empty"

  -- if its a single token it is likely a module (tbd: or a global sym)
  symbolSearch toks@[module_] = do
    m <- searchSymbolId toks (searchByModuleName module_)
    return $ mapResultLocation Flow.Entity_module_ <$> m

  -- module , method
  symbolSearch toks@[module_, name] = do
    a <- searchSymbolId toks $ searchByModule module_ name
    decl <- case a of
      None{} -> searchSymbolId toks $ searchTypeByModuleExport module_ name
      _ -> return a
    return $ mapResultLocation Flow.Entity_decl <$> decl

  -- inverse of T90301808 , if we don't have a short name use filepath
  symbolSearch toks = do
    let name = last toks
        path = joinFragments (init toks)
    a <- searchSymbolId toks $ searchByFileModule path name
    return $ mapResultLocation Flow.Entity_decl <$> a

-- With the Haste short module name and an identifier, find the decl
searchByModule :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchByModule module_ name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan,lname) `where_` [
      wild .= predicate @Flow.SearchByModule (
        rec $
          field @"string_" (string module_) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan lname
    ]

-- With the repo-anchored filepath and an identifier, find the decl
searchByFileModule
  :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchByFileModule path name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @Flow.SearchByFileModule (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan lname
    ]

-- With the Haste name, find the type decl from a .js.flow file
searchTypeByModuleExport
  :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchTypeByModuleExport module_ name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan,lname) `where_` [
      wild .= predicate @Flow.SearchTypeByModuleExport (
        rec $
          field @"string_" (string module_) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan lname
    ]

-- Single token module name lookup
searchByModuleName :: Text -> Angle (ResultLocation Flow.Module)
searchByModuleName modstr = vars $ \(modent :: Angle Flow.Module)
  (file :: Angle Src.File) (rangespan :: Angle Code.RangeSpan)
    (lname :: Angle Text) ->
  tuple (modent, file, rangespan, lname) `where_` [
    wild .= predicate @Flow.SearchByNameModule (
      rec $
        field @"name" (string modstr) $
        field @"module" (asPredicate modent)
      end),
    entityLocation (alt @"flow" (alt @"module_" (asPredicate modent)))
      file rangespan lname
  ]

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

import Data.Text as Text ( Text, pack, unpack )

import System.FilePath ( joinPath )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeFlow.Types as Flow
import qualified Glean.Schema.Flow.Types as Flow
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Codemarkup.Types as Code

instance Search Flow.Entity where
  symbolSearch [] = return $ None "Flow.symbolSearch: empty"

  symbolSearch toks@[module_, name] = do
    a <- runSearch toks $ searchByModule module_ name
    decl <- case a of
      None{} -> runSearch toks $ searchTypeByModuleExport module_ name
      _ -> return a
    return $ Flow.Entity_decl <$> decl

  -- inverse of T90301808 , if we don't have a short name use filepath
  symbolSearch toks = do
    let name = last toks
        path = joinFragments (init toks)
    a <- runSearch toks $ searchByFileModule path name
    return $ Flow.Entity_decl <$> a
    where
      -- opposite of SymbolId.Flow.pathFragments, reconstruct any path
      joinFragments = Text.pack . joinPath . map Text.unpack

-- With the Haste short module name and an identifier, find the decl
searchByModule :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchByModule module_ name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl,file,rangespan) `where_` [
      wild .= predicate @Flow.SearchByModule (
        rec $
          field @"string_" (string module_) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan
    ]

-- With the repo-anchored filepath and an identifier, find the decl
searchByFileModule
  :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchByFileModule path name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl,file,rangespan) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @Flow.SearchByFileModule (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan
    ]

-- With the Haste name, find the type decl from a .js.flow file
searchTypeByModuleExport
  :: Text -> Text -> Angle (ResultLocation Flow.SomeDeclaration)
searchTypeByModuleExport module_ name =
  vars $ \(decl :: Angle Flow.SomeDeclaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl,file,rangespan) `where_` [
      wild .= predicate @Flow.SearchTypeByModuleExport (
        rec $
          field @"string_" (string module_) $
          field @"name" (string name) $
          field @"decl" decl
        end),
      entityLocation (alt @"flow" (alt @"decl" decl)) file rangespan
    ]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo, PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Cxx
  ( {- instances -}
  ) where

import Data.Text ( Text )
import qualified Data.Text as Text ( null )

import Glean.Angle as Angle
import Glean.Haxl.Repos (ReposHaxl)

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.SearchCxx.Types as Cxx
import qualified Glean.Schema.SearchCode.Types as Code
import qualified Glean.Schema.Src.Types as Src

instance Search Cxx.Entity where
  symbolSearch [] =
    return $ None "Cxx.symbolSearch: empty symbol"
  symbolSearch [_] = do -- if the file is anchored at root, the path is missing?
    return $ None "Cxx.symbolSearch: missing path prefix"
  symbolSearch t@[path,name] = do -- i.e. global variable or container/struct?
    searchDefinitions t path [] name
  symbolSearch t@(path:rest@(_:_)) = case last rest of
    ".decl" -> searchDeclarations t path (init rest)
    name -> searchDefinitions t path (init rest) name

-- this is the most common path, for e.g. classes and functions
--
-- > fbsource/cpp @ fbcode/folly/Optional/assign
-- > fbsource/cpp @ fbcode/folly/Optional
--
-- would resolve to their definition occuences.
--
searchDefinitions
  :: [Text] -> Text -> [Text] -> Text -> ReposHaxl u w (SearchResult Cxx.Entity)
searchDefinitions t path ns local = searchSymbolId t $
  searchDefnByPathScopeAndName path ns local

--
-- declaration entities only
--
searchDeclarations
  :: [Text] -> Text -> [Text] -> ReposHaxl u w (SearchResult Cxx.Entity)
searchDeclarations _ _path [] =
  return $ None "Cxx.symbolSearch: empty decl symbol" -- invalid
searchDeclarations t path rest@(_:_) = do
  r <- searchSymbolId t $
    searchDeclByPathScopeAndName path (init rest) (last rest)
  case r of
    None{} -> searchSymbolId t $ searchNSDeclByPathAndScope path rest
    _ -> pure r

--
-- Definitions only, by path scope and name
--
searchDefnByPathScopeAndName
  :: Text -> [Text] -> Text -> Angle (ResultLocation Cxx.Entity)
searchDefnByPathScopeAndName anchor ns name =
  vars $ \(entity :: Angle Cxx.Entity) (codeEntity :: Angle Code.Entity)
     (file :: Angle Src.File) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity, file, rangespan, lname) `where_` ([
      wild .= predicate @Code.CxxSearchByNameAndScopeFact (
        rec $
          field @"name" (string name) $
          field @"scope" (scopeQuery ns) $
          field @"entity" codeEntity
        end),
      entityLocation codeEntity file rangespan lname,
      alt @"cxx" entity .= codeEntity
    ] ++ -- refine to specific sub-repo
    [file .= predicate @Src.File (stringPrefix anchor) | not (Text.null anchor)]
    )
  where
    scopeQuery ns = scope (reverse ns)


-- we always encode C++ symbols by name and scope, with a sub-repo prefix
--
-- E.g. xplat/folly/Optional is a class under "xplat" with namespace "folly"
--
-- Returns declarations. Not definitions.
--
searchDeclByPathScopeAndName
  :: Text -> [Text] -> Text -> Angle (ResultLocation Cxx.Entity)
searchDeclByPathScopeAndName anchor ns name =
  vars $ \(entity :: Angle Cxx.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (entity, file, rangespan, lname) `where_` ([
      wild .= predicate @Cxx.SearchByNameAndScope (
        rec $
          field @"name" (string name) $
          field @"scope" (scopeQuery ns) $
          field @"entity" entity
        end),
      entityLocation (alt @"cxx" entity) file rangespan lname
    ] ++ -- refine to specific sub-repo
    [file .= predicate @Src.File (stringPrefix anchor) | not (Text.null anchor)]
    )
  where
    scopeQuery ns = scope (reverse ns)

--
-- namespaces and records/container decls use scope-only search but its quite
-- lossy
--
searchNSDeclByPathAndScope
  :: Text -> [Text] -> Angle (ResultLocation Cxx.Entity)
searchNSDeclByPathAndScope anchor ns =
  vars $ \(entity :: Angle Cxx.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (entity, file, rangespan, lname) `where_` ([
      wild .= predicate @Cxx.SearchByScope (
        rec $
          field @"scope" (scopeQuery ns) $
          field @"entity" entity
        end),
      entityLocation (alt @"cxx" entity) file rangespan lname
    ] ++ -- refine to specific sub-repo
    [file .= predicate @Src.File (stringPrefix anchor) | not (Text.null anchor)]
    )
  where
    scopeQuery ns = scope (reverse ns)

--
-- Scope queries. There's lots of flavors.
--
scope :: [Text] -> Angle Cxx.Scope
scope [] = alt @"global_" wild
scope ns@(n:ns') =
  alt @"namespace_" (namespaceQName ns)
  .|
  alt @"recordWithAccess" (rec $
      field @"record" (rec $
        field @"name" (string n) $
        field @"scope" (scope ns')
      end)
    end)
  .|
  alt @"local" (functionQName n ns')

  where
    namespaceQName :: [Text] -> Angle Cxx.NamespaceQName_key
    namespaceQName [] =
      rec $
        field @"name" nothing $
        field @"parent" nothing
      end
    namespaceQName (n:ns) =
      rec $
        field @"name" (
          if n == "" then nothing else just (predicate (string n))) $
        field @"parent"
          (if null ns
            then nothing
            else just (predicate (namespaceQName ns)))
      end

    functionQName n ns =
      rec $
        field @"name" (alt @"name" (string n)) $
        field @"scope" (scope ns)
      end

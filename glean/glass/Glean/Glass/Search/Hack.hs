{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Hack
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeHack.Types as Hack
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.SearchHack.Types as Hack
import qualified Glean.Schema.Src.Types as Src

import Glean.Glass.Utils (searchWithLimit)

instance Search Hack.Entity where
  symbolSearch toks = fmap Hack.Entity_decl <$> symbolSearch toks

instance Search Hack.Declaration where
  symbolSearch t@("ns":[name]) = searchSymbolId t $ searchNamespace [] name
  symbolSearch ("ns":t@(_:_:_)) = do -- 2 or more path elements
    let name:rest = reverse t
    searchSymbolId t $ searchNamespace rest name

  symbolSearch ("fun":t@(_:_)) = do
    let name:rest = reverse t
    searchSymbolId t $ searchFunctionInNamespace rest name

  symbolSearch t@("module":[name]) = searchSymbolId t $ searchModule name

  symbolSearch ("const":t@(_:_)) = do -- 2 or more path elements
    let name:rest = reverse t
    searchSymbolId t $ searchGlobalConstInNamespace rest name

  symbolSearch t@[name] = searchSymbolId t $ searchTypeInNamespace [] name
  symbolSearch t@(_:_) = do -- 2 or more path elements
    let (name:rest@(context:ns)) = reverse t

    a <- searchSymbolId t $ searchTypeInNamespace rest name
    b <- searchSymbolId t $ searchInContainerOrEnum ns context name

    return $ case (a,b) of
      (x@One{}, _) -> x
      (_, y@One{}) -> y
      (x@Many{}, _) -> x
      (_, y@Many{}) -> y
      (x@None{}, None{}) -> x

  symbolSearch [] = return $ None "Hack.symbolSearch: empty path"

instance PrefixSearch Hack.Entity where
  prefixSearch lim toks =
    fmap (mapFst Hack.Entity_decl) <$> prefixSearch lim toks
    where mapFst f (x, y, z, a) = (f x, y, z, a)

instance PrefixSearch Hack.Declaration where
  prefixSearch lim [] = prefixSearch lim [""]
  prefixSearch lim ["ns", pName] = searchWithLimit (Just lim) $
    prefixSearchNamespace [] pName
  prefixSearch lim ("ns":toks) = do
    let (name:rest) = reverse toks
    searchWithLimit (Just lim) $ prefixSearchNamespace rest name
  prefixSearch lim ("fun":toks) = do
    let (name:rest) = reverse toks
    searchWithLimit (Just lim) $ prefixSearchFunctionInNamespace rest name
  prefixSearch lim ["mod", pName] = searchWithLimit (Just lim) $
    prefixSearchModule pName
  prefixSearch lim ("const":toks) = do
    let (name:rest) = reverse toks
    searchWithLimit (Just lim) $ prefixSearchGlobalConstInNamespace rest name
  prefixSearch lim [pName] = searchWithLimit (Just lim) $
    prefixSearchTypeInNamespace [] pName
  prefixSearch lim toks = do
    let (name:rest@(context:ns)) = reverse toks
    a <- searchWithLimit (Just lim) $
        prefixSearchTypeInNamespace rest name
    b <- searchWithLimit (Just lim) $
        prefixSearchInContainerOrEnum ns context name
    return $ a ++ b

searchTypeInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchTypeInNamespace = searchTypeInNamespace_ False

searchFunctionInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchFunctionInNamespace = searchFunctionInNamespace_ False

searchModule :: Text -> Angle (ResultLocation Hack.Declaration)
searchModule = searchModule_ False

searchGlobalConstInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchGlobalConstInNamespace = searchGlobalConstInNamespace_ False

prefixSearchTypeInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchTypeInNamespace = searchTypeInNamespace_ True

prefixSearchFunctionInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchFunctionInNamespace = searchFunctionInNamespace_ True

prefixSearchModule
  :: Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchModule = searchModule_ True

prefixSearchGlobalConstInNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchGlobalConstInNamespace = searchGlobalConstInNamespace_ True

searchInContainerOrEnum
  :: [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
searchInContainerOrEnum = searchInContainerOrEnum_ False

prefixSearchInContainerOrEnum
  :: [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchInContainerOrEnum = searchInContainerOrEnum_ True

searchFunctionInNamespace_
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchFunctionInNamespace_ isPrefix ns name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchFunctionInNamespace (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"namespace_" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

searchModule_
  :: Bool -> Text -> Angle (ResultLocation Hack.Declaration)
searchModule_ isPrefix name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchModule (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

searchGlobalConstInNamespace_
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchGlobalConstInNamespace_ isPrefix ns name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchGlobalConstInNamespace (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"namespace_" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

searchTypeInNamespace_
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchTypeInNamespace_ isPrefix ns name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchTypeInNamespace (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"namespace_" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

prefixSearchNamespace
  :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchNamespace = searchNamespace_ True

searchNamespace :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchNamespace = searchNamespace_ False

searchNamespace_
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchNamespace_ isPrefix ns name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl,file,rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchNamespace (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"namespace_" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

searchInContainerOrEnum_
  :: Bool -> [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
searchInContainerOrEnum_ isPrefix ns context name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl, file, rangespan, lname) `where_` [
      wild .= predicate @Hack.SearchInContainerOrEnum (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"contextName" (string context) $
          field @"contextNamespace" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan lname
    ]

namespaceQName :: [Text] -> Angle (Maybe Hack.NamespaceQName)
namespaceQName [] = nothing
namespaceQName (n:ns) = just $ predicate $
  rec $
    field @"name" (string n) $
    field @"parent" (namespaceQName ns)
  end

stringOrPrefix :: Bool -> Text -> Angle Text
stringOrPrefix isPrefix
  | isPrefix = stringPrefix
  | otherwise = string

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
  symbolSearch t@[name] = runSearch t $ searchInNamespace [] name
  symbolSearch t@(_:_) = do -- 2 or more path elements
    let (name:rest@(context:ns)) = reverse t

    a <- runSearch t $ searchInNamespace rest name
    b <- runSearch t $ searchInContainerOrEnum ns context name

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
    where mapFst f (x, y, z) = (f x, y, z)

instance PrefixSearch Hack.Declaration where
  prefixSearch lim [] = prefixSearch lim [""]
  prefixSearch lim [pName] = searchWithLimit (Just lim) $
    prefixSearchInNamespace [] pName
  prefixSearch lim toks = do
    let (name:rest@(context:ns)) = reverse toks
    a <- searchWithLimit (Just lim) $
        prefixSearchInNamespace rest name
    b <- searchWithLimit (Just lim) $
        prefixSearchInContainerOrEnum ns context name
    return $ a ++ b

searchInNamespace :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchInNamespace = searchInNamespace_ False

prefixSearchInNamespace :: [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchInNamespace = searchInNamespace_ True

searchInContainerOrEnum
  :: [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
searchInContainerOrEnum = searchInContainerOrEnum_ False

prefixSearchInContainerOrEnum
  :: [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
prefixSearchInContainerOrEnum = searchInContainerOrEnum_ True

searchInNamespace_
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Hack.Declaration)
searchInNamespace_ isPrefix ns name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl,file,rangespan) `where_` [
      wild .= predicate @Hack.SearchInNamespace (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"namespace_" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan
    ]

searchInContainerOrEnum_
  :: Bool -> [Text] -> Text -> Text -> Angle (ResultLocation Hack.Declaration)
searchInContainerOrEnum_ isPrefix ns context name =
  vars $ \(decl :: Angle Hack.Declaration) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl, file, rangespan) `where_` [
      wild .= predicate @Hack.SearchInContainerOrEnum (
        rec $
          field @"name" (stringOrPrefix isPrefix name) $
          field @"contextName" (string context) $
          field @"contextNamespace" (namespaceQName ns) $
          field @"decl" decl
        end),
      entityLocation (alt @"hack" (alt @"decl" decl)) file rangespan
    ]

namespaceQName :: [Text] -> Angle (Maybe Hack.NamespaceQName_key)
namespaceQName [] = nothing
namespaceQName (n:ns) = just $
  rec $
    field @"name" (string n) $
    field @"parent" (namespaceQName ns)
  end

stringOrPrefix :: Bool -> Text -> Angle Text
stringOrPrefix isPrefix
  | isPrefix = stringPrefix
  | otherwise = string

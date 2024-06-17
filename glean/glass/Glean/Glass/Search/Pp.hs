{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo, PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Pp
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Glass.Search.Class
import Glean.Angle as Angle
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodePp.Types as Pp
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Pp1.Types as Pp
import qualified Glean.Schema.Src.Types as Src

breakPathAndName :: [Text] -> Maybe (Path, Name)
breakPathAndName [] = Nothing
breakPathAndName [_name] = Nothing -- has to be a file and a name
breakPathAndName toks@(_:_) = Just (Path path, Name name)
  where
    path = joinFragments (init toks)
    name = last toks

data Query
  = Define !Path !Name
  | Undef !Path !Name
  | Include !Path

newtype Path = Path Text
newtype Name = Name Text

instance Search (ResultLocation Pp.Entity) where
  symbolSearch toks = case toks of
    [] -> return $ None "PP.symbolSearch: empty"
    [_] -> return $ None "PP.symbolSearch: singleton: not a symbolid"
    base:rest -> case base of
      "define" -> case breakPathAndName rest of
        Nothing -> return $ None "Pp.symbolSearch: define: missing name"
        Just (path, name) -> searchSymbolId toks $ runQuery (Define path name)
      "undef"  -> case breakPathAndName rest of
        Nothing -> return $ None "Pp.symbolSearch: undef: missing name"
        Just (path, name) -> searchSymbolId toks $ runQuery (Undef path name)
      _ -> searchSymbolId toks $ runQuery (Include (Path (joinFragments toks)))

runQuery :: Query -> Angle (ResultLocation Pp.Entity)
runQuery (Define path name) = searchDefine path name
runQuery (Undef path name) = searchUndef path name
runQuery (Include path) = searchInclude path

searchDefine :: Path -> Name -> Angle (ResultLocation Pp.Entity)
searchDefine (Path path) (Name name) =
  vars $ \(entity :: Angle Pp.Entity) (file :: Angle Src.File)
    (decl :: Angle Pp.Define) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      decl .= predicate @Pp.Define (
        rec $
          field @"macro" (string name) $
          field @"source" (rec $ field @"file" (asPredicate file) end)
        end),
      alt @"define" (asPredicate decl) .= sig entity,
      entityLocation (alt @"pp" entity) file rangespan lname
    ]

searchUndef :: Path -> Name -> Angle (ResultLocation Pp.Entity)
searchUndef (Path path) (Name name) =
  vars $ \(entity :: Angle Pp.Entity) (file :: Angle Src.File)
    (decl :: Angle Pp.Undef) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      decl .= predicate @Pp.Undef (
        rec $
          field @"macro" (string name) $
          field @"source" (rec $ field @"file" (asPredicate file) end)
        end),
      alt @"undef" (asPredicate decl) .= sig entity,
      entityLocation (alt @"pp" entity) file rangespan lname
    ]

searchInclude :: Path -> Angle (ResultLocation Pp.Entity)
searchInclude (Path path) =
  vars $ \(entity :: Angle Pp.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (entity,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @Pp.Include ( -- asserts it is a member of pp.Include
        rec $ field @"file" (asPredicate file) end),
      alt @"include_" (asPredicate file) .= sig entity,
      entityLocation (alt @"pp" entity) file rangespan lname
    ]

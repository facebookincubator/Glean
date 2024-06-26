{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Glean.Glass.Search.Thrift
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Angle as Angle
    ( AngleVars(vars),
      Angle,
      predicate,
      where_,
      (.=),
      string,
      tuple,
      rec,
      alt,
      field,
      end,
      wild,
      asPredicate,
      sig,
      AngleStatement
      )

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Fbthrift.Types as Thrift
import qualified Glean.Schema.CodeFbthrift.Types as Thrift
import qualified Glean.Schema.Src.Types as Src
import Glean.Haxl.Repos as Glean
import Glean.Typed.Binary (Type)
import Data.Typeable (Typeable)

-- Depending on context (e.g. find-references or describe), we want to
--  decode a symbol id into an entity with our without its location.
--
--  This module defines the corresponding Search instances and Angle queries

instance Search (ResultLocation Thrift.Entity) where
  symbolSearch toks =
    fmap (mapResultLocation Thrift.Entity_decl) <$> symbolSearch toks

instance Search Thrift.Entity where
  symbolSearch toks = fmap Thrift.Entity_decl <$> symbolSearch toks

instance Search (ResultLocation Thrift.Declaration) where
  symbolSearch = symbolSearchGen searchQNameWithLoc searchFunctionNameWithLoc
    searchThriftFileWithLoc searchFieldDeclWithLoc

instance Search Thrift.Declaration where
  symbolSearch =
    symbolSearchGen searchQName searchFunctionName searchThriftFile
      searchFieldDecl

-- Resolve symbol id tokens into an entity, with or without location.
-- Thrift symbol ids are ambiguous and it may take more than one query
-- to resolve them.
symbolSearchGen ::
  (Typeable t, Show t, Glean.Typed.Binary.Type t, QueryRepos u)
  => (Text -> Text -> Angle t)
  -> (Text -> Text -> Text -> Angle t)
  -> (Text -> Angle t)
  -> (Text -> Text -> Text -> Angle t)
  -> [Text]
  -> ReposHaxl u v (SearchResult t)
symbolSearchGen
  searchQName searchFunctionName searchThriftFile searchFieldDecl toks =
  case toks of
  [] -> return $ None "Thrift.symbolSearch: empty query"
  _ -> case (init toks, last toks) of
    (pieces, name) -> do
      let path = joinFragments pieces
      result <- searchSymbolId toks $ searchQName path name
      case result of
        None{} -> case (init pieces, last pieces) of
          (morePieces, serviceName) -> do
            let path = joinFragments morePieces
            moreResult <- searchSymbolId toks $ searchFunctionName
                path serviceName name
            case moreResult of
              None{} -> do
                moreResult <- searchSymbolId toks $ searchFieldDecl
                  path serviceName name
                case moreResult of
                  None{} ->
                    searchSymbolId toks $ searchThriftFile (joinFragments toks)
                  r -> return r
              r -> return r
        r -> return r

functionNameDecl
  :: Text -> Text -> Text -> Angle Src.File -> Angle Thrift.File
  -> Angle Thrift.QualName -> Angle Thrift.XRefTarget -> [AngleStatement]
functionNameDecl path servicename name file thriftFile qname decl =
    [ file .= predicate @Src.File (string path),
    thriftFile .= predicate @Thrift.File file,
    qname .= predicate @Thrift.QualName (
      rec $
        field @"file" (asPredicate thriftFile) $
        field @"name" (string servicename)
      end
    ),
    wild .= predicate @Thrift.FunctionDeclarationName (
      rec $
        field @"qname" (asPredicate qname) $
        field @"name" (string name) $
        field @"decl" decl
      end)
  ]

searchFunctionName
  :: Text -> Text -> Text -> Angle Thrift.Declaration
searchFunctionName path servicename name =
  vars $ \(file :: Angle Src.File) (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->
  decl `where_`
    functionNameDecl path servicename name file thriftFile qname decl

searchFunctionNameWithLoc
  :: Text -> Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchFunctionNameWithLoc path servicename name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_`
  (functionNameDecl path servicename name file thriftFile qname decl
  ++ [
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ])

thriftFileDecl
  :: Text -> Angle Src.File -> Angle Thrift.File -> Angle Thrift.Declaration
  -> [AngleStatement]
thriftFileDecl path file thriftFile decl =
    [
      file .= predicate @Src.File (string path),
      thriftFile .= predicate @Thrift.File file,
      decl .= sig (alt @"include_"
              (asPredicate thriftFile) :: Angle Thrift.Declaration)
    ]

searchThriftFileWithLoc :: Text -> Angle (ResultLocation Thrift.Declaration)
searchThriftFileWithLoc path = vars $ \(file :: Angle Src.File)
  (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration)
       (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl, file, rangespan, lname) `where_` (
    thriftFileDecl path file thriftFile decl ++ [
      entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
    ])

searchThriftFile :: Text -> Angle Thrift.Declaration
searchThriftFile path = vars $ \(file :: Angle Src.File)
  (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->
    decl `where_` thriftFileDecl path file thriftFile decl

thriftFieldDecl
  :: Text -> Text -> Text -> Angle Src.File -> Angle Thrift.File
  -> Angle Thrift.QualName -> Angle Thrift.FieldDecl -> Angle Thrift.XRefTarget
  -> [AngleStatement]
thriftFieldDecl path typename name file thriftFile qname fieldDecl decl =
    [ file .= predicate @Src.File (string path),
    thriftFile .= predicate @Thrift.File file,
    qname .= predicate @Thrift.QualName (
      rec $
        field @"file" (asPredicate thriftFile) $
        field @"name" (string typename)
      end
    ),
    fieldDecl .= predicate @Thrift.FieldDecl (
      rec $
        field @"qname" (asPredicate qname) $
        field @"name" (string name)
      end),
    decl .= sig (alt @"field"
            (asPredicate fieldDecl) :: Angle Thrift.Declaration)
  ]

searchFieldDecl
  :: Text -> Text -> Text -> Angle Thrift.Declaration
searchFieldDecl path typename name =
  vars $ \(file :: Angle Src.File) (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (fieldDecl' :: Angle Thrift.FieldDecl)
     (decl :: Angle Thrift.Declaration) ->
  decl `where_`
    thriftFieldDecl path typename name file thriftFile qname fieldDecl' decl

searchFieldDeclWithLoc
  :: Text -> Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchFieldDeclWithLoc path typename name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (fieldDecl' :: Angle Thrift.FieldDecl)
     (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_`
  (thriftFieldDecl path typename name file thriftFile qname fieldDecl' decl
  ++ [
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ])


qNameDecl
  :: Text -> Text -> Angle Thrift.File -> Angle Thrift.QualName
  -> Angle Src.File -> Angle Thrift.XRefTarget -> [AngleStatement]
qNameDecl path name thriftFile qname file decl =
  [ file .= predicate @Src.File (string path),
  thriftFile .= predicate @Thrift.File file,
  qname .= predicate @Thrift.QualName (
    rec $
      field @"file" (asPredicate thriftFile) $
      field @"name" (string name)
    end
  ),
  wild .= predicate @Thrift.DeclarationName (
    rec $
      field @"qname" (asPredicate qname) $
      field @"decl" decl
    end)]

-- A basic entity lookup:  this will find named decls, exceptions, constants and
-- service names, which are indexd by QualName
-- The separate thrift.File fact is slightly annoying.
searchQNameWithLoc :: Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchQNameWithLoc path name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_`
    (qNameDecl path name thriftFile qname file decl ++ [
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ])

searchQName :: Text -> Text -> Angle Thrift.Declaration
searchQName path name =
  vars $ \(file :: Angle Src.File) (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->
  decl `where_` qNameDecl path name thriftFile qname file decl

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Pretty.Fbthrift
  (
    prettyFbthriftSignature
  ) where

import System.FilePath ( dropExtension, takeBaseName )
import Data.Text ( Text, unpack )
import Compat.Prettyprinter

import Glean.Glass.Types ( SymbolId(..), RepoName(..) )
import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import Glean.Schema.CodeFbthrift.Types as Fbthrift ( Entity(..) )
import qualified Glean.Schema.Fbthrift.Types as Fbthrift
import qualified Glean.Schema.Src.Types as Src

-- Type of Fbthrift entity signatures to capture the the subset we generate
-- We use this to separate the processing of the Glean type from the text
-- we wish to generate

newtype File = File Text

newtype Identifier = Identifier Text

data NamedKind = Typedef | Enum | Struct | Union

data FieldKind = FUnion | FStruct | FException

data QualName = QualName File Identifier

data Declaration
  = IncludeFile File
  | Named QualName NamedKind -- user declared type
  | Service QualName
  | Exception QualName
  | Constant QualName
  | Function QualName Identifier
  | EnumValue QualName NamedKind Identifier
  | FieldDecl QualName FieldKind Identifier

-- The returned DocStream is annotated with SymbolId (links from sig
-- tokens to their definition). This isn't supported yet for thrift.
-- It'll be always Nothing, and () in the returned Doc types.
prettyFbthriftSignature
  :: LayoutOptions
  -> RepoName
  -> SymbolId
  -> Fbthrift.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyFbthriftSignature opts _repo sym (Fbthrift.Entity_decl d) =
    do
      mDecl <- fromAngleDeclaration d
      case mDecl of
        Nothing -> return Nothing
        Just decl ->
          let doc = layoutSmart opts $ prettyDecl opts sym decl
              annDoc = reAnnotateS (\() -> Nothing) doc in
          return $ Just annDoc
prettyFbthriftSignature _ _ _ Fbthrift.Entity_EMPTY = return Nothing

ppKind :: NamedKind -> Doc ()
ppKind kind = case kind of
  Typedef -> "typedef"
  Enum -> "enum"
  Struct -> "struct"
  Union -> "union"

ppFieldKind :: FieldKind -> Doc ()
ppFieldKind kind = case kind of
  FUnion -> "union"
  FStruct -> "struct"
  FException -> "exception"

ppFile :: File -> Doc ()
ppFile (File f) = pretty f

ppQualName :: QualName -> Doc ()
ppQualName (QualName (File file) (Identifier id)) =
  pretty (dropExtension (takeBaseName (unpack file)) <> "." <> unpack id)

ppScopedId :: QualName -> Identifier -> Doc ()
ppScopedId (QualName (File file) (Identifier idService)) (Identifier id) =
  pretty $ mconcat [
    dropExtension (takeBaseName (unpack file)), ".", unpack idService, "::",
    unpack id
  ]

prettyDecl :: LayoutOptions -> SymbolId -> Declaration -> Doc ()
prettyDecl _ _sym (Service qName ) = "service" <+> ppQualName qName
prettyDecl _ _sym (Exception qName ) = "exception" <+> ppQualName qName
prettyDecl _ _sym (Constant qName ) = "constant" <+> ppQualName qName
prettyDecl _ _sym (IncludeFile file) = "include" <+> ppFile file
prettyDecl _ _sym (Named qName kind) = ppKind kind <+> ppQualName qName
prettyDecl _ _sym (Function qName id) = "function" <+> ppScopedId qName id
prettyDecl _ _sym (EnumValue qName _kd id) = "enum value" <+> ppScopedId qName id
prettyDecl _ _sym (FieldDecl qName kind id) =
  ppFieldKind kind <+> "field" <+> ppScopedId qName id

fromFile :: Src.File -> Glean.RepoHaxl u w File
fromFile file = File <$> Glean.keyOf file

fromKind :: Fbthrift.NamedKind -> NamedKind
fromKind kind = case kind of
  Fbthrift.NamedKind_typedef_ -> Typedef
  Fbthrift.NamedKind_enum_ -> Enum
  Fbthrift.NamedKind_struct_ -> Struct
  Fbthrift.NamedKind_union_ -> Union
  Fbthrift.NamedKind__UNKNOWN _ -> error "unexpected NamedKind"

fromFieldKind :: Fbthrift.FieldKind -> FieldKind
fromFieldKind kind = case kind of
  Fbthrift.FieldKind_exception_ -> FException
  Fbthrift.FieldKind_struct_ -> FStruct
  Fbthrift.FieldKind_union_ -> FUnion
  Fbthrift.FieldKind__UNKNOWN _ -> error "unexpected FieldKind"

fromQualName :: Fbthrift.QualName_key -> Glean.RepoHaxl u w QualName
fromQualName (Fbthrift.QualName_key file id) = do
  id <- Identifier <$> Glean.keyOf id
  file <- fromFile =<< Glean.keyOf file
  return $ QualName file id

fromNamedDecl :: Fbthrift.NamedDecl_key -> Glean.RepoHaxl u w Declaration
fromNamedDecl (Fbthrift.NamedDecl_key namedType) = do
  let Fbthrift.NamedType qName kind = namedType
  qName <- fromQualName =<< Glean.keyOf qName
  return $ Named qName (fromKind kind)

fromExceptionName
 :: Fbthrift.ExceptionName_key -> Glean.RepoHaxl u w Declaration
fromExceptionName  (Fbthrift.ExceptionName_key qName) = do
  qName <- fromQualName =<< Glean.keyOf qName
  return $ Exception qName

fromServiceName :: Fbthrift.ServiceName_key -> Glean.RepoHaxl u w Declaration
fromServiceName (Fbthrift.ServiceName_key qName) = do
  qName <- fromQualName =<< Glean.keyOf qName
  return $ Service qName

fromConstant :: Fbthrift.Constant_key -> Glean.RepoHaxl u w Declaration
fromConstant (Fbthrift.Constant_key qName) = do
  qName <- fromQualName =<< Glean.keyOf qName
  return $ Constant qName

fromEnumValue :: Fbthrift.EnumValue_key -> Glean.RepoHaxl u w Declaration
fromEnumValue (Fbthrift.EnumValue_key enum name) = do
  let Fbthrift.NamedType qname kind = enum
  qualName <- fromQualName =<< Glean.keyOf qname
  id <- Identifier <$> Glean.keyOf name
  return $ EnumValue qualName (fromKind kind) id

fromFieldDecl :: Fbthrift.FieldDecl_key -> Glean.RepoHaxl u w Declaration
fromFieldDecl (Fbthrift.FieldDecl_key qname kind name) = do
  qualName <- fromQualName =<< Glean.keyOf qname
  id <- Identifier <$> Glean.keyOf name
  return $ FieldDecl qualName (fromFieldKind kind) id

fromFunctionName :: Fbthrift.FunctionName_key -> Glean.RepoHaxl u w Declaration
fromFunctionName (Fbthrift.FunctionName_key service name) = do
  Fbthrift.ServiceName_key serviceName <- Glean.keyOf service
  qName <- fromQualName =<< Glean.keyOf serviceName
  id <- Identifier <$> Glean.keyOf name
  return $ Function qName id

--
-- Process the raw declaration result from Glean to build up a cleaner
-- `Declaration` description of the type signature, possibly pulling in
-- some related information as we go
--
fromAngleDeclaration
  :: Fbthrift.Declaration -> Glean.RepoHaxl u w (Maybe Declaration)
fromAngleDeclaration d = case d of
  Fbthrift.XRefTarget_include_ file ->
    Just . IncludeFile <$> (fromFile =<< Glean.keyOf file)
  Fbthrift.XRefTarget_named namedDecl ->
    Just <$> (fromNamedDecl =<< Glean.keyOf namedDecl)
  Fbthrift.XRefTarget_exception_ exceptionName  ->
    Just <$> (fromExceptionName =<< Glean.keyOf exceptionName)
  Fbthrift.XRefTarget_service_ serviceName ->
    Just <$> (fromServiceName =<< Glean.keyOf serviceName)
  Fbthrift.XRefTarget_constant constant ->
    Just <$> (fromConstant =<< Glean.keyOf constant)
  Fbthrift.XRefTarget_enumValue enumValue ->
    Just <$> (fromEnumValue =<< Glean.keyOf enumValue)
  Fbthrift.XRefTarget_function_ functionName  ->
    Just <$> (fromFunctionName =<< Glean.keyOf functionName)
  Fbthrift.XRefTarget_field fieldDecl  ->
    Just <$> (fromFieldDecl =<< Glean.keyOf fieldDecl)
  Fbthrift.XRefTarget_EMPTY -> return Nothing

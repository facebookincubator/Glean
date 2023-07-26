{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo #-}

module Glean.Glass.Pretty.Java
  (
    prettyJavaSignature
  ) where

import Data.Maybe
-- import Data.Map.Strict ( Map )
-- import qualified Data.Map.Strict as Map
import Data.Text ( Text )
import Data.Text.Prettyprint.Doc
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad ( (<=<) )
-- import Util.List ( uniq )

import Glean.Angle as Angle hiding (Type)
import Glean.Glass.Path ( fromGleanPath )
import Glean.Glass.Base ( GleanPath(..) )
import Glean.Glass.Types ( SymbolId(..), RepoName(..) )
import Glean.Glass.Utils
import Glean.Glass.SymbolId ( toSymbolId )
import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Code.Types as Code
-- import qualified Glean.Schema.Codemarkup.Types as Code

import qualified Glean.Schema.CodeJava.Types as Java (Entity(..))
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as JavaKotlin
import qualified Glean.Schema.JavaAlpha.Types as JavaKotlin

-- import qualified Debug.Trace as Debug

-- Java signatures
data Declaration
  = Class {
      modifiers :: [Java.Modifier],
      clsName :: !Name,
      typeParams :: [TypeParam]
    }
  | Interface {
      modifiers :: [Java.Modifier],
      ifaceName :: !Name,
      typeParams :: [TypeParam]
    }
  | Enum {
      modifiers :: [Java.Modifier],
      enumName :: !Name
    }
  | Method {
      modifiers :: [Java.Modifier],
      methName :: !Name,
      params :: [Parameter],
      returnTy :: Maybe Type,
      throwTys :: [Type]
    }

-- names
newtype Name = Name Text

data Parameter =
  Parameter {
    pName :: !Name,
    pType :: Maybe Type
  }

data Type
  = Type Name (Maybe (Java.Declaration, GleanPath))
  | ArrayType Type

-- types
data TypeParam = TypeParam {
    tyName :: Name,
    tyExtends :: [Type]
  }

data Ann
  = None
  | BareDecl !Java.Declaration !GleanPath
  | SymId !SymbolId

--
-- Generate a nicely formatted Java signature with xrefs from a Java entity
--
prettyJavaSignature
  :: LayoutOptions
  -> RepoName
  -> SymbolId
  -> Java.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyJavaSignature opts repo sym (Java.Entity_decl decl) = runMaybeT $ do
    def <- maybeT $ fmap (pprDeclaration sym) <$> fromAngleDeclaration decl
    maybeT $ Just <$> sequence (annotateDocs def)
  where
    annotateDocs doc = reAnnotateS (declToSymbolId repo) (layoutSmart opts doc)
prettyJavaSignature _ _ _ Java.Entity_EMPTY = return Nothing

declToSymbolId :: RepoName -> Ann -> Glean.RepoHaxl u w (Maybe SymbolId)
declToSymbolId _repo None = return Nothing
declToSymbolId _repo (SymId symId) = return (Just symId)
declToSymbolId repo (BareDecl decl filepath) = Just <$>
    toSymbolId (fromGleanPath repo filepath) entity
  where
    entity = Code.Entity_java (Java.Entity_decl decl)

fromAngleDeclaration
  :: Java.Declaration -> Glean.RepoHaxl u w (Maybe Declaration)
fromAngleDeclaration def = case def of
{-
  Java.Declaration_param e -> Just <$> (fromParamDeclaration =<< Glean.keyOf e)
  Java.Declaration_local e -> Just <$> (fromLocalDeclaration =<< Glean.keyOf e)
  Java.Declaration_field e -> Just <$> (fromFieldDeclaration =<< Glean.keyOf e)
  Java.Declaration_ctor e -> Just <$> (fromCtorDeclaration =<< Glean.keyOf e)
-}
  Java.Declaration_method e -> Just <$>
    (fromMethodDeclaration =<< Glean.keyOf e)
  Java.Declaration_interface_ e ->
    Just <$> (fromInterfaceDeclaration =<< Glean.keyOf e)
  Java.Declaration_class_ e -> Just <$> (fromClassDeclaration =<< Glean.keyOf e)
  Java.Declaration_enum_ e -> Just <$> (fromEnumDeclaration =<< Glean.keyOf e)
  _ -> pure Nothing

fromMName :: JavaKotlin.MethodName -> Glean.RepoHaxl u w Name
fromMName mname = do
  JavaKotlin.MethodName_key qname _sig <- Glean.keyOf mname
  fromQName qname

fromQName :: JavaKotlin.QName -> Glean.RepoHaxl u w Name
fromQName qname = do
  JavaKotlin.QName_key nameFact _ctx <- Glean.keyOf qname
  nameStr <- Glean.keyOf nameFact
  return (Name nameStr)

fromTypeParams :: [Java.TypeParam] -> Glean.RepoHaxl u w [TypeParam]
fromTypeParams tys = mapM (fromTypeParam <=< Glean.keyOf) tys

fromTypeParam :: Java.TypeParam_key -> Glean.RepoHaxl u w TypeParam
fromTypeParam Java.TypeParam_key{..} = do
  tyName <- Name <$> Glean.keyOf typeParam_key_name
  tyExtends <- catMaybes <$>
    mapM (fromType <=< Glean.keyOf) typeParam_key_extends_
  return TypeParam{..}

-- todo: type args and interop types
fromType :: Java.Type_key -> Glean.RepoHaxl u w (Maybe Type)
fromType Java.Type_key{..} = do
  case type_key_baseType of
    Java.BaseType_object oTy -> do
      Java.ObjectType_key{..} <- Glean.keyOf oTy
      name <- fromQName objectType_key_type
      mDecl <- fetchDataRecursive $
        qnameToDecl (Glean.getId objectType_key_type)
      anns <- case mDecl of
        Nothing -> pure Nothing
        Just (decl, srcFile) -> do
          file <- GleanPath <$> Glean.keyOf srcFile
          return (Just (decl, file))
      return (Just (Type name anns))
    Java.BaseType_primitive pTy  -> do
      Java.PrimitiveType_key{..} <- Glean.keyOf pTy
      return (Just (Type (Name primitiveType_key_type) Nothing))
    Java.BaseType_variable vTy -> do
      Java.TypeVar_key{..} <- Glean.keyOf vTy
      nameStr <- Name <$> Glean.keyOf typeVar_key_type
      return (Just (Type nameStr Nothing))
    Java.BaseType_array aTy -> do
      Java.ArrayType_key{..} <- Glean.keyOf aTy
      typeFact <- Glean.keyOf arrayType_key_contents
      ty <- fromType typeFact
      return $ case ty of
        Nothing -> Nothing
        Just ty -> Just (ArrayType ty)
    JavaKotlin.BaseType_EMPTY{} ->
      return Nothing

fromMethodDeclaration
  :: Java.MethodDeclaration_key -> Glean.RepoHaxl u w Declaration
fromMethodDeclaration Java.MethodDeclaration_key{..} = do
  let modifiers = methodDeclaration_key_modifiers
  methName <- fromMName methodDeclaration_key_name
  params <- mapM (fromParamDeclaration <=< Glean.keyOf)
    methodDeclaration_key_parameters
  returnTy <- fromType =<< Glean.keyOf methodDeclaration_key_returnType
  throwTys <- catMaybes <$> mapM (fromType <=< Glean.keyOf)
    methodDeclaration_key_throws_
  return Method {..}

fromParamDeclaration
  :: Java.ParameterDeclaration_key -> Glean.RepoHaxl u w Parameter
fromParamDeclaration Java.ParameterDeclaration_key{..} = do
  pName <- fromQName parameterDeclaration_key_name
  pType <- fromType =<< Glean.keyOf parameterDeclaration_key_type
  return Parameter {..}

fromClassDeclaration
  :: Java.ClassDeclaration_key -> Glean.RepoHaxl u w Declaration
fromClassDeclaration Java.ClassDeclaration_key{..} = do
  clsName <- fromQName classDeclaration_key_name
  typeParams <- fromTypeParams classDeclaration_key_typeParams
  let modifiers = classDeclaration_key_modifiers
  return Class {..}

fromInterfaceDeclaration
  :: Java.InterfaceDeclaration_key -> Glean.RepoHaxl u w Declaration
fromInterfaceDeclaration Java.InterfaceDeclaration_key{..} = do
  ifaceName <- fromQName interfaceDeclaration_key_name
  typeParams <- fromTypeParams interfaceDeclaration_key_typeParams
  let modifiers = interfaceDeclaration_key_modifiers
  return Interface {..}

fromEnumDeclaration
  :: Java.EnumDeclaration_key -> Glean.RepoHaxl u w Declaration
fromEnumDeclaration Java.EnumDeclaration_key{..} = do
  enumName <- fromQName enumDeclaration_key_name
  let modifiers = enumDeclaration_key_modifiers
  return Enum {..}

pprDeclaration :: SymbolId -> Declaration -> Doc Ann
pprDeclaration self (Class mods name []) =
  hsep (map pprModifier mods) <+>
  "class" <+> annotate (SymId self) (pprName name)
pprDeclaration self (Class mods name tys) =
  hsep (map pprModifier mods) <+>
  "class" <+> annotate (SymId self) (pprName name) <>
    hcat ("<" : punctuate comma (map pprTypeParam tys) ++ [">"])
pprDeclaration self (Interface mods name []) =
  hsep (map pprModifier mods) <+>
  "interface" <+> annotate (SymId self) (pprName name)
pprDeclaration self (Interface mods name tys) =
  hsep (map pprModifier mods) <+>
  "interface" <+> annotate (SymId self) (pprName name) <>
    hcat ("<" : punctuate comma (map pprTypeParam tys) ++ [">"])
pprDeclaration self (Enum mods name) =
  hsep (map pprModifier mods) <+>
  "enum" <+> annotate (SymId self) (pprName name)
pprDeclaration self (Method _mods name params retTy throwTys) =
  maybe emptyDoc pprType retTy <+>
  annotate (SymId self) (pprName name) <> (
      if null params then "()"
        else "(" <> hsep (punctuate comma (map pprParam params)) <> ")"
    ) <> (
      if null throwTys then emptyDoc
        else hang 4 (space <> "throws"
           <+> hsep (punctuate comma (map pprType throwTys)))
    )

pprParam :: Parameter -> Doc Ann
pprParam (Parameter name mty) = maybe emptyDoc pprType mty <+> pprName name

pprModifier :: Java.Modifier -> Doc Ann
pprModifier m = case m of
  Java.Modifier_abstract_ -> "abstract"
  Java.Modifier_default_ -> "default"
  Java.Modifier_final_ -> "final"
  Java.Modifier_native_ -> "native"
  Java.Modifier_private_ -> "private"
  Java.Modifier_protected_ -> "protected"
  Java.Modifier_public_ -> "public"
  Java.Modifier_static_ -> "static"
  Java.Modifier_strictfp_ -> "strictfp"
  Java.Modifier_synchronized_ -> "synchronized"
  Java.Modifier_transient_ -> "transient"
  Java.Modifier_volatile_ -> "volatile"
  Java.Modifier__UNKNOWN{} -> emptyDoc

-- grab the symbol id here and we can xref it
pprTypeParam :: TypeParam -> Doc Ann
pprTypeParam (TypeParam nm []) = pprName nm
pprTypeParam (TypeParam nm exts) = pprName nm <+> "extends" <+>
  hsep (punctuate comma (map pprType exts))

pprType :: Type -> Doc Ann
pprType (Type nm Nothing) = pprName nm
pprType (Type nm (Just (decl, file))) = annotate (BareDecl decl file) $
  pprName nm
pprType (ArrayType nm) = "[" <> pprType nm <> "]"

pprName :: Name -> Doc Ann
pprName (Name name) = pretty name

qnameToDecl :: Glean.IdOf JavaKotlin.QName -> Angle (Java.Declaration, Src.File)
qnameToDecl qnameId = vars $ \(decl :: Angle Java.Declaration)
  (file :: Angle Src.File) ->
      tuple (decl, file) `where_` [
        wild .= predicate @Java.QNameToDefinition (
          rec $
            field @"name" (asPredicate (factId qnameId)) $
            field @"defn" decl
          end
        ),
        wild .= predicate @Java.DeclarationLocation (
          rec $
            field @"decl" decl $
            field @"file" (asPredicate file)
          end
        )
      ]

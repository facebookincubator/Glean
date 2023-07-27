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
import Data.Text ( Text )
import Data.Text.Prettyprint.Doc
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad ( (<=<) )

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

import qualified Glean.Schema.CodeJava.Types as Java (Entity(..))
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as JavaKotlin
import qualified Glean.Schema.JavaAlpha.Types as JavaKotlin

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
  | CTor {
      modifiers :: [Java.Modifier],
      parent :: Type,
      params :: [Parameter],
      throwTys :: [Type]
    }
  | Field {
      modifiers :: [Java.Modifier],
      fieldName :: !Name,
      fieldTy :: Maybe Type
    }

-- names
newtype Name = Name Text

data Parameter =
  Parameter {
    pName :: !Name,
    pType :: Maybe Type,
    pAnns :: [Annotation]
  }

newtype Annotation = Annotation Name

data Type
  = Type {
      typeName :: Name,
      typeArgs :: [TypeArg],
      typeXRefs :: Maybe (Java.Declaration, GleanPath)
    }
  | ArrayType Type

data TypeArg = TypeArg Type | TypeArgWild WildCardType

data WildCardType
  = Extends Type
  | Super Type
  | UnboundedWildcard

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
-}
  Java.Declaration_field e -> Just <$> (fromFieldDeclaration =<< Glean.keyOf e)
  Java.Declaration_ctor e -> Just <$> (fromCtorDeclaration =<< Glean.keyOf e)
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

fromTypeArg :: Java.TypeArg_key -> Glean.RepoHaxl u w (Maybe TypeArg)
fromTypeArg (Java.TypeArg_key_type ty) = do
  theTy <- fromType =<< Glean.keyOf ty
  return (TypeArg <$> theTy)
fromTypeArg (Java.TypeArg_key_wildcard card) = case card of
  Java.Wildcard_extends_ ty -> do
    mty <- fromType =<< Glean.keyOf ty
    return (TypeArgWild . Extends <$> mty)
  Java.Wildcard_super_ ty -> do
    mty <- fromType =<< Glean.keyOf ty
    return (TypeArgWild . Super <$> mty)
  Java.Wildcard_unbounded _b -> pure (Just (TypeArgWild UnboundedWildcard))
  Java.Wildcard_EMPTY{} -> pure Nothing
fromTypeArg Java.TypeArg_key_EMPTY{} = pure Nothing

-- todo: type args and interop types
fromType :: Java.Type_key -> Glean.RepoHaxl u w (Maybe Type)
fromType Java.Type_key{..} = do
  typeArgs <- catMaybes <$> mapM (fromTypeArg <=< Glean.keyOf) type_key_typeArgs
  case type_key_baseType of
    Java.BaseType_object oTy -> do
      Java.ObjectType_key{..} <- Glean.keyOf oTy
      typeName <- fromQName objectType_key_type
      mDecl <- fetchDataRecursive $
        qnameToDecl (Glean.getId objectType_key_type)
      typeXRefs <- case mDecl of
        Nothing -> pure Nothing
        Just (decl, srcFile) -> do
          file <- GleanPath <$> Glean.keyOf srcFile
          return (Just (decl, file))
      return (Just Type{..})
    Java.BaseType_primitive pTy  -> do
      Java.PrimitiveType_key{..} <- Glean.keyOf pTy
      return (Just (Type (Name primitiveType_key_type) typeArgs Nothing))
    Java.BaseType_variable vTy -> do
      Java.TypeVar_key{..} <- Glean.keyOf vTy
      nameStr <- Name <$> Glean.keyOf typeVar_key_type
      return (Just (Type nameStr typeArgs Nothing))
    Java.BaseType_array aTy -> do
      Java.ArrayType_key{..} <- Glean.keyOf aTy
      typeFact <- Glean.keyOf arrayType_key_contents
      ty <- fromType typeFact
      return $ case ty of
        Nothing -> Nothing
        Just ty -> Just (ArrayType ty)
    JavaKotlin.BaseType_EMPTY{} ->
      return Nothing

fromFieldDeclaration
  :: Java.FieldDeclaration_key -> Glean.RepoHaxl u w Declaration
fromFieldDeclaration Java.FieldDeclaration_key{..} = do
  let modifiers = fieldDeclaration_key_modifiers
  fieldName <- fromQName fieldDeclaration_key_name
  fieldTy <- fromType =<< Glean.keyOf fieldDeclaration_key_type
  return Field {..}

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

fromCtorDeclaration
  :: Java.ConstructorDeclaration_key -> Glean.RepoHaxl u w Declaration
fromCtorDeclaration Java.ConstructorDeclaration_key{..} = do
  let modifiers = constructorDeclaration_key_modifiers

  JavaKotlin.MethodName_key qname _ <-
    Glean.keyOf constructorDeclaration_key_name
  JavaKotlin.QName_key _name path <- Glean.keyOf qname
  JavaKotlin.Path_key base _ <- Glean.keyOf path
  pNameStr <- Name <$> Glean.keyOf base

  mParentDecl <- case constructorDeclaration_key_container of
        Java.Definition_class_ decl -> do
          key <- Glean.keyOf decl
          return $ Just (Java.Declaration_class_ decl
                 ,Java.classDeclaration_key_file key)

        Java.Definition_interface_ decl -> do
          key <- Glean.keyOf decl
          return $ Just (Java.Declaration_interface_ decl
                  ,Java.interfaceDeclaration_key_file key)
        Java.Definition_enum_ decl -> do
          key <- Glean.keyOf decl
          return $ Just (Java.Declaration_enum_ decl
                  ,Java.enumDeclaration_key_file key)

        Java.Definition_EMPTY{} -> pure Nothing

  parent <- Type pNameStr [] <$> case mParentDecl of
        Nothing -> pure Nothing
        Just (decl, srcFile) -> do
          path <- GleanPath <$> Glean.keyOf srcFile
          return (Just (decl, path))

  params <- mapM (fromParamDeclaration <=< Glean.keyOf)
    constructorDeclaration_key_parameters
  throwTys <- catMaybes <$> mapM (fromType <=< Glean.keyOf)
    constructorDeclaration_key_throws_
  return CTor {..}

fromAnn :: Java.Annotation -> Glean.RepoHaxl u w Annotation
fromAnn ann = do
  Java.Annotation_key{..} <- Glean.keyOf ann
  Annotation <$> fromQName annotation_key_name

fromParamDeclaration
  :: Java.ParameterDeclaration_key -> Glean.RepoHaxl u w Parameter
fromParamDeclaration Java.ParameterDeclaration_key{..} = do
  pName <- fromQName parameterDeclaration_key_name
  pType <- fromType =<< Glean.keyOf parameterDeclaration_key_type
  pAnns <- mapM fromAnn parameterDeclaration_key_annotations
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
  annotate (SymId self) (pprName name) <> pprParamList params <> (
      if null throwTys then emptyDoc
        else hang 4 (space <> "throws"
           <+> hsep (punctuate comma (map pprType throwTys)))
    )
pprDeclaration _self (CTor _mods parent params throwTys) =
  pprType parent <> pprParamList params <> (
      if null throwTys then emptyDoc
        else nest 4 (space <> "throws"
           <+> hsep (punctuate comma (map pprType throwTys)))
    )
pprDeclaration self (Field _mods name fieldTy) =
  maybe emptyDoc pprType fieldTy <+>
    annotate (SymId self) (pprName name)

pprParamList :: [Parameter] -> Doc Ann
pprParamList [] = "()"
pprParamList ps
 | length ps < 5 = "(" <> hsep (punctuate comma (map pprParam ps)) <> ")"
 | otherwise = vcat [
      nest 4 (vcat ("(" : punctuate comma (map pprParam ps))),
      ")"
    ]

pprParam :: Parameter -> Doc Ann
pprParam (Parameter name mty []) = maybe emptyDoc pprType mty <+> pprName name
pprParam (Parameter name mty anns) =
  hsep (map pprAnn anns) <+> maybe emptyDoc pprType mty <+> pprName name

pprAnn :: Annotation -> Doc Ann
pprAnn (Annotation n) = "@" <> pprName n

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

pprTypeArg :: TypeArg -> Doc Ann
pprTypeArg (TypeArg ty) = pprType ty
pprTypeArg (TypeArgWild card) = case card of
  Extends ty -> "?" <+> "extends" <+> pprType ty
  Super ty -> "?" <+> "super" <+> pprType ty
  UnboundedWildcard -> "?"

pprType :: Type -> Doc Ann
pprType (Type nm [] Nothing) = pprName nm
pprType (Type nm args Nothing) = pprName nm <>
  hcat ("<" : punctuate comma (map pprTypeArg args) ++ [">"])
pprType (Type nm _args (Just (decl, file))) =
  annotate (BareDecl decl file) $ pprName nm
pprType (ArrayType nm) = pprType nm <> "[]"

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

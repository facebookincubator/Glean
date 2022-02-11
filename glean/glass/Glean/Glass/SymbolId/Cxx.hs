{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Cxx (
    {- instances -}
    cxxEntityDefinitionType
  , cxxEntityKind

  ) where

import Data.Text ( Text, intercalate )

import Glean.Glass.SymbolId.Class

import qualified Glean
import Glean.Angle ( alt, )
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types
import qualified Glean.Glass.Types as Glass
import qualified Glean.Schema.Cxx1.Types as Cxx

import Glean.Schema.CodeCxx.Types as Cxx
    ( Entity(..), Definition(..) )

instance Symbol Cxx.Entity where
  toSymbol e = case e of
    Cxx.Entity_decl decl -> toSymbol decl
    Cxx.Entity_defn defn -> toSymbol defn
    Cxx.Entity_enumerator enum -> toSymbolPredicate enum

instance Symbol Cxx.Declaration where
  toSymbol d = case d of
    Cxx.Declaration_namespace_ x -> toSymbolPredicate x
    Cxx.Declaration_usingDeclaration x -> toSymbolPredicate x
    Cxx.Declaration_usingDirective x -> toSymbolPredicate x
    Cxx.Declaration_record_ x -> toSymbolPredicate x
    Cxx.Declaration_enum_ x -> toSymbolPredicate x
    Cxx.Declaration_function_ x -> toSymbolPredicate x
    Cxx.Declaration_variable x -> toSymbolPredicate x
    Cxx.Declaration_objcContainer x -> toSymbolPredicate x
    Cxx.Declaration_objcMethod x -> toSymbolPredicate x
    Cxx.Declaration_objcProperty x -> toSymbolPredicate x
    Cxx.Declaration_typeAlias x -> toSymbolPredicate x

-- Results of DeclToDef calls
instance Symbol Cxx.Definition where
  toSymbol defn = case defn of
    Cxx.Definition_record_ x -> toSymbolPredicate x
    Cxx.Definition_function_ x -> toSymbolPredicate x
    Cxx.Definition_enum_ x -> toSymbolPredicate x
    Cxx.Definition_objcMethod x -> toSymbolPredicate x
    Cxx.Definition_objcContainer x -> toSymbolPredicate x
    Cxx.Definition_variable x -> toSymbolPredicate x
    Cxx.Definition_namespace_ x -> toSymbolPredicate x

instance Symbol Cxx.RecordDefinition_key where
  toSymbol (Cxx.RecordDefinition_key decl _bases _members) =
    toSymbolPredicate decl

instance Symbol Cxx.FunctionDefinition_key where
  toSymbol (Cxx.FunctionDefinition_key decl _inline) =
    toSymbolPredicate decl

instance Symbol Cxx.EnumDefinition_key where
  toSymbol (Cxx.EnumDefinition_key decl _enums) =
    toSymbolPredicate decl

instance Symbol Cxx.ObjcMethodDeclaration where
  toSymbol decl = Glean.keyOf decl >>= toSymbol

instance Symbol Cxx.NamespaceDefinition_key where
  toSymbol (Cxx.NamespaceDefinition_key decl _members) =
    toSymbolPredicate decl

instance Symbol Cxx.ObjcContainerDefinition_key where
  toSymbol (Cxx.ObjcContainerDefinition_key decl _protocols _members) =
    toSymbolPredicate decl

instance Symbol Cxx.Enumerator_key where
  toSymbol (Cxx.Enumerator_key name decl _) = decl <:> name

instance Symbol Cxx.EnumDeclaration where
  toSymbol e = toSymbolPredicate e

instance Symbol Cxx.NamespaceDeclaration_key where
  toSymbol (Cxx.NamespaceDeclaration_key qname _) = toSymbolPredicate qname

instance Symbol Cxx.UsingDeclaration_key where
  toSymbol (Cxx.UsingDeclaration_key qname _) = toSymbolPredicate qname

instance Symbol Cxx.UsingDirective_key where
  toSymbol (Cxx.UsingDirective_key qname _) = toSymbolPredicate qname

instance Symbol Cxx.RecordDeclaration_key where
  toSymbol (Cxx.RecordDeclaration_key qname _kind _) = toSymbolPredicate qname

instance Symbol Cxx.EnumDeclaration_key where
  toSymbol (Cxx.EnumDeclaration_key qname _is_scoped _type _) =
    toSymbolPredicate qname

instance Symbol Cxx.FunctionDeclaration_key where
  toSymbol (Cxx.FunctionDeclaration_key fqname _sig _todo _) =
     toSymbolPredicate fqname

instance Symbol Cxx.VariableDeclaration_key where
  toSymbol (Cxx.VariableDeclaration_key qname _ty _kind _) =
     toSymbolPredicate qname

instance Symbol Cxx.ObjcContainerDeclaration_key where
  toSymbol (Cxx.ObjcContainerDeclaration_key cid _) = toSymbol cid

instance Symbol Cxx.ObjcMethodDeclaration_key where
  toSymbol (Cxx.ObjcMethodDeclaration_key name cid _ _ _ _ _) =
    cid <:> name

instance Symbol Cxx.ObjcPropertyDeclaration_key where
  toSymbol (Cxx.ObjcPropertyDeclaration_key name cid _ty _ _ _ _ _) =
    cid <:> name

instance Symbol Cxx.TypeAliasDeclaration_key  where
  toSymbol (Cxx.TypeAliasDeclaration_key name _ _ _) = toSymbolPredicate name

instance Symbol Cxx.NamespaceQName_key where
  toSymbol (Cxx.NamespaceQName_key mname Nothing) = toMaybeName mname
  toSymbol (Cxx.NamespaceQName_key mname (Just ns)) = do
    xs <- toSymbol ns
    x <- toMaybeName mname
    return (xs ++ x)

instance Symbol Cxx.QName_key where
  toSymbol (Cxx.QName_key name scope) = scope <:> name

instance Symbol Cxx.FunctionQName_key where
  toSymbol (Cxx.FunctionQName_key name scope) = scope <:> name

instance Symbol Cxx.NamespaceQName where
  toSymbol k = toSymbolPredicate k

instance Symbol Cxx.FunctionName where
  toSymbol k = toSymbolPredicate k

instance Symbol Cxx.Signature where
  toSymbol k = toSymbolPredicate k

instance Symbol Cxx.Signature_key where
  toSymbol (Cxx.Signature_key ty _params) = toSymbol ty

instance Symbol Cxx.FunctionName_key where
  toSymbol k = case k of
    Cxx.FunctionName_key_name x -> toSymbol x
    Cxx.FunctionName_key_operator_ x -> return [x]
    Cxx.FunctionName_key_literalOperator x -> return [x]
    Cxx.FunctionName_key_constructor _ -> return []
    Cxx.FunctionName_key_destructor _ -> return []
    Cxx.FunctionName_key_conversionOperator x -> toSymbol x

instance Symbol Cxx.Scope where
  toSymbol k = case k of
    Cxx.Scope_global_ _ -> return []
    Cxx.Scope_namespace_ nsqname -> toSymbolPredicate nsqname
    Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ qname _) ->
      toSymbolPredicate qname
    Cxx.Scope_local qname -> toSymbolPredicate qname

instance Symbol Cxx.ObjcContainerId where
  toSymbol k = case k of
    Cxx.ObjcContainerId_protocol x -> toSymbol x
    Cxx.ObjcContainerId_interface_ x -> toSymbol x
    Cxx.ObjcContainerId_categoryInterface x -> toSymbol x
    Cxx.ObjcContainerId_extensionInterface x -> toSymbol x
    Cxx.ObjcContainerId_implementation x -> toSymbol x
    Cxx.ObjcContainerId_categoryImplementation x -> toSymbol x

instance Symbol Cxx.ObjcCategoryId where
  toSymbol (Cxx.ObjcCategoryId n1 n2) = do
    k1 <- Glean.keyOf n1
    k2 <- Glean.keyOf n2
    return [k1,k2] -- which order?

instance Symbol Cxx.ObjcSelector where
  toSymbol k = reverse <$> Glean.keyOf k -- :: [Text] ? order?

instance Symbol Cxx.Type where
  toSymbol k = do
    v <- Glean.keyOf k
    return [v]

-- The cxx schema sometimes uses nothing to represent implicit or anonymous
-- scopes and names. We need to preserve that rather than elide
toMaybeName :: Maybe Cxx.Name -> Glean.RepoHaxl u w [Text]
toMaybeName Nothing = return [""]
toMaybeName (Just name) = toSymbol name

instance Symbol Cxx.Name where
  toSymbol k = do
    v <- Glean.keyOf k
    return [v]

--
-- Search by entity
--
instance ToAngle Cxx.Entity where
  toAngle e = case e of
    Cxx.Entity_decl x -> alt @"decl" (toAngle x)
    Cxx.Entity_defn x -> alt @"defn" (toAngle x)
    Cxx.Entity_enumerator x -> alt @"enumerator" (mkKey x)

instance ToAngle Cxx.Declaration where
  toAngle e = case e of
    Cxx.Declaration_namespace_ x -> alt @"namespace_" (mkKey x)
    Cxx.Declaration_usingDeclaration x -> alt @"usingDeclaration" (mkKey x)
    Cxx.Declaration_usingDirective x -> alt @"usingDirective" (mkKey x)
    Cxx.Declaration_record_ x -> alt @"record_" (mkKey x)
    Cxx.Declaration_enum_ x -> alt @"enum_" (mkKey x)
    Cxx.Declaration_function_ x -> alt @"function_" (mkKey x)
    Cxx.Declaration_variable x -> alt @"variable" (mkKey x)
    Cxx.Declaration_objcContainer x -> alt @"objcContainer" (mkKey x)
    Cxx.Declaration_objcMethod x -> alt @"objcMethod" (mkKey x)
    Cxx.Declaration_objcProperty x -> alt @"objcProperty" (mkKey x)
    Cxx.Declaration_typeAlias x -> alt @"typeAlias" (mkKey x)

instance ToAngle Cxx.Definition where
  toAngle e = case e of
    Cxx.Definition_record_ x -> alt @"record_" (mkKey x)
    Cxx.Definition_function_ x -> alt @"function_" (mkKey x)
    Cxx.Definition_enum_ x -> alt @"enum_" (mkKey x)
    Cxx.Definition_objcMethod x -> alt @"objcMethod" (mkKey x)
    Cxx.Definition_objcContainer x -> alt @"objcContainer" (mkKey x)
    Cxx.Definition_variable x -> alt @"variable" (mkKey x)
    Cxx.Definition_namespace_ x -> alt @"namespace_" (mkKey x)

--
-- Entity "parent" relationship labelling
--

instance ToSymbolParent Cxx.Entity where
  toSymbolParent e = case e of
    Cxx.Entity_decl x -> toSymbolParent x
    Cxx.Entity_defn{} -> return Nothing
    Cxx.Entity_enumerator x -> Glean.keyOf x >>= toSymbolParent

instance ToSymbolParent Cxx.Declaration where
  toSymbolParent e = case e of
    Cxx.Declaration_namespace_ x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_usingDeclaration x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_usingDirective x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_record_ x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_enum_ x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_function_ x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Declaration_variable x-> Glean.keyOf x  >>= toSymbolParent
    Cxx.Declaration_objcContainer{} -> return Nothing -- TODO
    Cxx.Declaration_objcMethod{} -> return Nothing -- TODO
    Cxx.Declaration_objcProperty{} -> return Nothing -- TODO
    Cxx.Declaration_typeAlias x -> Glean.keyOf x >>= toSymbolParent

instance ToSymbolParent Cxx.Enumerator_key where
  toSymbolParent (Cxx.Enumerator_key _name decl _) =
    Glean.keyOf decl >>= cxxEnumDeclParentName

instance ToSymbolParent Cxx.NamespaceDeclaration_key where
  toSymbolParent (Cxx.NamespaceDeclaration_key qname _) =
    Glean.keyOf qname >>= toSymbolParent

instance ToSymbolParent Cxx.UsingDeclaration_key where
  toSymbolParent (Cxx.UsingDeclaration_key qname _) =
    Glean.keyOf qname >>= toSymbolParent

instance ToSymbolParent Cxx.UsingDirective_key where
  toSymbolParent (Cxx.UsingDirective_key qname _) =
    Glean.keyOf qname >>= toSymbolParent

instance ToSymbolParent Cxx.QName_key where
  toSymbolParent (Cxx.QName_key _name scope) = cxxScopeName scope

-- The parent of a qname is the first immediate parent name identifier
instance ToSymbolParent Cxx.NamespaceQName_key where
  toSymbolParent (Cxx.NamespaceQName_key _name mparent) =
    case mparent of
      Nothing -> return Nothing
      Just p -> cxxParentNSName p

instance ToSymbolParent Cxx.FunctionQName_key where
  toSymbolParent (Cxx.FunctionQName_key _name scope) = cxxScopeName scope

instance ToSymbolParent Cxx.RecordDeclaration_key where
  toSymbolParent (Cxx.RecordDeclaration_key qname _kind _) =
    Just <$> cxxParentQName qname

instance ToSymbolParent Cxx.EnumDeclaration_key where
  toSymbolParent (Cxx.EnumDeclaration_key qname _is_scoped _type _) =
    Just <$> cxxParentQName qname

instance ToSymbolParent Cxx.FunctionDeclaration_key where
  toSymbolParent (Cxx.FunctionDeclaration_key fqname _sig _todo _) =
    Glean.keyOf fqname >>= toSymbolParent

instance ToSymbolParent Cxx.VariableDeclaration_key where
  toSymbolParent (Cxx.VariableDeclaration_key qname _ty _kind _) =
    Just <$> cxxParentQName qname

instance ToSymbolParent Cxx.TypeAliasDeclaration_key  where
  toSymbolParent (Cxx.TypeAliasDeclaration_key qname _ _ _) =
    Just <$> cxxParentQName qname

cxxEnumDeclParentName
  :: Cxx.EnumDeclaration_key -> Glean.RepoHaxl u w (Maybe Name)
cxxEnumDeclParentName (Cxx.EnumDeclaration_key qname _scoped _type _) =
  Just <$> cxxParentQName qname

cxxScopeName :: Cxx.Scope -> Glean.RepoHaxl u w (Maybe Name)
cxxScopeName scope = case scope of
  Cxx.Scope_global_ _ -> return Nothing
  Cxx.Scope_namespace_ nsqname -> cxxParentNSName nsqname
  Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ qname _) ->
    Just <$> cxxParentQName qname
  Cxx.Scope_local _fqname -> return Nothing

cxxParentQName :: Cxx.QName -> Glean.RepoHaxl u w Name
cxxParentQName qname = do
  (Cxx.QName_key name _scope) <- Glean.keyOf qname
  cxxNameToName name

cxxParentNSName :: Cxx.NamespaceQName -> Glean.RepoHaxl u w (Maybe Name)
cxxParentNSName nsqname = do
  (Cxx.NamespaceQName_key mname _) <- Glean.keyOf nsqname
  case mname of
    Nothing -> return Nothing
    Just name -> Just <$> cxxNameToName name

cxxNameToName :: Cxx.Name -> Glean.RepoHaxl u w Name
cxxNameToName name = Name <$> Glean.keyOf name

--
-- Tag to distinguish definition entities from declaration entities
--
-- All can be xref targets, but depending on language context we might
-- prefer one or the other
--
cxxEntityDefinitionType :: Cxx.Entity -> Glass.DefinitionKind
cxxEntityDefinitionType e = case e of
  Cxx.Entity_decl{} -> Glass.DefinitionKind_Declaration
  Cxx.Entity_defn{} -> Glass.DefinitionKind_Definition
  Cxx.Entity_enumerator{} -> Glass.DefinitionKind_Definition -- is this ok?

--
-- entity labelling
--
cxxEntityKind :: Cxx.Entity -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
cxxEntityKind e = case e of
  Cxx.Entity_decl x -> toSymbolDeclKind x
  Cxx.Entity_defn x -> toSymbolDefnKind x
  Cxx.Entity_enumerator{} -> return (Just SymbolKind_Enumerator)

-- to match idelsp/GleanLSPConverter.php
toSymbolDeclKind
  :: Cxx.Declaration -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
toSymbolDeclKind e = case e of
  Cxx.Declaration_namespace_{} -> return $ Just SymbolKind_Namespace
  Cxx.Declaration_usingDeclaration{} -> return $ Just SymbolKind_Namespace
  Cxx.Declaration_usingDirective{} -> return Nothing -- unknown
  Cxx.Declaration_record_ x -> toSymbolRecordKind x
  Cxx.Declaration_enum_{} -> return $ Just SymbolKind_Enum
  Cxx.Declaration_function_{} -> return $ Just SymbolKind_Function
  Cxx.Declaration_variable{} -> return $ Just SymbolKind_Variable
  Cxx.Declaration_objcContainer{} -> return Nothing
  Cxx.Declaration_objcMethod{} -> return $ Just SymbolKind_Method
  Cxx.Declaration_objcProperty{} -> return $ Just SymbolKind_Property
  Cxx.Declaration_typeAlias{} -> return $ Just SymbolKind_Class_

toSymbolRecordKind
  :: Cxx.RecordDeclaration
  -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
toSymbolRecordKind k = do
  (Cxx.RecordDeclaration_key _ kind _) <- Glean.keyOf k
  return $ Just $ case kind of
    Cxx.RecordKind_struct_{} -> SymbolKind_Struct
    Cxx.RecordKind_class_{} -> SymbolKind_Class_
    Cxx.RecordKind_union_{} -> SymbolKind_Union

toSymbolDefnKind
  :: Cxx.Definition -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
toSymbolDefnKind k = case k of
  Cxx.Definition_record_ x -> do
    Cxx.RecordDefinition_key decl _ _ <- Glean.keyOf x
    toSymbolRecordKind decl
  Cxx.Definition_function_{} -> return $ Just SymbolKind_Function
  Cxx.Definition_enum_{} -> return $ Just SymbolKind_Enum
  Cxx.Definition_objcMethod{} -> return Nothing
  Cxx.Definition_objcContainer{} -> return Nothing
  Cxx.Definition_variable{} -> return $ Just SymbolKind_Variable
  Cxx.Definition_namespace_{} -> return $ Just SymbolKind_Namespace

--
-- Qualified names for C++
--

instance ToQName Cxx.Entity where
  toQName e = do
    symId <- toSymbol e
    return $ case symId of
      [] -> Left "C++: toQName: No qualified name for this symbol"
      [name] -> Right (Name name, Name "")
      x@(_:_) -> case (init x, last x) of
        (ns, name) -> Right (Name name, Name (intercalate "::" ns))

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Cxx (
    {- instances -}
    cxxEntityDefinitionType
  , cxxEntityKind

  ) where

import Data.Text as Text ( Text, intercalate, break, replace, splitOn )
import Data.Maybe ( maybeToList )
import Control.Monad.Catch ( throwM )

import Glean.Glass.SymbolId.Class

import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types
import qualified Glean.Glass.Types as Glass
import qualified Glean.Schema.Cxx1.Types as Cxx

import Glean.Schema.CodeCxx.Types as Cxx
    ( Entity(..), Definition(..) )

instance Symbol Cxx.Entity where
  toSymbol _ = throwM $ SymbolError "Cxx.Entity: use toSymbolWithPath"

  toSymbolWithPath e (Path path) = (root++) <$> case e of
      Cxx.Entity_defn defn -> toSymbol defn
      -- these are "second class" in that they're less significant in
      -- glass activities than defns, so we tag them to differentiate
      -- we see a lot of them in xrefs through, where we don't do as much
      -- decl -> defn conversions
      Cxx.Entity_decl decl -> do
        sym <- toSymbol decl
        pure (sym ++ [".decl"])
      Cxx.Entity_enumerator enum -> toSymbolPredicate enum
      Cxx.Entity_EMPTY -> return []
    where
      -- find common repo anchor (e.g. "fbcode" or "xplat")
      (mroot, rest) = Text.break (=='/') path
      root = case mroot of
        "" -> []
        p | rest == "" -> [mempty] -- no path (e.g. "test" repo) produces //
          | otherwise -> [p]

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
    Cxx.Declaration_EMPTY -> return []

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
    Cxx.Definition_EMPTY -> return []

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
  toSymbol (Cxx.FunctionDeclaration_key fqname sig _method _) =
    toSymbolFunctionDeclaration fqname sig

--
-- function declarations. Symbol Id format is
--
-- scope1 / scope2 / name ( / .decl )?
-- scope1 / name / .ctor / .decl  -- default constructor
-- scope1 / name / .ctor / sig_param .. / .decl  -- overloaded by type
--
toSymbolFunctionDeclaration
  :: Cxx.FunctionQName -> Cxx.Signature -> Glean.RepoHaxl u w [Text]
toSymbolFunctionDeclaration fqname sig = do
  Cxx.FunctionQName_key fname scope <- Glean.keyOf fqname
  scopeToks <- toSymbol scope
  fn <- Glean.keyOf fname
  nameToks <- case fn of
    -- functions: name/.f/p1,p2,retTy(/.decl)?
    --
    -- tbd. nullary/void functions
    --
    Cxx.FunctionName_key_name n -> do
      nameStr <- Glean.keyOf n
      Cxx.Signature_key retTy params <- Glean.keyOf sig
      sigStr <- toSymbolSignature params retTy
      return [nameStr,".f",sigStr]

    Cxx.FunctionName_key_operator_ x -> return [x]
    Cxx.FunctionName_key_literalOperator x -> return [x]

    --
    -- constructors: class/.c/p1,p2(/.decl)?
    --
    -- Note: .c doesn't use the body of the explicit constructors in unions
    -- e.g.   explicit Data() : nul(nullptr) {}
    -- nor:   const_dynamic_view() noexcept = default;
    --
    -- Nullary constructors have no signature token.
    --
    Cxx.FunctionName_key_constructor{}-> do
      Cxx.Signature_key _retTy params <- Glean.keyOf sig
      mSigStr <- toSymbolSignatureParams params
      return (".c" : maybeToList mSigStr)

    Cxx.FunctionName_key_destructor{} -> return [".dtor"]
    Cxx.FunctionName_key_conversionOperator ty -> pure <$> Glean.keyOf ty
    Cxx.FunctionName_key_EMPTY -> return []

  return (scopeToks ++ nameToks)

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

instance Symbol Cxx.FunctionQName where
  toSymbol fqn = Glean.keyOf fqn >>= toSymbol

instance Symbol Cxx.FunctionQName_key where
  toSymbol (Cxx.FunctionQName_key name scope) = scope <:> name

instance Symbol Cxx.NamespaceQName where
  toSymbol k = toSymbolPredicate k

instance Symbol Cxx.FunctionName where
  toSymbol k = toSymbolPredicate k

-- For things like constructors where we know the return type is void
-- we can just focus on the param types
toSymbolSignatureParams :: [Cxx.Parameter] -> Glean.RepoHaxl u w (Maybe Text)
toSymbolSignatureParams [] = pure Nothing
toSymbolSignatureParams params = do
  sigToks <- mapM (\(Cxx.Parameter _name ty) -> toTypeSymbol ty) params
  return $ Just (intercalate "," sigToks)

-- For overloaded functions we need the full type signature to get the semantic
-- encoding sufficiently unique. There is _always_ a return type, so that's the
-- last element
toSymbolSignature :: [Cxx.Parameter] -> Cxx.Type -> Glean.RepoHaxl u w Text
toSymbolSignature [] retTy = toTypeSymbol retTy
toSymbolSignature params retTy = do
  sigToks <- mapM (\(Cxx.Parameter _name ty) -> toTypeSymbol ty) params
  retTyStr <- toTypeSymbol retTy
  return $ intercalate "," sigToks <> "," <> retTyStr

instance Symbol Cxx.FunctionName_key where
  toSymbol k = case k of
    Cxx.FunctionName_key_name x -> toSymbol x
    Cxx.FunctionName_key_operator_ x -> return [x]
    Cxx.FunctionName_key_literalOperator x -> return [x]
    Cxx.FunctionName_key_constructor _ -> return [".ctor"]
    Cxx.FunctionName_key_destructor _ -> return [".dtor"]
    Cxx.FunctionName_key_conversionOperator ty -> pure <$> Glean.keyOf ty
    Cxx.FunctionName_key_EMPTY -> return []

instance Symbol Cxx.Scope where
  toSymbol k = case k of
    Cxx.Scope_global_ _ -> return []
    Cxx.Scope_namespace_ nsqname -> toSymbolPredicate nsqname
    Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ qname _) ->
      toSymbolPredicate qname
      -- local scope: could be e.g. a function ctor
    Cxx.Scope_local qname -> toSymbolPredicate qname
    Cxx.Scope_EMPTY -> return []

instance Symbol Cxx.ObjcContainerId where
  toSymbol k = case k of
    Cxx.ObjcContainerId_protocol x -> toSymbol x
    Cxx.ObjcContainerId_interface_ x -> toSymbol x
    Cxx.ObjcContainerId_categoryInterface x -> toSymbol x
    Cxx.ObjcContainerId_extensionInterface x -> toSymbol x
    Cxx.ObjcContainerId_implementation x -> toSymbol x
    Cxx.ObjcContainerId_categoryImplementation x -> toSymbol x
    Cxx.ObjcContainerId_EMPTY -> return []

instance Symbol Cxx.ObjcCategoryId where
  toSymbol (Cxx.ObjcCategoryId n1 n2) = do
    k1 <- Glean.keyOf n1
    k2 <- Glean.keyOf n2
    return [k1,k2] -- which order?

instance Symbol Cxx.ObjcSelector where
  toSymbol k = reverse <$> Glean.keyOf k -- :: [Text] ? order?

-- Currently we don't encode the parameter name, just its type.
-- > "folly::dynamic::Array &&"
toTypeSymbol :: Cxx.Type -> Glean.RepoHaxl u w Text
toTypeSymbol ty = escape . normalize <$> Glean.keyOf ty
  where
    -- For readability in the url encoding we replace %20 with +
    -- so we can get encodings like:
    -- > fbsource/cpp/fbcode/folly/dynamic/.c/const+char+*
    --
    -- reversed in Cxx.Parse.tokenize
    --
    normalize :: Text -> Text
    normalize = Text.replace " " "+"
    --
    -- We also need to escape any comma literals, as that's our separate char
    -- e.g. 'std::pair<a, b>' has to go through unscathed
    --
    escape :: Text -> Text
    escape = Text.replace "," " "

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
-- Entity "parent" relationship labelling
--

instance ToSymbolParent Cxx.Entity where
  toSymbolParent e = case e of
    Cxx.Entity_decl x -> toSymbolParent x
    Cxx.Entity_defn{} -> return Nothing
    Cxx.Entity_enumerator x -> Glean.keyOf x >>= toSymbolParent
    Cxx.Entity_EMPTY -> return Nothing

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
    Cxx.Declaration_EMPTY -> return Nothing

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
    cxxParentQNameScope qname

instance ToSymbolParent Cxx.EnumDeclaration_key where
  toSymbolParent (Cxx.EnumDeclaration_key qname _is_scoped _type _) =
    cxxParentQNameScope qname

instance ToSymbolParent Cxx.FunctionDeclaration_key where
  toSymbolParent (Cxx.FunctionDeclaration_key fqname _sig _todo _) =
    Glean.keyOf fqname >>= toSymbolParent

instance ToSymbolParent Cxx.VariableDeclaration_key where
  toSymbolParent (Cxx.VariableDeclaration_key qname _ty _kind _) =
    cxxParentQNameScope qname

instance ToSymbolParent Cxx.TypeAliasDeclaration_key  where
  toSymbolParent (Cxx.TypeAliasDeclaration_key qname _ _ _) =
    cxxParentQNameScope qname

cxxEnumDeclParentName
  :: Cxx.EnumDeclaration_key -> Glean.RepoHaxl u w (Maybe Name)
cxxEnumDeclParentName (Cxx.EnumDeclaration_key qname _scoped _type _) =
  cxxParentQNameScope qname

cxxScopeName :: Cxx.Scope -> Glean.RepoHaxl u w (Maybe Name)
cxxScopeName scope = case scope of
  Cxx.Scope_global_ _ -> return Nothing
  Cxx.Scope_namespace_ nsqname -> cxxParentNSName nsqname
  Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ qname _) ->
    Just <$> cxxParentQName qname
  Cxx.Scope_local fqname -> cxxParentFunctionQName fqname
  Cxx.Scope_EMPTY -> return Nothing

cxxParentFunctionQName :: Cxx.FunctionQName -> Glean.RepoHaxl u w (Maybe Name)
cxxParentFunctionQName fqname = do
  (Cxx.FunctionQName_key _name _scope) <- Glean.keyOf fqname
  return Nothing -- TODO(batanasov) parent name from Cxx.FunctionName_key

cxxParentQName :: Cxx.QName -> Glean.RepoHaxl u w Name
cxxParentQName qname = do
  (Cxx.QName_key name _scope) <- Glean.keyOf qname
  cxxNameToName name

cxxParentQNameScope :: Cxx.QName -> Glean.RepoHaxl u w (Maybe Name)
cxxParentQNameScope qname = do
  (Cxx.QName_key _name scope) <- Glean.keyOf qname
  cxxScopeName scope

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
  Cxx.Entity_EMPTY -> Glass.DefinitionKind_Definition -- fallback to definition

--
-- entity labelling
--
cxxEntityKind :: Cxx.Entity -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
cxxEntityKind e = case e of
  Cxx.Entity_decl x -> toSymbolDeclKind x
  Cxx.Entity_defn x -> toSymbolDefnKind x
  Cxx.Entity_enumerator{} -> return (Just SymbolKind_Enumerator)
  Cxx.Entity_EMPTY -> return Nothing

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
  Cxx.Declaration_EMPTY -> return Nothing

toSymbolRecordKind
  :: Cxx.RecordDeclaration
  -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
toSymbolRecordKind k = do
  (Cxx.RecordDeclaration_key _ kind _) <- Glean.keyOf k
  return $ case kind of
    Cxx.RecordKind_struct_{} -> Just SymbolKind_Struct
    Cxx.RecordKind_class_{} -> Just SymbolKind_Class_
    Cxx.RecordKind_union_{} -> Just SymbolKind_Union
    Cxx.RecordKind_EMPTY -> Nothing

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
  Cxx.Definition_EMPTY -> return Nothing

--
-- Qualified names for C++
--
-- Todo: rewrite this entirely based on induction on the entity type
-- make sure the generated scope/local name fragments match clangd
--
instance ToQName Cxx.Entity where
  toQName e = do
    symId <- toSymbolWithPath e (Path mempty) -- we already had the symbolid tho
    return $ case symId of -- this is a hack, should use the real scope/qname
      [] -> Left "C++: toQName: No qualified name for this symbol"
      [name] -> Right (Name name, Name "")
      x@(_:_) ->
        -- check .c signature case
        case Prelude.break (== ".c") x of
          (scope, ".c":params) -> ctorSignatureQName scope params
          _ -> case Prelude.break (== ".f") x of
            (scope, ".f":params) -> functionSignatureQName scope params
            _ -> case (init x, last x) of
              (ms, name)
                | name `elem` [".ctor",".dtor",".decl"]
                -> let (ns, name') = (init ms, last ms)
                   in Right (Name name', Name (intercalate "::" ns))
                | otherwise -> Right (Name name, Name (intercalate "::" ms))

--
-- e.g. default constructors:
-- fbsource/cpp/fbcode/folly/dynamic/.c/.decl
--
-- fbsource/cpp/fbcode/folly/dynamic_view/.c
-- > dynamic_view() in folly::dynamic_view
--
-- fbsour..../folly/dynamic_view/.c/const+folly::dynamic_view+%26
-- > dynamic_view(const folly::dynamic_view &) in folly::dynamic_view
--
ctorSignatureQName :: [Text] -> [Text] -> Either Text (Name, Name)
ctorSignatureQName prefix ps = Right (Name localname, Name scopename)
  where
    localname = name <> formatParams (sigOf ps)
    scopename = intercalate "::" scope
    (scope, name) = case prefix of
      [n] -> ([n], n)
      _ -> (prefix, last prefix)

sigOf :: [Text] -> Maybe Text
sigOf ps = case ps of
  [] -> Nothing
  [".decl"] -> Nothing
  [sig,".decl"] -> Just sig
  [sig] -> Just sig
  _ -> Nothing -- we never have more than one sig token

--
-- function names
--
functionSignatureQName :: [Text] -> [Text] -> Either Text (Name, Name)
functionSignatureQName [] _ = Left "functionSignatureQName: empty function name"
functionSignatureQName prefix _ps = Right (Name localname, Name scopename)
  where
    localname = name {- not sure we want rtypes in function names -}
    scopename = intercalate "::" scope

    (scope, name) = case prefix of
      [n] -> ([], n) -- global function
      _ -> (init prefix, last prefix)

formatParams :: Maybe Text -> Text
formatParams params = "(" <> maybe "" denormalize params <> ")"

denormalize :: Text -> Text
denormalize =
      intercalate ", "
    . map (Text.replace "+" " " .
            Text.replace " " ",")
    . Text.splitOn ","

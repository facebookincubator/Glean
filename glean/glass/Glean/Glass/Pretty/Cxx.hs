{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Pretty.Cxx (
    prettyCxxSignature
  ) where

import Glean.Schema.CodeCxx.Types as Cxx ( Entity(..), Definition(..) )

import qualified Glean.Schema.Cxx1.Types as Cxx

import Data.Text ( Text, intercalate )
import Data.Maybe ( fromMaybe )
import Util.Text ( textShow )
import qualified Data.Text as Text

(<+>) :: Text -> Text -> Text
"" <+> b = b
a <+> "" = a
a <+> b = a <> " " <> b

(<::>) :: Text -> Text -> Text
a <::> b = a <> "::" <> b

-- This behavior is intended to match clangd 'hover' feature
-- to make sure, user would not notice the change from
-- clangd language server to glean-glass language server.
--
-- Some signatures doesn't match clangd
--   * glean-glass signatures have more information than clangd (easy to fix)
--   * glean-glass signatures have less information than clangd (hard to fix,
--      as it may involve changing of the cxx schema and clang indexer changes)
prettyCxxSignature :: Cxx.Entity -> Text
prettyCxxSignature e = case e of
  Cxx.Entity_decl decl -> prettyDecl decl
  Cxx.Entity_defn def -> prettyDefinition def
  Cxx.Entity_enumerator x -> prettyEnumerator x

intentionallyEmpty :: Text
intentionallyEmpty = ""

todoEmpty :: Text
todoEmpty = ""

-- The key should always be there, but it may not be fetched.
-- It is a responsibility of the caller to make sure
-- the Entity is fully fetched before
-- generating Entity's signature
missingKey :: Text
missingKey = ""

anonymousNamespace :: Text
anonymousNamespace = "(anonymous)"

prettyEnumerator :: Cxx.Enumerator -> Text
prettyEnumerator (Cxx.Enumerator _ mkey) = case mkey of
  Nothing -> missingKey
  -- clangd just shows name and just it :( it is not useful actually
  -- but we want to stick to the clangd
  Just (Cxx.Enumerator_key name _ _) -> prettyName name
  -- Instead we could have had potentially:
  -- -> prettyEnum enum_decl <+> "{ " <+> prettyName name <+> " }"
  -- which would have produced something like
  -- "enum class biggrep2::FetchResult {  NoSpace  }"
  --   * Which doesn't have all values of the enum
  --   * but it is easy to read
  --   * and it WILL be correctly cpp-highlighted

prettyDecl :: Cxx.Declaration -> Text
prettyDecl d = case d of
  Cxx.Declaration_namespace_ x -> prettyNamespace x
  Cxx.Declaration_usingDeclaration x -> prettyUsingDecl x
  Cxx.Declaration_usingDirective x -> prettyUsingDirective x
  Cxx.Declaration_enum_ x -> prettyEnum x
  Cxx.Declaration_variable x -> prettyVariable x
  Cxx.Declaration_objcContainer _ -> intentionallyEmpty
  Cxx.Declaration_objcMethod _ -> intentionallyEmpty
  Cxx.Declaration_objcProperty _ -> intentionallyEmpty
  Cxx.Declaration_typeAlias x -> prettyTypeAlias x
  Cxx.Declaration_record_ x -> prettyRecord x
  Cxx.Declaration_function_ x -> prettyFunction x

prettyEnum :: Cxx.EnumDeclaration -> Text
prettyEnum (Cxx.EnumDeclaration _ mkey) = case mkey of
  Just (Cxx.EnumDeclaration_key qname is_scoped ttype _) -> foldr1 (<+>)
    ["enum"
    , optional is_scoped "class"
    , prettyQName qname
    , prettyEnumType ttype
    ]
  Nothing -> missingKey

prettyEnumType :: Maybe Cxx.Type -> Text
prettyEnumType Nothing = intentionallyEmpty
prettyEnumType (Just ttype) = ":" <+> prettyType ttype

-- Notes:
-- * "kind = { global_ = {definition = true}" doesn't guarantee that this
--   variable has an 'extern' key
prettyVariable :: Cxx.VariableDeclaration -> Text
prettyVariable (Cxx.VariableDeclaration _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.VariableDeclaration_key qname ttype kind _) -> foldr1 (<+>)
    [ prettyVarPrefix kind
    , prettyType ttype
    , prettyVarName kind qname
    ]

prettyVarName :: Cxx.VariableKind -> Cxx.QName -> Text
prettyVarName kind qname = case kind of
  Cxx.VariableKind_field (Cxx.Field _ _) -> prettyQNameDot qname
  _ -> prettyQName qname

prettyVarPrefix :: Cxx.VariableKind -> Text
prettyVarPrefix kind = case kind of
  Cxx.VariableKind_field (Cxx.Field mutable _) -> optional mutable "mutable"
  Cxx.VariableKind_global_ (Cxx.GlobalVariable gv_kind attribute _) ->
    prefix1 <+> prefix2
    where
      prefix1 = case gv_kind of
        Cxx.GlobalVariableKind_StaticVariable -> "static"
        Cxx.GlobalVariableKind_StaticMember -> "static"
        Cxx.GlobalVariableKind_SimpleVariable -> intentionallyEmpty
      prefix2 = case attribute of
        Cxx.GlobalVariableAttribute_Plain -> intentionallyEmpty
        Cxx.GlobalVariableAttribute_Inline -> intentionallyEmpty
        -- TODO: there is a bug with const
        Cxx.GlobalVariableAttribute_Constexpr -> todoEmpty
  Cxx.VariableKind_ivar _ -> intentionallyEmpty

prettyTypeAlias :: Cxx.TypeAliasDeclaration -> Text
prettyTypeAlias (Cxx.TypeAliasDeclaration _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.TypeAliasDeclaration_key name ttype kind _) -> case kind of
    Cxx.TypeAliasKind_Typedef -> foldr1 (<+>)
      [ "typedef"
      , prettyType ttype
      , prettyQName name
      ]
    Cxx.TypeAliasKind_Using -> foldr1 (<+>)
      [ "using"
      , prettyQName name
      , "="
      , prettyType ttype
      ]

prettyFunction :: Cxx.FunctionDeclaration -> Text
prettyFunction (Cxx.FunctionDeclaration _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionDeclaration_key name sign mmethod _) ->
    foldr1 (<+>)
      [ prettyFuncPrefix mmethod
      , if hasReturn name then prettyReturnType sign else intentionallyEmpty
      , prettyFuncQName name <> "(" <> prettyParams sign <> ")"
      , prettyFuncPostfix mmethod
      ]

-- False means that we MUST not even try to print the return type
-- For example in constructor we have it set to 'void'
-- but we do not want to print it
hasReturn :: Cxx.FunctionQName -> Bool
hasReturn (Cxx.FunctionQName _ mkey) = case mkey of
  Nothing -> True
  Just (Cxx.FunctionQName_key (Cxx.FunctionName _ mmkey) _) -> case mmkey of
    Nothing -> True -- TODO
    Just key -> case key of
      Cxx.FunctionName_key_name _ -> True
      Cxx.FunctionName_key_literalOperator _ -> True -- TODO: check it
      Cxx.FunctionName_key_conversionOperator _ -> True -- TODO: check it
      Cxx.FunctionName_key_operator_ _ -> True
      Cxx.FunctionName_key_constructor _ -> False
      Cxx.FunctionName_key_destructor _ -> False


prettyFuncPostfix :: Maybe Cxx.MethodSignature -> Text
prettyFuncPostfix Nothing = todoEmpty
prettyFuncPostfix (Just (Cxx.MethodSignature _ is_const _ _)) =
  optional is_const "const"


prettyFuncPrefix :: Maybe Cxx.MethodSignature -> Text
prettyFuncPrefix Nothing = todoEmpty
prettyFuncPrefix (Just (Cxx.MethodSignature is_virtual _ is_volatile _)) =
  optional is_virtual "virtual" <+>
  optional is_volatile "volatile"

prettyReturnType :: Cxx.Signature -> Text
prettyReturnType (Cxx.Signature _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.Signature_key mtype _) -> prettyType mtype

prettyParams :: Cxx.Signature -> Text
prettyParams (Cxx.Signature _ mkey) = case mkey of
  Nothing -> todoEmpty
  Just (Cxx.Signature_key _ []) -> intentionallyEmpty
  Just (Cxx.Signature_key _ params) ->
    intercalate ", " (map prettyParameter params)

prettyParameter :: Cxx.Parameter -> Text
prettyParameter (Cxx.Parameter name mtype) =
  prettyType mtype <+> prettyName name

prettyType :: Cxx.Type -> Text
prettyType (Cxx.Type _ mkey) = fromMaybe "<unknown-type>" mkey

prettyFuncQName :: Cxx.FunctionQName -> Text
prettyFuncQName (Cxx.FunctionQName _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionQName_key func_name scope) ->
    prettyScope scope "::" <> prettyFunctionName func_name scope

prettyFunctionName :: Cxx.FunctionName -> Cxx.Scope -> Text
prettyFunctionName (Cxx.FunctionName _ mkey) scope = case mkey of
  Nothing -> missingKey
  Just key -> case key of
    Cxx.FunctionName_key_name nname -> prettyName nname
    Cxx.FunctionName_key_literalOperator loname -> loname
    Cxx.FunctionName_key_conversionOperator mtype -> prettyType mtype
    Cxx.FunctionName_key_operator_ oname -> if is_lambda
      then "lambda_" <> get_record_id scope
      else oname
    Cxx.FunctionName_key_constructor _ -> owner_name
    Cxx.FunctionName_key_destructor _ -> "~" <> owner_name
    where
      owner_name = prettyScopeBasic scope
      is_lambda = case scope of
        Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ _ _) ->
          Text.null owner_name
        _ -> False

get_record_id::Cxx.Scope -> Text
get_record_id scope = case scope of
  -- TODO: Make it better. Seems to be prone to errors
  Cxx.Scope_recordWithAccess (
    Cxx.Scope_recordWithAccess_ (
      Cxx.QName _ (Just (Cxx.QName_key (Cxx.Name id _) _) ) ) _) -> textShow id
  _ -> intentionallyEmpty

prettyNamespace :: Cxx.NamespaceDeclaration -> Text
prettyNamespace (Cxx.NamespaceDeclaration _ mkey) = case mkey of
  Just (Cxx.NamespaceDeclaration_key qname _) ->
    "namespace" <+> prettyNamespaceQName qname
  -- TODO: Decide anonymousNamespace | intentionallyEmpty | missingKey
  -- Check if there are "anonymous" things setting 'key' to Nothing.
  Nothing -> todoEmpty

prettyNamespaceQName :: Cxx.NamespaceQName -> Text
prettyNamespaceQName (Cxx.NamespaceQName _ mkey) = case mkey of
  Just key -> case key of
    -- TODO: Decide anonymousNamespace | intentionallyEmpty | missingKey
    -- Check if there are "anonymous" namespaces setting 'name' to Nothing.
    Cxx.NamespaceQName_key name Nothing -> maybe todoEmpty prettyName name
    Cxx.NamespaceQName_key name (Just parent) ->
      prettyNamespaceQName parent <::> maybe todoEmpty prettyName name
  -- Scope.namespace in VariableDeclaration hits it here
  Nothing -> anonymousNamespace

prettyName :: Cxx.Name -> Text
-- NamespaceDeclaration hits it here
prettyName (Cxx.Name _ key) = fromMaybe anonymousNamespace key

prettyRecord :: Cxx.RecordDeclaration -> Text
prettyRecord (Cxx.RecordDeclaration _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.RecordDeclaration_key name kind _) -> case kind of
    Cxx.RecordKind_struct_{} -> "struct" <+> prettyQName name
    Cxx.RecordKind_class_{} -> "class" <+> prettyQName name
    Cxx.RecordKind_union_{} -> "union" <+> prettyQName name

-- -- Produces X::Y::Z.T
-- -- As we usually access fields through the .
-- Uses "::" as separator everywhere expect the last one, where "." is used
prettyQNameDot :: Cxx.QName -> Text
prettyQNameDot (Cxx.QName _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.QName_key name scope) -> prettyScope scope "." <> prettyName name

-- Produces X::Y::Z::T
-- Uses "::" as separator everywhere
prettyQName :: Cxx.QName -> Text
prettyQName (Cxx.QName _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.QName_key name scope) -> prettyScope scope "::" <> prettyName name

-- Intentionally scope has the <::> inside the function
-- as we want to distinguish local and global as otherwise
-- it is impossible to handle it different ways on the caller side
prettyScope :: Cxx.Scope -> Text -> Text
prettyScope k separator = case k of
    Cxx.Scope_global_ _ -> separator
    Cxx.Scope_namespace_ name -> prettyNamespaceQName name <> separator
    Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ record _) ->
      prettyQName record <> separator
    -- Current implementation explicitly ignores it
    -- but we can actually print a function name if needed
    Cxx.Scope_local _ ->  intentionallyEmpty

prettyScopeBasic :: Cxx.Scope -> Text
prettyScopeBasic k = case k of
    Cxx.Scope_recordWithAccess (
      Cxx.Scope_recordWithAccess_ (Cxx.QName _ mkey) _) ->
        case mkey of
          Nothing -> missingKey
          Just (Cxx.QName_key name _) -> prettyName name
    _ -> intentionallyEmpty

optional :: Bool -> Text -> Text
optional True text = text
optional False _ = intentionallyEmpty

prettyUsingDecl :: Cxx.UsingDeclaration -> Text
prettyUsingDecl (Cxx.UsingDeclaration _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.UsingDeclaration_key name _) -> "using" <+> prettyFuncQName name

prettyUsingDirective :: Cxx.UsingDirective -> Text
prettyUsingDirective (Cxx.UsingDirective _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.UsingDirective_key name _) -> "using namespace" <+> prettyQName name

prettyDefinition :: Cxx.Definition -> Text
prettyDefinition def = case def of
  Cxx.Definition_record_ x -> prettyRecordDef x
  Cxx.Definition_function_ x -> prettyFuncDef x
  Cxx.Definition_enum_ x -> prettyEnumDef x
  Cxx.Definition_objcMethod _ -> intentionallyEmpty
  Cxx.Definition_objcContainer _ -> intentionallyEmpty
  Cxx.Definition_variable x -> prettyVariable x
  Cxx.Definition_namespace_ x -> prettyNamespaceDef x

prettyNamespaceDef :: Cxx.NamespaceDefinition -> Text
prettyNamespaceDef (Cxx.NamespaceDefinition _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.NamespaceDefinition_key decl _) -> prettyNamespace decl

prettyEnumDef :: Cxx.EnumDefinition -> Text
prettyEnumDef (Cxx.EnumDefinition _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.EnumDefinition_key decl _) -> prettyEnum decl

prettyFuncDef :: Cxx.FunctionDefinition -> Text
prettyFuncDef (Cxx.FunctionDefinition _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionDefinition_key decl _) -> prettyFunction decl

prettyRecordDef :: Cxx.RecordDefinition -> Text
prettyRecordDef (Cxx.RecordDefinition _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.RecordDefinition_key decl _ _) -> prettyRecord decl

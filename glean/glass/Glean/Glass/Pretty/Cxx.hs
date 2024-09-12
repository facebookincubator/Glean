{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Pretty.Cxx (
  prettyCxxSignature,
  Qualified(..)
) where

import qualified Glean

import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.Cxx1.Types as Cxx

import Data.Text ( Text, intercalate )
import Compat.Prettyprinter
  (pretty, layoutSmart, LayoutOptions, SimpleDocStream, reAnnotateS)
import Data.Maybe ( fromMaybe )
import qualified Data.Text as Text
import Safe (atMay)
import Util.Text (textShow)
import Glean.Glass.Types ( SymbolId(..) )

(<+>) :: Text -> Text -> Text
"" <+> b = b
a <+> "" = a
a <+> b = a <> " " <> b

(<::>) :: Text -> Text -> Text
a <::> b = a <> "::" <> b

data Qualified = Qualified | Unqualified

isQualified :: Qualified -> Bool
isQualified Qualified = True
isQualified Unqualified = False

-- This behavior is intended to match clangd 'hover' feature
-- to make sure, user would not notice the change from
-- clangd language server to glean-glass language server.
--
-- Some signatures doesn't match clangd
--   * glean-glass signatures have more information than clangd (easy to fix)
--   * glean-glass signatures have less information than clangd (hard to fix,
--      as it may involve changing of the cxx schema and clang indexer changes)
--
-- Signatures can be qualified or unqualified. This affects whether the
-- scope of the symbol will be output. Other symbol signatures in
-- the pretty signature will remain fully qualified even if this is
-- false.
prettyCxxSignature
  :: LayoutOptions
  -> Cxx.Entity
  -> Qualified
  -> Maybe (SimpleDocStream (Maybe SymbolId))
prettyCxxSignature opts e qualified = case text of
    "" -> Nothing
    _ ->  Just $ reAnnotateS (const Nothing) $ layoutSmart opts $ pretty text
  where
    text = case e of
      Cxx.Entity_decl decl -> prettyDecl decl qualified
      Cxx.Entity_defn def -> prettyDefinition def qualified
      Cxx.Entity_enumerator x ->
        prettyEnumerator x -- clangd doesn't qualify enumerators
      Cxx.Entity_objcSelectorSlot slot -> prettyObjcSelectorSlot slot
      Cxx.Entity_EMPTY -> intentionallyEmpty

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

implicitUnderlyingEnumType :: Text
implicitUnderlyingEnumType = "int"

prettyEnumerator :: Cxx.Enumerator -> Text
prettyEnumerator (Cxx.Enumerator _ mkey) = case mkey of
  Nothing -> missingKey
  -- clangd just shows name and just it :( it is not useful actually
  -- but we want to stick to the clangd
  Just (Cxx.Enumerator_key name _ _) -> prettyName name

-- Instead we could have had potentially:
-- -> prettyEnum enum_decl <+> "{ " <+> prettyName name <+> " }"
-- which would have produced something like
-- "enum class biggrep::FetchResult {  NoSpace  }"
--   * Which doesn't have all values of the enum
--   * but it is easy to read
--   * and it WILL be correctly cpp-highlighted

prettyDecl :: Cxx.Declaration -> Qualified -> Text
prettyDecl d qualified = case d of
  Cxx.Declaration_namespace_ x -> prettyNamespace x qualified
  Cxx.Declaration_usingDeclaration x -> prettyUsingDecl x
  Cxx.Declaration_usingDirective x -> prettyUsingDirective x
  Cxx.Declaration_enum_ x -> prettyEnum x qualified
  Cxx.Declaration_variable x -> prettyVariable x qualified
  Cxx.Declaration_objcContainer _ -> intentionallyEmpty
  Cxx.Declaration_objcMethod _ -> intentionallyEmpty
  Cxx.Declaration_objcProperty _ -> intentionallyEmpty
  Cxx.Declaration_typeAlias x -> prettyTypeAlias x qualified
  Cxx.Declaration_namespaceAlias x -> prettyNamespaceAlias x qualified
  Cxx.Declaration_record_ x -> prettyRecord x qualified
  Cxx.Declaration_function_ x -> prettyFunction x qualified
  Cxx.Declaration_EMPTY -> intentionallyEmpty

prettyEnum :: Cxx.EnumDeclaration -> Qualified -> Text
prettyEnum (Cxx.EnumDeclaration _ mkey) qualified = case mkey of
  Just (Cxx.EnumDeclaration_key qname is_scoped ttype _) ->
    foldr1
      (<+>)
      [ "enum"
      , optional is_scoped "class"
      , prettyQName qname qualified
      , prettyEnumType ttype
      ]
  Nothing -> missingKey

prettyEnumType :: Maybe Cxx.Type -> Text
prettyEnumType maybeType = ":" <+> fillImplicitUnderlyingType maybeType
  where
    fillImplicitUnderlyingType Nothing = implicitUnderlyingEnumType
    fillImplicitUnderlyingType (Just ttype) = prettyType ttype

prettyObjcSelectorSlot :: Cxx.ObjcSelectorSlotEntity -> Text
prettyObjcSelectorSlot (Cxx.ObjcSelectorSlotEntity method idx) =
  fromMaybe missingKey $ do
    decl <- case method of
      Cxx.ObjcMethodEntity_decl decl -> Just decl
      Cxx.ObjcMethodEntity_defn Cxx.ObjcMethodDefinition{
        objcMethodDefinition_key = Just decl} -> Just decl
      _ -> Nothing
    Cxx.ObjcMethodDeclaration_key{
      objcMethodDeclaration_key_selector = Cxx.ObjcSelector{
        objcSelector_key = Just selector
      }
    } <- Glean.getFactKey decl
    let index = fromIntegral $ Glean.fromNat idx
    atMay selector index
-- Notes:
-- * "kind = { global_ = {definition = true}" doesn't guarantee that this
--   variable has an 'extern' key
prettyVariable :: Cxx.VariableDeclaration -> Qualified -> Text
prettyVariable (Cxx.VariableDeclaration _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.VariableDeclaration_key qname ttype kind _) ->
    foldr1
      (<+>)
      [ prettyVarPrefix kind
      , prettyType ttype
      , prettyQName qname qualified
      ]

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
        Cxx.GlobalVariableKind__UNKNOWN {} -> intentionallyEmpty
      prefix2 = case attribute of
        Cxx.GlobalVariableAttribute_Plain -> intentionallyEmpty
        Cxx.GlobalVariableAttribute_Inline -> intentionallyEmpty
        -- TODO: there is a bug with const
        Cxx.GlobalVariableAttribute_Constexpr -> todoEmpty
        Cxx.GlobalVariableAttribute__UNKNOWN {} -> todoEmpty
  Cxx.VariableKind_local (Cxx.LocalVariable lv_kind attribute) ->
    prefix1 <+> prefix2
    where
      prefix1 = case lv_kind of
        Cxx.LocalVariableKind_StaticVariable -> "static"
        Cxx.LocalVariableKind_SimpleVariable -> intentionallyEmpty
        Cxx.LocalVariableKind_Parameter -> intentionallyEmpty
        Cxx.LocalVariableKind__UNKNOWN {} -> intentionallyEmpty
      prefix2 = case attribute of
        Cxx.LocalVariableAttribute_Plain -> intentionallyEmpty
        -- TODO: there is a bug with const
        Cxx.LocalVariableAttribute_Constexpr -> todoEmpty
        Cxx.LocalVariableAttribute__UNKNOWN {} -> todoEmpty
  Cxx.VariableKind_ivar _ -> intentionallyEmpty
  Cxx.VariableKind_EMPTY -> todoEmpty

prettyTypeAlias :: Cxx.TypeAliasDeclaration -> Qualified -> Text
prettyTypeAlias (Cxx.TypeAliasDeclaration _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.TypeAliasDeclaration_key name ttype kind _) -> case kind of
    Cxx.TypeAliasKind_Typedef ->
      foldr1
        (<+>)
        [ "typedef"
        , prettyType ttype
        , prettyQName name qualified
        ]
    Cxx.TypeAliasKind_Using ->
      foldr1
        (<+>)
        [ "using"
        , prettyQName name qualified
        , "="
        , prettyType ttype
        ]
    Cxx.TypeAliasKind__UNKNOWN {} -> todoEmpty

prettyNamespaceAlias :: Cxx.NamespaceAliasDeclaration -> Qualified -> Text
prettyNamespaceAlias (Cxx.NamespaceAliasDeclaration _ mkey) qualified =
  case mkey of
    Nothing -> missingKey
    Just (Cxx.NamespaceAliasDeclaration_key qname target _) ->
      foldr1
        (<+>)
        [ "namespace", prettyNamespaceQName qname qualified, "=", targetName ]
      where
        targetName = case target of
          Cxx.NamespaceTarget_namespace_
            (Cxx.NamespaceDeclaration _ mkey) ->
              case mkey of
                Just (Cxx.NamespaceDeclaration_key qname _) ->
                  prettyNamespaceQName qname Qualified
                Nothing -> missingKey
          Cxx.NamespaceTarget_namespaceAlias
            (Cxx.NamespaceAliasDeclaration _ mkey) ->
              case mkey of
                Just (Cxx.NamespaceAliasDeclaration_key qname _ _) ->
                  prettyNamespaceQName qname Qualified
                Nothing -> missingKey
          Cxx.NamespaceTarget_EMPTY -> todoEmpty

prettyFunction :: Cxx.FunctionDeclaration -> Qualified -> Text
prettyFunction (Cxx.FunctionDeclaration _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionDeclaration_key name sign mmethod _) ->
    foldr1
      (<+>)
      [ prettyFuncPrefix mmethod
      , if hasReturn name then prettyReturnType sign else intentionallyEmpty
      , prettyFuncQName name qualified <> "(" <> prettyParams sign <> ")"
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
      Cxx.FunctionName_key_conversionOperator _ -> False
      Cxx.FunctionName_key_operator_ _ -> True
      Cxx.FunctionName_key_constructor _ -> False
      Cxx.FunctionName_key_destructor _ -> False
      Cxx.FunctionName_key_EMPTY -> True

prettyFuncPostfix :: Maybe Cxx.MethodSignature -> Text
prettyFuncPostfix Nothing = todoEmpty
prettyFuncPostfix (Just (Cxx.MethodSignature _ is_const _ _)) =
  optional is_const "const"

prettyFuncPrefix :: Maybe Cxx.MethodSignature -> Text
prettyFuncPrefix Nothing = todoEmpty
prettyFuncPrefix (Just (Cxx.MethodSignature is_virtual _ is_volatile _)) =
  optional is_virtual "virtual"
    <+> optional is_volatile "volatile"

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
prettyType (Cxx.Type _ mkey) = alignRefOrPtrQualifier
  $ fromMaybe "<unknown-type>" mkey

alignRefOrPtrQualifier :: Text -> Text
alignRefOrPtrQualifier ttype =
  Text.stripEnd (Text.dropWhileEnd isRefOrPtrQualifier ttype)
  <> Text.takeWhileEnd isRefOrPtrQualifier ttype
  where
    isRefOrPtrQualifier x = x == '&' || x == '*'

prettyFuncQName :: Cxx.FunctionQName -> Qualified -> Text
prettyFuncQName (Cxx.FunctionQName _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionQName_key func_name scope)
    | isQualified qualified ->
      prettyScope scope "::" <> prettyFunctionName func_name scope
    | otherwise ->
      prettyFunctionName func_name scope

prettyFunctionName :: Cxx.FunctionName -> Cxx.Scope -> Text
prettyFunctionName (Cxx.FunctionName _ mkey) scope = case mkey of
  Nothing -> missingKey
  Just key -> case key of
    Cxx.FunctionName_key_name nname -> prettyName nname
    Cxx.FunctionName_key_literalOperator loname -> loname
    Cxx.FunctionName_key_conversionOperator mtype -> prettyType mtype
    Cxx.FunctionName_key_operator_ oname
      | is_lambda -> "lambda_" <> getRecordId scope
      | otherwise -> oname
    Cxx.FunctionName_key_constructor _ -> owner_name
    Cxx.FunctionName_key_destructor _ -> "~" <> owner_name
    Cxx.FunctionName_key_EMPTY -> "<unknown>"
    where
      owner_name = prettyScopeBasic scope
      is_lambda = case scope of
        Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ _ _) ->
          Text.null owner_name
        _ -> False

getRecordId :: Cxx.Scope -> Text
getRecordId scope = case scope of
  -- TODO: Make it better. Seems to be prone to errors
  Cxx.Scope_recordWithAccess
    ( Cxx.Scope_recordWithAccess_
        (Cxx.QName _ (Just (Cxx.QName_key (Cxx.Name id _) _)))
        _
      ) -> textShow id
  _ -> intentionallyEmpty

prettyNamespace :: Cxx.NamespaceDeclaration -> Qualified -> Text
prettyNamespace (Cxx.NamespaceDeclaration _ mkey) qualified = case mkey of
  Just (Cxx.NamespaceDeclaration_key qname _) ->
    "namespace" <+> prettyNamespaceQName qname qualified
  -- TODO: Decide anonymousNamespace | intentionallyEmpty | missingKey
  -- Check if there are "anonymous" things setting 'key' to Nothing.
  Nothing -> todoEmpty

prettyNamespaceQName :: Cxx.NamespaceQName -> Qualified -> Text
prettyNamespaceQName (Cxx.NamespaceQName _ mkey) qualified = case mkey of
  Just key -> case key of
    -- TODO: Decide anonymousNamespace | intentionallyEmpty | missingKey
    -- Check if there are "anonymous" namespaces setting 'name' to Nothing.
    Cxx.NamespaceQName_key name Nothing -> maybe todoEmpty prettyName name
    Cxx.NamespaceQName_key name (Just parent)
      | isQualified qualified ->
        prettyNamespaceQName parent Qualified <::>
          maybe todoEmpty prettyName name
      | otherwise -> maybe todoEmpty prettyName name
  -- Scope.namespace in VariableDeclaration hits it here
  Nothing -> anonymousNamespace

prettyName :: Cxx.Name -> Text
-- NamespaceDeclaration hits it here
prettyName (Cxx.Name _ key) = fromMaybe anonymousNamespace key

prettyRecord :: Cxx.RecordDeclaration -> Qualified -> Text
prettyRecord (Cxx.RecordDeclaration _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.RecordDeclaration_key name kind _) -> case kind of
    Cxx.RecordKind_struct_ {} -> "struct" <+> prettyQName name qualified
    Cxx.RecordKind_class_ {} -> "class" <+> prettyQName name qualified
    Cxx.RecordKind_union_ {} -> "union" <+> prettyQName name qualified
    Cxx.RecordKind_EMPTY ->
      "<unknown-declaration>" <+> prettyQName name qualified

-- Produces X::Y::Z::T
-- Uses "::" as separator everywhere
prettyQName :: Cxx.QName -> Qualified -> Text
prettyQName (Cxx.QName _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.QName_key name scope)
    | isQualified qualified ->
      prettyScope scope "::" <> prettyName name
    | otherwise -> prettyName name

-- Intentionally scope has the <::> inside the function
-- as we want to distinguish local and global as otherwise
-- it is impossible to handle it different ways on the caller side
prettyScope :: Cxx.Scope -> Text -> Text
prettyScope k separator = case k of
  Cxx.Scope_global_ _ -> separator
  Cxx.Scope_namespace_ name -> prettyNamespaceQName name Qualified <> separator
  Cxx.Scope_recordWithAccess (Cxx.Scope_recordWithAccess_ record _) ->
    prettyQName record Qualified <> separator
  -- Current implementation explicitly ignores it
  -- but we can actually print a function name if needed
  Cxx.Scope_local _ -> intentionallyEmpty
  Cxx.Scope_EMPTY -> "<unknown-scope>"

prettyScopeBasic :: Cxx.Scope -> Text
prettyScopeBasic k = case k of
  Cxx.Scope_recordWithAccess
    (Cxx.Scope_recordWithAccess_ (Cxx.QName _ mkey) _) ->
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
  Just (Cxx.UsingDeclaration_key name _) ->
    -- using declarations are always qualified as they don't
    -- have a declarator nested-name-specifier to omit in an
    -- unqualified-id
    -- for example `using namespace std::chrono;` declares no
    -- symbol name that could be an unqualified-id OR a qualified-id
    -- In this case, we qualify the symbol they refer to, which
    -- is always unscoped but fully qualified
    "using" <+> prettyFuncQName name Qualified

prettyUsingDirective :: Cxx.UsingDirective -> Text
prettyUsingDirective (Cxx.UsingDirective _ mkey) = case mkey of
  Nothing -> missingKey
  Just (Cxx.UsingDirective_key name _) ->
    -- using directives are always qualified as they don't
    -- have a declarator nested-name-specifier to omit in an
    -- unqualified-id
    -- for example `using namespace std::chrono;` declares no
    -- symbol name that could be an unqualified-id OR a qualified-id
    -- In this case, we qualify the symbol they refer to, which
    -- is always unscoped but fully qualified
    "using namespace" <+> prettyQName name Qualified

prettyDefinition :: Cxx.Definition -> Qualified -> Text
prettyDefinition def qualified = case def of
  Cxx.Definition_record_ x -> prettyRecordDef x qualified
  Cxx.Definition_function_ x -> prettyFuncDef x qualified
  Cxx.Definition_enum_ x -> prettyEnumDef x qualified
  Cxx.Definition_objcMethod _ -> intentionallyEmpty
  Cxx.Definition_objcContainer _ -> intentionallyEmpty
  Cxx.Definition_variable x -> prettyVariable x qualified
  Cxx.Definition_namespace_ x -> prettyNamespaceDef x qualified
  Cxx.Definition_EMPTY -> intentionallyEmpty

prettyNamespaceDef :: Cxx.NamespaceDefinition -> Qualified -> Text
prettyNamespaceDef (Cxx.NamespaceDefinition _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.NamespaceDefinition_key decl _) -> prettyNamespace decl qualified

prettyEnumDef :: Cxx.EnumDefinition -> Qualified -> Text
prettyEnumDef (Cxx.EnumDefinition _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.EnumDefinition_key decl _) -> prettyEnum decl qualified

prettyFuncDef :: Cxx.FunctionDefinition -> Qualified -> Text
prettyFuncDef (Cxx.FunctionDefinition _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.FunctionDefinition_key decl _) -> prettyFunction decl qualified

prettyRecordDef :: Cxx.RecordDefinition -> Qualified -> Text
prettyRecordDef (Cxx.RecordDefinition _ mkey) qualified = case mkey of
  Nothing -> missingKey
  Just (Cxx.RecordDefinition_key decl _ _) -> prettyRecord decl qualified

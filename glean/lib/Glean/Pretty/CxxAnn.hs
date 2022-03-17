{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

-- | A reworking of "Glean.Pretty.Cxx" for @'Doc' 'Ann'@
--
-- This does not expect any missing @Nothing@ key data in predicates. Handle
-- the unexpected with @mempty :: Doc Ann@.
--
-- When this covers enough of cxx.angle it can replace "Glean.Pretty.Cxx"
-- by using 'unAnnotate'
module Glean.Pretty.CxxAnn
  ( -- * Pretty
    prettyName
  , prettyType
  , prettyAccess
  , prettyVirtual
  , prettyShortNamespaceQName
  , prettyScopedNamespaceQName
  , prettyShortQName
  , prettyScopedQName
  , prettyScopeRecordWithAccess
  , prettyScopeColons
  , prettyRecDecl
  , prettyBase
  , prettyFnName
  , prettyShortFunctionName
  , prettyScopedFunctionName
  , prettyFunctionDeclarationName
  , prettyRetType
  , prettyParam
  , prettyParams
  , prettyMethodSignature
  , prettyFunctionSignature
  , prettyEnumDecl
  , prettyEnumerator
  , prettyEnumDef
  , prettyVariableDecl
  , prettyTypeAliasDecl
  , prettyRecordDecl
  , prettyNamespaceDecl
    -- * Utility
  , clangRecordKind
  , getMaybeContainerName
  , breakType
  ) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow (showt)

import Glean.Pretty.Shared
import Glean.Pretty.Styles
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Typed (Predicate(getFactKey), fromNat)
import Glean.Util.AnnMaker
  (AnnMaker, Ann, declRefTos, defDeclRefTos, enumeratorRefTos, anStyle, mayAct)

import Glean.Pretty.Style (Style)

nullName :: Cxx.Name -> Bool
nullName = maybe True Text.null . getFactKey

-- | Get final component of record name in this scope, if it exists, without
-- a namespace prefix. Useful for constructor and destructor names
getMaybeContainerName :: Cxx.Scope -> Maybe Text
getMaybeContainerName = \case
  Cxx.Scope_recordWithAccess Cxx.Scope_recordWithAccess_{..} -> do
    Cxx.QName_key{..} <- getFactKey scope_recordWithAccess__record
    getFactKey qName_key_name
  _ -> Nothing

-- | Using the suggested width, either the text input is already short
-- enough (Left result) or an attempt is made to break it into pieces
-- (Right result).
breakType :: Int -> Text -> Either Text [Text]
breakType limit type_
    | check type_ = Left type_
    | otherwise =
        let choices = scanl (flip ($)) [type_]
              [ chunkOn_ trail ">,"
              , chunkOn_ trail ","
              , chunkOn_ trail ">"
              , chunkOn_ lead "<"
              , chunkOn_ lead " "
              ]
            choice = fromMaybe (last choices) . listToMaybe
                   . filter (all check) $ choices
        in Right choice
  where
    check t = Text.length t <= limit

    -- After Text.splitOn pat, attach pat to end of Text pieces
    trail _pat [] = []
    trail pat xs = map (<> pat) (init xs) ++ [last xs]

    -- After Text.splitOn pat, attach pat to beginning of Text pieces
    lead _pat [] = []
    lead pat (x:xs) = x : map (pat <>) xs

    -- split a Text into [Text], 'side' is 'trail' or 'lead'
    chunkOn side pat long = filter (not . Text.null) . map Text.strip $
      side pat (Text.splitOn pat long)

    -- Only split this piece when it is still too long
    chunkOn' side pat piece | check piece = [piece]
                            | otherwise = chunkOn side pat piece

    -- split pieces that are still too long
    chunkOn_ side pat = concatMap (chunkOn' side pat)

-- -----------------------------------------------------------------------------

prettyName :: Style -> Cxx.Name -> Doc (Ann r)
prettyName style n = fromMaybe mempty $ do
  name_text <- getFactKey n
  pure (txt style name_text)

-- | Attempt to break long types at desired punctuation,
-- apply 'breakType' using the page width as suggested limit.
prettyType :: Cxx.Type -> Doc (Ann r)
prettyType t = pageWidth $ \ pw -> fromMaybe mempty $ do
  type_text <- getFactKey t
  let limit = case pw of
        Unbounded -> 80
        AvailablePerLine w _ -> w
  pure $ either (txt name_type) assemble (breakType limit type_text)
  where
    bump = 4
    assemble xs = (nest bump . fillSep . map (txt name_type) $ xs) <> hardline

prettyAccess :: Cxx.Access -> Doc (Ann r)
prettyAccess a = txt keyword $ case a of
  Cxx.Access_Public -> "public"
  Cxx.Access_Protected -> "protected"
  Cxx.Access_Private -> "private"
  Cxx.Access__UNKNOWN{} -> ""

prettyVirtual :: Bool -> [Doc (Ann r)]
prettyVirtual False = []
prettyVirtual True = [txt keyword "virtual"]

prettyShortNamespaceQName :: Cxx.NamespaceQName -> Doc (Ann r)
prettyShortNamespaceQName nqn = fromMaybe mempty $ do
  Cxx.NamespaceQName_key{..} <- namespaceQName_key nqn
  pure $ case namespaceQName_key_name of
    Nothing -> txt name_namespace "{anonymous}"
    Just name -> prettyName name_namespace name

prettyScopedNamespaceQName :: Cxx.NamespaceQName -> Doc (Ann r)
prettyScopedNamespaceQName nqn = fromMaybe mempty $ do
  Cxx.NamespaceQName_key{..} <- namespaceQName_key nqn
  let here = prettyShortNamespaceQName nqn
  pure $ case namespaceQName_key_parent of
    Nothing -> here
    Just ns -> prettyScopedNamespaceQName ns
      <//> txt operator "::" <//> here

prettyShortQName :: Style -> Cxx.QName -> Doc (Ann r)
prettyShortQName style qn = fromMaybe mempty $ do
  Cxx.QName_key{..} <- getFactKey qn
  pure $ prettyName style qName_key_name

prettyScopedQName :: Style -> Cxx.QName -> Doc (Ann r)
prettyScopedQName style qn = fromMaybe mempty $ do
  Cxx.QName_key{..} <- getFactKey qn
  pure $ prettyScopeColons qName_key_scope <//> prettyName style qName_key_name

prettyScopeRecordWithAccess :: Cxx.Scope_recordWithAccess_ -> Doc (Ann r)
prettyScopeRecordWithAccess Cxx.Scope_recordWithAccess_{..} =
  prettyScopedQName name_class scope_recordWithAccess__record

prettyScopeColons :: Cxx.Scope -> Doc (Ann r)
prettyScopeColons (Cxx.Scope_global_ _) = mempty
prettyScopeColons (Cxx.Scope_namespace_ namespaceQName) =
  prettyScopedNamespaceQName namespaceQName <//> txt operator "::"
prettyScopeColons (Cxx.Scope_recordWithAccess recordWithAccess) =
  prettyScopeRecordWithAccess recordWithAccess <//> txt operator "::"
prettyScopeColons (Cxx.Scope_local fqn) = prettyScopedFnQName fqn  <//> "::"
prettyScopeColons Cxx.Scope_EMPTY = ""

prettyScopeAccess :: Cxx.Scope -> Maybe (Doc (Ann r))
prettyScopeAccess (Cxx.Scope_recordWithAccess recordWithAccess) =
  Just $ prettyAccess (Cxx.scope_recordWithAccess__access recordWithAccess)
    <> txt operator ":"
prettyScopeAccess _ = Nothing

prettyQNameAccess :: Cxx.QName -> Maybe (Doc (Ann r))
prettyQNameAccess qn = fromMaybe mempty $ do
  Cxx.QName_key{..} <- getFactKey qn
  pure $ prettyScopeAccess qName_key_scope

clangRecordKind :: Cxx.RecordKind -> Text
clangRecordKind (Cxx.RecordKind_class_ _) = "class"
clangRecordKind (Cxx.RecordKind_struct_ _) = "struct"
clangRecordKind (Cxx.RecordKind_union_ _) = "union"
clangRecordKind Cxx.RecordKind_EMPTY = ""

prettyRecordKind :: Cxx.RecordKind -> Doc (Ann r)
prettyRecordKind = txt keyword . clangRecordKind

-- | Record name becomes a link.
prettyRecDecl :: AnnMaker r -> Cxx.RecordDeclaration -> [Doc (Ann r)]
prettyRecDecl am decl = fromMaybe [] $ do
  Cxx.RecordDeclaration_key{..} <- getFactKey decl
  let kind = prettyRecordKind recordDeclaration_key_kind
      name = prettyScopedQName name_class recordDeclaration_key_name
  pure [kind, mayAct (declRefTos am decl) name]

-- | Record name becomes a link.
prettyBase :: AnnMaker r -> Cxx.RecordBase -> Doc (Ann r)
prettyBase am Cxx.RecordBase{..} = fillSep $
  [prettyAccess recordBase_access] ++
  prettyVirtual recordBase_isVirtual ++
  prettyRecDecl am recordBase_base

prettyFnName :: Maybe Text -> Cxx.FunctionName -> Doc (Ann r)
prettyFnName maybeContainerName fn = fromMaybe mempty $ do
  fnk <- getFactKey fn
  case fnk of
    Cxx.FunctionName_key_name name -> Just $ prettyName name_function name
    Cxx.FunctionName_key_operator_ x -> Just $ txt operator x
    Cxx.FunctionName_key_literalOperator x -> Just $ txt operator_word x
    Cxx.FunctionName_key_constructor _unit -> Just $ maybe "/* constructor */"
      (txt name_function) maybeContainerName
    Cxx.FunctionName_key_destructor _unit -> Just $ maybe "/* destructor */"
      (\n -> txt name_function ("~" <> n)) maybeContainerName
    Cxx.FunctionName_key_conversionOperator t -> Just $
      txt keyword "operator" </> prettyType t <> lparen <> rparen
    Cxx.FunctionName_key_EMPTY -> Nothing

-- | Shows the name without the scope
prettyShortFnQName :: Cxx.FunctionQName -> Doc (Ann r)
prettyShortFnQName fqn = fromMaybe mempty $ do
  Cxx.FunctionQName_key{..} <- getFactKey fqn
  pure $ prettyFnName (getMaybeContainerName functionQName_key_scope)
    functionQName_key_name

-- | Shows the name with the scope
prettyScopedFnQName :: Cxx.FunctionQName -> Doc (Ann r)
prettyScopedFnQName fqn = fromMaybe mempty $ do
  Cxx.FunctionQName_key{..} <- getFactKey fqn
  pure $ prettyScopeColons functionQName_key_scope <//> prettyShortFnQName fqn

-- | Whole function name becomes a link.
--
-- Inject references @[r]@ to allow for link to definition
prettyFunctionDeclarationName
  :: [r] -> Cxx.FunctionDeclaration -> Doc (Ann r)
prettyFunctionDeclarationName refTos fd = fromMaybe mempty $ do
  Cxx.FunctionDeclaration_key{..} <- getFactKey fd
  pure $ mayAct refTos (prettyScopedFnQName functionDeclaration_key_name)

prettyRetType :: Cxx.Signature -> Doc (Ann r)
prettyRetType s = fromMaybe mempty $ do
  Cxx.Signature_key{..} <- signature_key s
  pure $ prettyType signature_key_returns

prettyParam :: Cxx.Parameter -> Doc (Ann r)
prettyParam Cxx.Parameter{..}
  | nullName parameter_name = prettyType parameter_type
  | otherwise = prettyType parameter_type </>
    prettyName name_variable parameter_name

-- | Pretty prints function parameters, e.g.
--   int * argc, char *** argv, _Bool removeFlags
-- with multi-line support for long names, e.g.
--  int * argc,
--  char *** argv,
--  _Bool removeFlags
prettyParams :: Cxx.Signature -> Doc (Ann r)
prettyParams s = fromMaybe mempty $ do
  Cxx.Signature_key{..} <- signature_key s
  pure $ sep $ punctuate comma $ map prettyParam signature_key_parameters

-- | Just the parameter types without names, useful for ambiguous functions
prettyParamTypes :: Cxx.Signature -> Doc (Ann r)
prettyParamTypes s = fromMaybe mempty $ do
  Cxx.Signature_key{..} <- signature_key s
  pure $ sep $ punctuate comma $ map (prettyType . Cxx.parameter_type)
    signature_key_parameters

helperFunctionName
  :: (Cxx.FunctionQName -> Doc (Ann r))
  -> Bool
  -> Cxx.FunctionDeclaration
  -> Doc (Ann r)
helperFunctionName makeFnName withDetail fd = fromMaybe mempty $ do
  Cxx.FunctionDeclaration_key{..} <- getFactKey fd
  let shortName = makeFnName functionDeclaration_key_name
      (_preM, postM) = prettyMethodSignature functionDeclaration_key_method
      complete | withDetail = after postM $ shortName <> parens
        (prettyParamTypes functionDeclaration_key_signature)
               | otherwise = shortName
  pure complete

-- | If you want the function parameters as well then pass True, otherwise
-- pass False to omit the function parameters.
prettyShortFunctionName :: Bool -> Cxx.FunctionDeclaration -> Doc (Ann r)
prettyShortFunctionName = helperFunctionName prettyShortFnQName

-- | If you want the function parameters as well then pass True, otherwise
-- pass False to omit the function parameters.
prettyScopedFunctionName :: Bool -> Cxx.FunctionDeclaration -> Doc (Ann r)
prettyScopedFunctionName = helperFunctionName prettyScopedFnQName

-- | Return possible prefix (some of @virtual volatile@)
-- and possible suffix (some of @const & &&@) of method declaration
prettyMethodSignature
  :: Maybe Cxx.MethodSignature -> (Maybe (Doc (Ann r)), Maybe (Doc (Ann r)))
prettyMethodSignature Nothing = (mempty, mempty)
prettyMethodSignature (Just Cxx.MethodSignature{..}) =
  let virtual = [ txt keyword "virtual" | methodSignature_isVirtual ]
      volatile = [ txt keyword "volatile" | methodSignature_isVolatile ]
      const = [ txt keyword "const" | methodSignature_isConst ]
      refQualifier = case  methodSignature_refQualifier of
        Cxx.RefQualifier_None_ -> []
        Cxx.RefQualifier_LValue -> [ txt keyword "&" ]
        Cxx.RefQualifier_RValue -> [ txt keyword "&&" ]
        Cxx.RefQualifier__UNKNOWN{} -> []
      combine [] = Nothing
      combine xs = Just (hsep xs)
  in (combine virtual, combine (volatile <> const <> refQualifier))

-- | Re-usable rending of the scope::name and signature.
-- Always print the return type and fn name on a single line.
-- Function name becomes a link.
--
-- Inject references @[r]@ to allow for link to definition
prettyFunctionSignature :: [r] -> Cxx.FunctionDeclaration -> Doc (Ann r)
prettyFunctionSignature refTos fd = fromMaybe mempty $ do
  Cxx.FunctionDeclaration_key{..} <- getFactKey fd
  Cxx.FunctionQName_key{..} <- getFactKey functionDeclaration_key_name
  let withAccess = before (prettyScopeAccess functionQName_key_scope)
      (preM, postM) = prettyMethodSignature functionDeclaration_key_method
  pure $ nest 2 $ group $ withAccess $ before preM $ after postM $
    prettyRetType functionDeclaration_key_signature
      </> prettyFunctionDeclarationName refTos fd
      <> parens (line' <> prettyParams functionDeclaration_key_signature)

-- | Inject references @[r]@ to allow for link to definition
prettyEnumDecl :: [r] -> Cxx.EnumDeclaration -> Doc (Ann r)
prettyEnumDecl refTos decl = fromMaybe mempty $ do
  Cxx.EnumDeclaration_key{..} <- getFactKey decl
  let withAccess = prepend (prettyQNameAccess enumDeclaration_key_name)
      name = mayAct refTos $
        prettyScopedQName name_class enumDeclaration_key_name
  pure $ nest 2 $ fillSep $ withAccess $ concat
    [ [ txt keyword "enum" ]
    , [ txt keyword "class" | enumDeclaration_key_isScoped ]
    , [ name ]
    , case enumDeclaration_key_type of
        Just ty -> [txt operator ":", prettyType ty]
        Nothing -> [] ]

prettyEnumerator :: AnnMaker r -> Cxx.Enumerator -> Doc (Ann r)
prettyEnumerator am enumerator = fromMaybe mempty $ do
  Cxx.Enumerator_key{..} <- getFactKey enumerator
  pure $ mayAct (enumeratorRefTos am enumerator) $
    prettyName name_constant enumerator_key_name

-- | Includes name and values
prettyEnumDef :: AnnMaker r -> Cxx.EnumDefinition -> Doc (Ann r)
prettyEnumDef am def = fromMaybe mempty $ do
  Cxx.EnumDefinition_key{..} <- getFactKey def
  let vals = punctuate comma $ map (prettyEnumerator am)
        enumDefinition_key_enumerators
      refTos = defDeclRefTos am def enumDefinition_key_declaration
  pure $ prettyEnumDecl refTos enumDefinition_key_declaration
    </> txt operator "=" <> " " <> txt operator "[" </>
    nest 2 (fillSep (vals ++ [txt operator "]"]))

-- | This includes the attributes and type
prettyVariableDecl :: AnnMaker r -> Cxx.VariableDeclaration -> Doc (Ann r)
prettyVariableDecl am decl = fromMaybe mempty $ do
  Cxx.VariableDeclaration_key{..} <- getFactKey decl
  let withAccess = prepend (prettyQNameAccess variableDeclaration_key_name)
      name = mayAct (declRefTos am decl) $
        prettyScopedQName name_variable variableDeclaration_key_name
      leading = case variableDeclaration_key_kind of
        Cxx.VariableKind_global_ Cxx.GlobalVariable{..} ->
          let kind = case globalVariable_kind of
                Cxx.GlobalVariableKind_SimpleVariable -> []
                Cxx.GlobalVariableKind_StaticVariable ->
                  [ txt keyword "static" ]
                Cxx.GlobalVariableKind_StaticMember ->
                  [ txt keyword "static" ]
                Cxx.GlobalVariableKind__UNKNOWN{} -> []
              attribute = case globalVariable_attribute of
                Cxx.GlobalVariableAttribute_Plain -> []
                Cxx.GlobalVariableAttribute_Inline ->
                  [ txt keyword "inline" ]
                Cxx.GlobalVariableAttribute_Constexpr ->
                  [ txt keyword "constexpr" ]
                Cxx.GlobalVariableAttribute__UNKNOWN{} -> []
          in kind ++ attribute
        Cxx.VariableKind_field Cxx.Field{..} ->
          [ txt keyword "mutable" | field_mutable_ ]
        Cxx.VariableKind_ivar Cxx.ObjcIVar{..} ->
          [ txt keyword "@synthesize" | objcIVar_synthesize ]
        Cxx.VariableKind_EMPTY -> []
      trailing = case variableDeclaration_key_kind of
        Cxx.VariableKind_global_ {} -> []
        Cxx.VariableKind_field Cxx.Field{..} -> bs field_bitsize
        Cxx.VariableKind_ivar Cxx.ObjcIVar{..} -> bs objcIVar_bitsize
        Cxx.VariableKind_EMPTY-> []
        where
          bs Nothing = []
          bs (Just n) = [ txt operator ":"
            , txt literal_number_integer (showt (fromNat n)) ]
  pure $ nest 2 $ fillSep $ withAccess (leading ++
    (prettyType variableDeclaration_key_type : name : trailing))

-- | Okay, but not syntactic for function pointer typedefs
prettyTypeAliasDecl :: AnnMaker r -> Cxx.TypeAliasDeclaration -> Doc (Ann r)
prettyTypeAliasDecl am decl = fromMaybe mempty $ do
  Cxx.TypeAliasDeclaration_key{..} <- getFactKey decl
  let withAccess = prepend (prettyQNameAccess typeAliasDeclaration_key_name)
      name = mayAct (declRefTos am decl) $
        prettyScopedQName name_type typeAliasDeclaration_key_name
      typ = prettyType typeAliasDeclaration_key_type
  pure $ nest 2 $ fillSep $ withAccess $ case typeAliasDeclaration_key_kind of
    Cxx.TypeAliasKind_Typedef -> [txt keyword "typedef", typ, name]
    Cxx.TypeAliasKind_Using -> [txt keyword "using", name
      , txt operator "=", typ]
    Cxx.TypeAliasKind__UNKNOWN{} -> []

prettyRecordDecl :: [r] -> Cxx.RecordDeclaration -> Doc (Ann r)
prettyRecordDecl refTos decl = fromMaybe mempty $ do
  Cxx.RecordDeclaration_key{..} <- getFactKey decl
  let withAccess :: [Doc (Ann r)] -> [Doc (Ann r)]
      withAccess = prepend (prettyQNameAccess recordDeclaration_key_name)
      kind = anStyle keyword $
        prettyRecordKind recordDeclaration_key_kind
      name = mayAct refTos $ anStyle name_class $
        prettyScopedQName name_class recordDeclaration_key_name
  pure $ nest 2 $ fillSep $ withAccess [kind, name]

prettyNamespaceDecl :: [r] -> Cxx.NamespaceDeclaration -> Doc (Ann r)
prettyNamespaceDecl refTos def = fromMaybe mempty $ do
  Cxx.NamespaceDeclaration_key{..} <- getFactKey def
  pure $ nest 2 $ mayAct refTos $
    prettyScopedNamespaceQName namespaceDeclaration_key_name

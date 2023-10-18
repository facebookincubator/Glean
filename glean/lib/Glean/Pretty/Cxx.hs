{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Cxx
  ( simpleFnDeclK
  , breakTypeDoc
  , simpleVariableDeclaration
  , simpleTypeAliasDecl
  , simpleEnumDecl
  , prettyScopeNamespace
  , prettyFnQName, prettyScopedFnQName
  , prettyObjcMethodDeclSignature
  , prettyObjcPropertyDeclExplain
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import Safe (atMay)

import Glean
import Glean.Pretty.CxxAnn (clangRecordKind, getMaybeContainerName, breakType)
import Glean.Pretty.Src ()
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Schema.Pp1.Types as Pp
import Glean.Schema.Src.Types as Src
import Glean.Util.URI

intentionallyEmpty :: Doc ann
intentionallyEmpty = ""

instance Pretty Cxx.Entity where
  pretty (Cxx.Entity_decl decl) = pretty decl
  pretty (Cxx.Entity_defn defn) = pretty defn
  pretty (Cxx.Entity_enumerator en) = pretty en
  pretty (Cxx.Entity_objcSelectorSlot slot) = pretty slot
  pretty Cxx.Entity_EMPTY = intentionallyEmpty

instance Pretty Cxx.Definition where
  pretty (Cxx.Definition_function_ defn) = pretty defn
  pretty (Cxx.Definition_record_ defn) = pretty defn
  pretty (Cxx.Definition_enum_ defn) = pretty defn
  pretty (Cxx.Definition_objcMethod defn) = pretty defn
  pretty (Cxx.Definition_objcContainer defn) = pretty defn
  pretty (Cxx.Definition_variable defn) = pretty defn
  pretty (Cxx.Definition_namespace_ defn) = pretty defn
  pretty Cxx.Definition_EMPTY = intentionallyEmpty

instance Pretty Cxx.NamespaceDefinition where
  pretty defn =
    maybe mempty (pretty . Cxx.namespaceDefinition_key_declaration)
      (getFactKey defn)

instance Pretty Cxx.NamespaceDeclaration where
  pretty = prettyNamespaceDecl

instance Pretty Cxx.RecordDefinition where
  pretty = prettyRecDef

instance Pretty Cxx.RecordDeclaration where
  pretty = prettyRecDecl

instance Pretty EnumDeclaration where
  pretty = prettyEnumDecl

instance Pretty EnumDefinition where
  pretty = prettyEnumDef

instance Pretty Pp.Define where
  pretty = prettyMacroDecl

instance Pretty Cxx.FunctionDefinition where
  pretty = prettyFnDef

instance Pretty Cxx.FunctionDeclaration where
  pretty = prettyFnDecl

instance Pretty Cxx.VariableDeclaration where
  pretty = prettyVarDecl

instance Pretty TypeAliasDeclaration where
  pretty = prettyTypeAliasDecl

instance Pretty UsingDeclaration where
  pretty = prettyUsingDecl

instance Pretty UsingDirective where
  pretty = prettyUsingDirective

instance Pretty ObjcMethodDefinition where
  pretty defn = maybe mempty pretty $ getFactKey defn

instance Pretty ObjcContainerDefinition where
  pretty defn =
    maybe mempty (pretty . Cxx.objcContainerDefinition_key_declaration)
      (getFactKey defn)

instance Pretty ObjcMethodDeclaration where
  pretty = prettyObjcMethodDecl

instance Pretty ObjcSelectorSlotEntity where
  pretty = prettyObjcSelectorSlot

instance Pretty ObjcPropertyDeclaration where
  pretty = prettyObjcPropertyDecl

instance Pretty ObjcContainerDeclaration where
  pretty = prettyObjcContainerDecl

instance Pretty Cxx.Enumerator where
  pretty = prettyEnumerator

instance Pretty Cxx.ObjcSelector where
  pretty = prettyObjcSelector

instance Pretty Cxx.ObjcContainerId where
  pretty = prettyObjcContId

instance Pretty Cxx.Declaration where
  pretty (Cxx.Declaration_namespace_ ns) = prettyNamespaceDecl ns
  pretty (Cxx.Declaration_record_ recDecl) = prettyRecDecl recDecl
  pretty (Cxx.Declaration_enum_ enumDecl) = prettyEnumDecl enumDecl
  pretty (Cxx.Declaration_function_ fnDecl) = prettyFnDecl fnDecl
  pretty (Cxx.Declaration_variable vDecl) = prettyVarDecl vDecl
  pretty (Cxx.Declaration_typeAlias tDecl) = prettyTypeAliasDecl tDecl
  pretty (Cxx.Declaration_usingDeclaration uDecl) = prettyUsingDecl uDecl
  pretty (Cxx.Declaration_usingDirective uDir) = prettyUsingDirective uDir
  pretty (Cxx.Declaration_objcMethod method) = prettyObjcMethodDecl method
  pretty (Cxx.Declaration_objcProperty prop) = prettyObjcPropertyDecl prop
  pretty (Cxx.Declaration_objcContainer contDecl) =
    prettyObjcContainerDecl contDecl
  pretty Cxx.Declaration_EMPTY = intentionallyEmpty

instance Pretty Cxx.FunctionQName where
  pretty = fromMaybe mempty . prettyScopedFnQName

instance Pretty Cxx.QName where
  pretty = prettyQName

instance Pretty Cxx.Name where
  pretty = prettyName

instance Pretty Cxx.RecordKind where
  pretty = pretty . clangRecordKind

prettyRecDef :: Cxx.RecordDefinition -> Doc ann
prettyRecDef Cxx.RecordDefinition {
  recordDefinition_key = Just Cxx.RecordDefinition_key {
    recordDefinition_key_declaration = decl,
    recordDefinition_key_members = Cxx.Declarations {
      declarations_key = members }}}
  | Just memberDecls <- members = vsep [
      prettyRecDecl decl,
      nest 2 $ vsep ["Members:", prettyDecls memberDecls]]
  | Nothing <- members = prettyRecDecl decl
prettyRecDef _ = ""

prettyDecls :: [Cxx.Declaration] -> Doc ann
prettyDecls decls = vsep $ map (\decl -> pretty decl <> line) decls

prettyObjcMethodDecl :: Cxx.ObjcMethodDeclaration -> Doc ann
prettyObjcMethodDecl Cxx.ObjcMethodDeclaration {
  objcMethodDeclaration_key = Just Cxx.ObjcMethodDeclaration_key {
    objcMethodDeclaration_key_selector = selector,
    objcMethodDeclaration_key_container = contId,
    objcMethodDeclaration_key_signature = signature_,
    objcMethodDeclaration_key_source = range
  }} = vsep [
    sep [
      prettyRetType signature_,
      prettyObjcSelector selector,
      prettyObjcContId contId],
    "at" <+> pretty range
  ]
prettyObjcMethodDecl _ = ""

-- | Display well-formed objc method signature over a few lines, in ObjC style
prettyObjcMethodDeclSignature
  :: Cxx.ObjcMethodDeclaration_key -> Maybe (Doc ann)
prettyObjcMethodDeclSignature Cxx.ObjcMethodDeclaration_key {
    objcMethodDeclaration_key_selector = selector,
    objcMethodDeclaration_key_container = contId,
    objcMethodDeclaration_key_signature = signature_,
    objcMethodDeclaration_key_isInstance = isInstance
  } = do
    let classOrInstance | isInstance = "-"
                        | otherwise = "+"
        ret = lparen <> prettyRetType signature_ <> rparen
        prefix h = classOrInstance <+> ret <+> h
    sels <- Cxx.objcSelector_key selector
    params <- Cxx.signature_key_parameters <$> Cxx.signature_key signature_
    method <- case (sels, params) of
      ([h], []) -> Just $ prefix (pretty h)
      (ss, ps) | length ss == length ps -> case zipWith mergeSig ss ps of
        [] -> Nothing
        (h:t) -> Just $ nest 2 $ sep (prefix h : t)
      _ -> Nothing
    Just $ vsep [ method, "in" <+> prettyObjcContId contId ]
  where
    mergeSig s p = pretty s <> colon
      <> lparen <> prettyType (parameter_type p) <> rparen
      <> prettyName (parameter_name p)

prettyObjcSelector :: Cxx.ObjcSelector -> Doc ann
prettyObjcSelector Cxx.ObjcSelector { objcSelector_key = mSelector }
  | Nothing <- mSelector = mempty
  | Just selectors <- mSelector =
      case selectors of
        [] -> mempty
        nonEmpty -> fillSep $ map (\ h -> pretty h <> colon) nonEmpty

prettyObjcSelectorSlot :: Cxx.ObjcSelectorSlotEntity -> Doc ann
prettyObjcSelectorSlot (Cxx.ObjcSelectorSlotEntity method idx) =
  fromMaybe "" $ do
    decl <- case method of
      Cxx.ObjcMethodEntity_decl decl -> Just decl
      Cxx.ObjcMethodEntity_defn Cxx.ObjcMethodDefinition{
        objcMethodDefinition_key = Just decl} -> Just decl
      _ -> Nothing
    Cxx.ObjcMethodDeclaration_key{
      objcMethodDeclaration_key_selector = Cxx.ObjcSelector{
        objcSelector_key = Just selector
      },
      objcMethodDeclaration_key_locations = locations
    } <- Glean.getFactKey decl
    let index = fromIntegral $ Glean.fromNat idx
    name <- atMay selector index
    location <- atMay locations index
    return $ vsep [pretty name, "at" <+> pretty location]

prettyObjcPropertyDeclExplain :: Cxx.ObjcPropertyDeclaration_key -> Doc ann
prettyObjcPropertyDeclExplain Cxx.ObjcPropertyDeclaration_key {
    objcPropertyDeclaration_key_name = name, -- Name
    objcPropertyDeclaration_key_container = contId, -- ObjcContainerId
    objcPropertyDeclaration_key_type = type_ -- Type
  } = sep [
  prettyType type_,
  prettyName name,
  "in", prettyObjcContId contId]

prettyObjcPropertyDecl :: Cxx.ObjcPropertyDeclaration -> Doc ann
prettyObjcPropertyDecl Cxx.ObjcPropertyDeclaration {
  objcPropertyDeclaration_key = Just k@Cxx.ObjcPropertyDeclaration_key {
    objcPropertyDeclaration_key_source = range -- Range
  }
} = vsep [
  prettyObjcPropertyDeclExplain k,
  "at" <+> pretty range]
prettyObjcPropertyDecl _ = ""

prettyObjcContainerDecl :: Cxx.ObjcContainerDeclaration -> Doc ann
prettyObjcContainerDecl Cxx.ObjcContainerDeclaration {
  objcContainerDeclaration_key = Just Cxx.ObjcContainerDeclaration_key {
    objcContainerDeclaration_key_id = contId,
    objcContainerDeclaration_key_source = range
  }
} = vsep [prettyObjcContId contId, "at" <+> pretty range]
prettyObjcContainerDecl _ = ""

prettyObjcContId :: Cxx.ObjcContainerId -> Doc ann
prettyObjcContId contId
  | Cxx.ObjcContainerId_protocol name <- contId = prettyName name
  | Cxx.ObjcContainerId_interface_ name <- contId = prettyName name
  | Cxx.ObjcContainerId_extensionInterface name <- contId = prettyName name
  | Cxx.ObjcContainerId_implementation name <- contId = prettyName name
  | Cxx.ObjcContainerId_categoryInterface catId <- contId =
      prettyObjcCategoryId catId
  | Cxx.ObjcContainerId_categoryImplementation catId <- contId =
      prettyObjcCategoryId catId
  | Cxx.ObjcContainerId_EMPTY <- contId = intentionallyEmpty

prettyObjcCategoryId :: Cxx.ObjcCategoryId -> Doc ann
prettyObjcCategoryId Cxx.ObjcCategoryId {
  objcCategoryId_className = className,
  objcCategoryId_categoryName = catName
} = prettyName className <> "+" <> prettyName catName

prettyNamespaceDecl :: Cxx.NamespaceDeclaration -> Doc ann
prettyNamespaceDecl Cxx.NamespaceDeclaration { namespaceDeclaration_key = ns }
  | Just Cxx.NamespaceDeclaration_key {
      namespaceDeclaration_key_name = namespaceQName,
      namespaceDeclaration_key_source = namespaceSrc } <- ns =
        vsep [
          prettyScopeNamespace namespaceQName,
          "at" <+> pretty namespaceSrc]
  | otherwise = ""

prettyUsingDecl :: Cxx.UsingDeclaration -> Doc ann
prettyUsingDecl Cxx.UsingDeclaration { usingDeclaration_key = u }
  | Just Cxx.UsingDeclaration_key {
      usingDeclaration_key_name = name,
      usingDeclaration_key_source = source } <- u =
        vsep [
          "using" <+> fromMaybe "" (prettyFnQName name),
          "at" <+> pretty source]
  | otherwise = ""

prettyUsingDirective :: Cxx.UsingDirective -> Doc ann
prettyUsingDirective Cxx.UsingDirective { usingDirective_key = u }
  | Just Cxx.UsingDirective_key {
      usingDirective_key_name = name,
      usingDirective_key_source = source } <- u =
        vsep [
          "using namespace" <+> prettyQName name,
          "at" <+> pretty source]
  | otherwise = ""

prettyRecDecl :: Cxx.RecordDeclaration -> Doc ann
prettyRecDecl Cxx.RecordDeclaration { recordDeclaration_key = recDecl }
  | Just Cxx.RecordDeclaration_key {
      recordDeclaration_key_name = qname,
      recordDeclaration_key_kind = rkind,
      recordDeclaration_key_source = range
    } <- recDecl = vsep [
      pretty (clangRecordKind rkind) <+> prettyQName qname,
      "at" <+> pretty range,
      prettyDiffusion range]
  | Nothing <- recDecl = ""

simpleTypeAliasDecl :: Cxx.TypeAliasDeclaration -> Maybe (Doc ann)
simpleTypeAliasDecl tad = do
  tadk <- Cxx.typeAliasDeclaration_key tad
  let name = Cxx.typeAliasDeclaration_key_name tadk
      typ = Cxx.typeAliasDeclaration_key_type tadk
      kind = Cxx.typeAliasDeclaration_key_kind tadk
      prefix = case kind of
          Cxx.TypeAliasKind_Typedef -> "typedef"
          Cxx.TypeAliasKind_Using -> "using"
          Cxx.TypeAliasKind__UNKNOWN{} -> intentionallyEmpty
  return $ fillSep [prefix, prettyType typ, prettyQName name]

prettyTypeAliasDecl :: Cxx.TypeAliasDeclaration -> Doc ann
prettyTypeAliasDecl tad = fromMaybe "" $ do
  stad <- simpleTypeAliasDecl tad
  tadk <- Cxx.typeAliasDeclaration_key tad
  let source = Cxx.typeAliasDeclaration_key_source tadk
  return $ vsep [ stad, "at" <+> pretty source ]

simpleVariableDeclaration :: Cxx.VariableDeclaration -> Maybe (Doc ann)
simpleVariableDeclaration vd = do
  vdk <- Cxx.variableDeclaration_key vd
  let qn = Cxx.variableDeclaration_key_name vdk
      typ = Cxx.variableDeclaration_key_type vdk
  sqn <- simpleQName qn
  return $ fillSep [prettyType typ, sqn]

prettyVarDecl :: Cxx.VariableDeclaration -> Doc ann
prettyVarDecl vd = fromMaybe "" $ do
  svd <- simpleVariableDeclaration vd
  vdk <- Cxx.variableDeclaration_key vd
  let source = Cxx.variableDeclaration_key_source vdk
  return $ vsep [svd, "at" <+> pretty source]

prettyFnDef :: Cxx.FunctionDefinition -> Doc ann
prettyFnDef Cxx.FunctionDefinition { functionDefinition_key = fnDef }
  | Just Cxx.FunctionDefinition_key {
      functionDefinition_key_declaration = decl
    } <- fnDef = prettyFnDecl decl
  | Nothing <- fnDef = ""

prettyFnDecl :: Cxx.FunctionDeclaration -> Doc ann
prettyFnDecl Cxx.FunctionDeclaration { functionDeclaration_key = fnDecl }
  | Just fdk@Cxx.FunctionDeclaration_key {
      functionDeclaration_key_source = range
    } <- fnDecl
  , Just sig <- simpleFnDeclK fdk = vsep
      [ sig, "at" <+> pretty range, prettyDiffusion range ]
  | otherwise = ""

prettyEnumDef :: Cxx.EnumDefinition -> Doc ann
prettyEnumDef Cxx.EnumDefinition { enumDefinition_key = Just
  Cxx.EnumDefinition_key
    { enumDefinition_key_declaration = decl }} = pretty decl
prettyEnumDef _ = mempty

simpleEnumDecl :: Cxx.EnumDeclaration -> Maybe (Doc ann)
simpleEnumDecl ed = do
  edk <- Cxx.enumDeclaration_key ed
  let name = Cxx.enumDeclaration_key_name edk
      scoped = Cxx.enumDeclaration_key_isScoped edk
      maybeType = Cxx.enumDeclaration_key_type edk
      decl = hsep $ concat
       [ [ "enum" ]
       , [ "class" | scoped ]
       , [ prettyQName name ]
       , case maybeType of
           Just ty -> [":", prettyType ty]
           Nothing -> []
       ]
  return decl

prettyEnumDecl :: Cxx.EnumDeclaration -> Doc ann
prettyEnumDecl ed = fromMaybe "" $ do
  decl <- simpleEnumDecl ed
  edk <- Cxx.enumDeclaration_key ed
  let range = Cxx.enumDeclaration_key_source edk
  return $ vsep [ decl, "at" <+> pretty range, prettyDiffusion range ]

prettyMacroDecl :: Pp.Define -> Doc ann
prettyMacroDecl (Pp.Define _ (Just
  Pp.Define_key
    { define_key_macro = Pp.Macro {
        macro_key = Just n }
      , define_key_source = r})) =
  vsep [pretty n, "at", pretty r, prettyDiffusion r]
prettyMacroDecl _ = mempty

prettyEnumerator :: Cxx.Enumerator -> Doc ann
prettyEnumerator Cxx.Enumerator { enumerator_key = Just Cxx.Enumerator_key
  { enumerator_key_name = name
  , enumerator_key_source = range
  }} = vsep [ prettyName name, "at", pretty range, prettyDiffusion range ]
prettyEnumerator _ = mempty

-- | Re-usable rending of the scope::name and signature
simpleFnDeclK :: Cxx.FunctionDeclaration_key -> Maybe (Doc ann)
simpleFnDeclK fdk = do
  let fqn = Cxx.functionDeclaration_key_name fdk
      signature_ = Cxx.functionDeclaration_key_signature fdk
  prettyFQN <- prettyScopedFnQName fqn
  Just $ nest 2 $ group $
        -- Always print the return type and fn name on a single line.
        prettyRetType signature_
          <+> prettyFQN <> lparen <> line'
        -- Try to concat the parameters, if not possible put each on a
        -- nested newline.
         <> prettyParams signature_ <> rparen

prettyScopedFnQName :: Cxx.FunctionQName -> Maybe (Doc ann)
prettyScopedFnQName fqn = do
  fqnk <- Cxx.functionQName_key fqn
  let scope = functionQName_key_scope fqnk
      scopeColons = prettyScopeColons scope
  fnNameK <- Cxx.functionName_key (Cxx.functionQName_key_name fqnk)
  return (scopeColons <> prettyFnNameK (getMaybeContainerName scope) fnNameK)

-- | Shows the name without the scope
prettyFnQName :: Cxx.FunctionQName -> Maybe (Doc ann)
prettyFnQName Cxx.FunctionQName {
  functionQName_key = Just Cxx.FunctionQName_key {
    functionQName_key_scope = scope,
    functionQName_key_name = Cxx.FunctionName {
      functionName_key = Just fnk
    }}} = Just $ prettyFnNameK (getMaybeContainerName scope) fnk
prettyFnQName _ = Nothing

prettyFnNameK :: Maybe Text -> Cxx.FunctionName_key -> Doc ann
prettyFnNameK maybeContainerName fnk = case fnk of
  Cxx.FunctionName_key_name name -> prettyName name
  Cxx.FunctionName_key_operator_ x -> pretty x
  Cxx.FunctionName_key_literalOperator x -> pretty x
  Cxx.FunctionName_key_constructor _unit -> maybe "/* constructor */"
    (\n -> pretty n) maybeContainerName
  Cxx.FunctionName_key_destructor _unit -> maybe "/* destructor */"
    (\n -> pretty ("~" <> n)) maybeContainerName
  Cxx.FunctionName_key_conversionOperator t -> parens (prettyType t)
  Cxx.FunctionName_key_EMPTY -> intentionallyEmpty

prettyScopeColons :: Cxx.Scope -> Doc ann
prettyScopeColons (Cxx.Scope_namespace_ namespaceQName) =
  prettyScopeNamespace namespaceQName <> "::"
prettyScopeColons (Cxx.Scope_recordWithAccess recordWithAccess) =
  prettyScopeRecordWithAccess recordWithAccess <> "::"
prettyScopeColons _ = mempty

prettyScopeNamespace :: Cxx.NamespaceQName -> Doc ann
prettyScopeNamespace Cxx.NamespaceQName {
  namespaceQName_key = Just Cxx.NamespaceQName_key {
    namespaceQName_key_name = mName,
    namespaceQName_key_parent = mParent }} =
      let parent = case mParent of
            Nothing -> mempty
            Just ns -> prettyScopeNamespace ns <> "::"
          here = case mName of
            Nothing -> "{anonymous}"
            Just name -> prettyName name
      in parent <> here
prettyScopeNamespace _ = ""

prettyScopeRecordWithAccess :: Cxx.Scope_recordWithAccess_ -> Doc ann
prettyScopeRecordWithAccess Cxx.Scope_recordWithAccess_ {
  Cxx.scope_recordWithAccess__record = qname } = prettyQName qname

simpleQName :: Cxx.QName -> Maybe (Doc ann)
simpleQName qn = do
  qnk <- Cxx.qName_key qn
  let name = Cxx.qName_key_name qnk
      scope = Cxx.qName_key_scope qnk
  return (prettyScopeColons scope <> prettyName name)

prettyQName :: Cxx.QName -> Doc ann
prettyQName = fromMaybe "" . simpleQName

prettyRetType :: Cxx.Signature -> Doc ann
prettyRetType Signature { signature_key = signature_ }
  | Just Cxx.Signature_key { signature_key_returns = type_ } <- signature_ =
      prettyType type_
  | Nothing <- signature_ = ""

-- Pretty prints function parameters, e.g.
--   int * argc, char *** argv, _Bool removeFlags
-- with multi-line support for long names, e.g.
--  int * argc,
--  char *** argv,
--  _Bool removeFlags
prettyParams :: Cxx.Signature -> Doc ann
prettyParams Signature { signature_key = signature_ }
  | Just Cxx.Signature_key { signature_key_parameters = params } <- signature_ =
      sep $ punctuate comma $ map prettyParam params
  | Nothing <- signature_ = ""

prettyParam :: Cxx.Parameter -> Doc ann
prettyParam Cxx.Parameter { parameter_name = name, parameter_type = type_ } =
  prettyType type_ <+> prettyName name

prettyDiffusion :: Src.Range -> Doc ann
prettyDiffusion range = pretty $ show <$> fbsDiffusionURIForRange range

prettyName :: Cxx.Name -> Doc ann
prettyName Name { name_key = name } = pretty name

-- | Apply 'breakType' using the page width as suggested limit.
breakTypeDoc :: Text -> Doc ann
breakTypeDoc t = pageWidth $ \ pw ->
    let limit = case pw of
          Unbounded -> 80
          AvailablePerLine w _ -> w
    in either pretty assemble (breakType limit t)
  where
    bump = 4
    assemble xs = (nest bump . fillSep . map pretty $ xs) <> hardline

-- | Attempt to break long types at desired punctuation.
prettyType :: Cxx.Type -> Doc ann
prettyType Type { type_key = m } = maybe "" breakTypeDoc m

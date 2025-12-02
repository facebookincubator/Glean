{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}

module AngleIndexer.Builder (buildFacts) where


import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Glean.Schema.Builtin.Types ( Unit(..))
import qualified Glean.Schema.Src.Types as Src
import Glean
import Glean.Database.Config
  (ProcessedSchema (..))
import Glean.Angle.Types
import Glean.Schema.Types
import qualified Glean.Schema.Anglelang.Types as Anglelang

import AngleIndexer.Utils

-- Combining all info needed to build facts for an angle schema
-- such that we can easily iterate them on a "per schema" basis.
data AngleSchema = AngleSchema {
 rSchema :: ResolvedSchemaRef,
 fpath :: FilePath,
 toByteSpan :: ToByteSpan,
 imports :: [ImportDecl],
 schemaSpan :: SrcSpan
}

data ImportDecl = ImportDecl
  { importName    :: Name
  , importVersion :: Version
  , importSrcSpan :: SrcSpan
  }


type DeclBuilder a =
  forall m. (NewFact m) => a -> m (Anglelang.Declaration, SrcSpan)

buildFacts :: ProcessedSchema -> [SourceFileInfo] -> FactBuilder
buildFacts ProcessedSchema{..} fileinfos = do
  schemaFileInfo <- mapM buildFileLines fileinfos
  let schemaFileInfoMap = toFileInfoMap schemaFileInfo
      resolvedSchemaMap = toResolvedSchemaMap procSchemaResolved
      toDef = lookupQualRef resolvedSchemaMap

  schemas <- mapM (\sSchema -> do
    let name = sourceRefName $ schemaName sSchema
        srcSpan = schemaSrcSpan sSchema
        -- we don't have import statements in ResolvedSchema,
        -- so we get them from SourceSchema but use resolved version
        imports = mapMaybe (\d -> case d of
          SourceImport (SourceRef n v) s -> case v of
              Just v' -> Just $ ImportDecl n v' s
              Nothing -> case HashMap.lookup n resolvedSchemaMap of
                Just resSchema -> Just $ ImportDecl n v' s
                  where v' = resolvedSchemaVersion resSchema
                Nothing -> fail $ "Couldn't find resolved schema: " <> show n
          _ -> Nothing
          ) $ schemaDecls sSchema
    rSchema <- case HashMap.lookup name resolvedSchemaMap of
      Just rSchema -> return rSchema
      Nothing -> fail $ "Couldn't find resolved schema" <> show name
    (file, toBs) <- case HashMap.lookup name schemaFileInfoMap of
      Just SchemaFileInfo{..} -> return (filepath sourceFileInfo, toByteSpan)
      Nothing -> fail $ "Couldn't find file info for schema " <> show name
    return $ AngleSchema rSchema file toBs imports srcSpan
    ) $ srcSchemas procSchemaSource

  mapM_ (`buildSchemaFacts` toDef) schemas

buildFileLines ::forall m. NewFact m => SourceFileInfo -> m SchemaFileInfo
buildFileLines fileInfo = do
  let SourceFileInfo _name filepath bytestr = fileInfo
      byteLines = BC.lines bytestr
      byteLinesWithNewLine = map (<> "\n") byteLines
      byteFileLines = map (Glean.Nat . fromIntegral . BC.length)
        byteLinesWithNewLine

  file <- makeFact @Src.File (Text.pack filepath)
  let fileLinesKey = Src.FileLines_key
          { fileLines_key_file = file
          , fileLines_key_lengths = byteFileLines
          , fileLines_key_endsInNewline = True
          , fileLines_key_hasUnicodeOrTabs = False
          }
  makeFact_ @Src.FileLines fileLinesKey

  -- When writing the later facts, we need:
  --   filepath
  --   A function: SrcSpan -> ByteSpan.
  let toByteSpan = fromSrcSpan fileLinesKey bytestr
  return $ SchemaFileInfo fileInfo toByteSpan

buildTypeFact :: forall m. (NewFact m) => Type_ SrcSpan PredicateRef TypeRef
  -> m Anglelang.Type
buildTypeFact typeDef = do
  typeFact <- case typeDef of
      ByteTy ->  return (Anglelang.Type_key_byte_ Unit)
      NatTy ->  return (Anglelang.Type_key_nat_ Unit)
      BooleanTy ->  return (Anglelang.Type_key_boolean_ Unit)
      StringTy -> return (Anglelang.Type_key_string_ Unit)
      ArrayTy type_ -> Anglelang.Type_key_array_ <$> buildTypeFact type_
      SetTy type_ -> Anglelang.Type_key_set_ <$> buildTypeFact type_
      RecordTy fieldDefs ->
        Anglelang.Type_key_record_ <$> mapM buildField fieldDefs
      SumTy fieldDefs ->
        Anglelang.Type_key_sum_ <$> mapM buildField fieldDefs
      PredicateTy _ (PredicateRef n v) -> do
        Anglelang.Type_key_predicate_ <$> buildNameFact n v
      NamedTy _ (TypeRef n v) -> do
        Anglelang.Type_key_named_ <$> buildNameFact n v
      MaybeTy tref -> do
        Anglelang.Type_key_maybe_ <$> buildTypeFact tref
      EnumeratedTy names -> Anglelang.Type_key_enum_ <$> mapM buildName names
      _ -> fail $ "Couldn't build fact of unexpected type: " <> show typeDef
  makeFact @Anglelang.Type typeFact

  where
    buildField fieldDef = do
        name <- makeFact @Anglelang.Name $ fieldDefName fieldDef
        ty <- buildTypeFact $ fieldDefType fieldDef
        return $ Anglelang.Field name ty
    buildName name = makeFact @Anglelang.Name name


buildNameFact ::forall m. (NewFact m) =>  Name -> Version -> m Anglelang.Name
buildNameFact name v = makeFact @Anglelang.Name $ name
  <> (Text.pack . ("." ++) . show ) v

buildSchemaFacts :: AngleSchema -> ToDef -> FactBuilder
buildSchemaFacts AngleSchema{rSchema = rs@ResolvedSchema{..}, ..} toDef  = do
  let preds = HashMap.elems resolvedSchemaPredicates
      types = HashMap.elems resolvedSchemaTypes
      derives = HashMap.elems resolvedSchemaDeriving
      sourceRefs = findXRefs rs

  fileFact <- makeFact @Src.File $ Text.pack fpath

  -- build declarations
  impDeclFacts <-
    mapM (\d -> buildDecl buildImportDeclFacts d toByteSpan fileFact ) imports
  predDeclFacts <-
    mapM (\p -> buildDecl buildPredDeclFact p toByteSpan fileFact ) preds
  typeDeclFacts <-
    mapM (\t -> buildDecl buildTypeDeclFact t toByteSpan fileFact ) types
  deriveDeclFacts <-
    mapM (\d -> buildDecl buildDeriveDeclFact d toByteSpan fileFact ) derives
  let allDecls =
        impDeclFacts <> predDeclFacts <> typeDeclFacts <> deriveDeclFacts
  _ <- buildDecl buildSchemaDeclFact
    (resolvedSchemaName, resolvedSchemaVersion, schemaSpan, allDecls)
      toByteSpan fileFact

  -- build XRefs
  xrefs <- mapM (\xref -> buildXRef xref toDef toByteSpan) sourceRefs
  buildFileXRefsFact xrefs fileFact

buildDecl
  :: forall a m. (NewFact m) => DeclBuilder a
  -> a
  -> ToByteSpan
  -> Src.File
  -> m Anglelang.DeclarationLocation
buildDecl declBuilder def toByteSpan fileFact = do
  (decl, srcSpan) <- declBuilder def
  makeFact @Anglelang.DeclarationLocation $
    Anglelang.DeclarationLocation_key decl fileFact (toByteSpan srcSpan)

buildSchemaDeclFact ::
  DeclBuilder (Name, Version, SrcSpan, [Anglelang.DeclarationLocation])
buildSchemaDeclFact (name, version, srcSpan, declLocs) = do
  nameFact <- buildNameFact name version
  schemaFact <- makeFact @Anglelang.SchemaDecl
    $ Anglelang.SchemaDecl_key nameFact decls
  return (Anglelang.Declaration_schema schemaFact, srcSpan)
  where
    decls = mapMaybe toDecl declLocs
    toDecl = fmap Anglelang.declarationLocation_key_decl
      . Anglelang.declarationLocation_key

buildTypeDeclFact :: DeclBuilder ResolvedTypeDef
buildTypeDeclFact TypeDef{typeDefRef = TypeRef{..}, ..} = do
  name <- buildNameFact typeRef_name typeRef_version
  ty <- buildTypeFact typeDefType
  typeDecl <- makeFact @Anglelang.TypeDecl $ Anglelang.TypeDecl_key name ty
  return (Anglelang.Declaration_ty typeDecl, typeDefSrcSpan)

buildPredDeclFact :: DeclBuilder ResolvedPredicateDef
buildPredDeclFact PredicateDef{predicateDefRef = PredicateRef{..}, ..} = do
  let derived = toDeriveEnum predicateDefDeriving
  name <- buildNameFact predicateRef_name predicateRef_version
  keyTy <- buildTypeFact predicateDefKeyType
  valTy <- buildTypeFact predicateDefValueType
  predDecl <- makeFact @Anglelang.PredicateDecl $
    Anglelang.PredicateDecl_key name keyTy valTy derived

  return (Anglelang.Declaration_pred predDecl, predicateDefSrcSpan)

toDeriveEnum :: DerivingInfo q -> Anglelang.DeriveInfo
toDeriveEnum = \case
  NoDeriving -> Anglelang.DeriveInfo_NoDeriving
  Derive DeriveOnDemand _ -> Anglelang.DeriveInfo_OnDemand
  Derive DerivedAndStored _ -> Anglelang.DeriveInfo_Stored
  Derive DeriveIfEmpty _ -> Anglelang.DeriveInfo_IfEmpty

buildImportDeclFacts :: DeclBuilder ImportDecl
buildImportDeclFacts ImportDecl{..} = do
  name <- buildNameFact importName importVersion
  return (Anglelang.Declaration_imp name, importSrcSpan)

buildDeriveDeclFact :: DeclBuilder ResolvedDerivingDef
buildDeriveDeclFact DerivingDef{..}= do
  let prefName = predicateRef_name derivingDefRef
      prefVersion = predicateRef_version derivingDefRef
      deriveInfo = toDeriveEnum derivingDefDeriveInfo
  name <- buildNameFact prefName prefVersion
  deriveDecl <- makeFact @Anglelang.DerivingDecl $
    Anglelang.DerivingDecl_key name deriveInfo
  return (Anglelang.Declaration_derive_ deriveDecl, derivingDefSrcSpan)

findTypeXRefs :: Type_ SrcSpan PredicateRef TypeRef -> [(Ref, SrcSpan)]
findTypeXRefs = \case
  ByteTy -> []
  NatTy ->  []
  BooleanTy -> []
  StringTy -> []
  ArrayTy type_ -> findTypeXRefs type_
  SetTy type_ -> findTypeXRefs type_
  MaybeTy type_ -> findTypeXRefs type_
  RecordTy fields -> concatMap fieldRefs fields
  SumTy fields -> concatMap fieldRefs fields
  PredicateTy s pref -> [ (Pred pref, s) ]
  NamedTy s tref -> [ (Ty tref, s) ]
  EnumeratedTy _ -> [] -- todo: add enums refs
  _ -> []
  where
    fieldRefs (FieldDef _ ty)  = findTypeXRefs ty

buildXRef ::
  forall m. (NewFact m) =>
    XRef -> ToDef -> ToByteSpan -> m Anglelang.XRef
buildXRef (ref, spans) toDef toBs = do
  decl <- case toDef ref of
    Just (PredDef d) -> fst <$> buildPredDeclFact d
    Just (TyDef d) -> fst <$> buildTypeDeclFact d
    _ -> fail $ "Couldn't find declaration for xref: " ++ show ref

  let bytespans = map toBs spans
      xreftarget = Anglelang.XRefTarget decl
  return $ Anglelang.XRef xreftarget bytespans

buildFileXRefsFact :: [Anglelang.XRef] -> Src.File ->  FactBuilder
buildFileXRefsFact xRefs file =
  makeFact_ @Anglelang.FileXRefs $ Anglelang.FileXRefs_key file xRefs


findPatXRefs :: ResolvedPat -> [(Ref, SrcSpan)]
findPatXRefs = \case
  Glean.Angle.Types.Nat _ _ -> []
  String _ _ -> []
  StringPrefix _ _ -> []
  ByteArray _ _ -> []
  Wildcard _ -> []
  Variable _ _ -> []
  FactId{} -> []
  Never _ -> []
  Enum _ _ -> []
  Array _ pats -> concatMap findPatXRefs pats
  ArrayPrefix _ pats -> concatMap findPatXRefs pats
  Tuple _ pats -> concatMap findPatXRefs pats
  Struct _ fields -> concatMap findFieldXRefs fields
  App _ pat pats -> findPatXRefs pat ++ concatMap findPatXRefs pats
  KeyValue _ pat pat' -> findPatXRefs pat ++ findPatXRefs pat'
  Elements _ pat -> findPatXRefs pat
  All _ pat -> findPatXRefs pat
  ElementsOfArray _ pat -> findPatXRefs pat
  OrPattern _ pat pat' -> findPatXRefs pat ++ findPatXRefs pat'
  NestedQuery _ query -> findQueryXRefs query
  Negation _ pat -> findPatXRefs pat
  TypeSignature _ pat ty -> findPatXRefs pat ++ findTypeXRefs ty
  IfPattern _ x y z -> findPatXRefs x ++ findPatXRefs y ++ findPatXRefs z
  FieldSelect _ pat _ _ -> findPatXRefs pat
  Deref _ pat -> findPatXRefs pat
  Clause _ s pref pat _ -> (Pred pref,s) : findPatXRefs pat
  Prim _ _ pats -> concatMap findPatXRefs pats
  where
    findFieldXRefs (Field _ pat) = findPatXRefs pat

findQueryXRefs :: ResolvedQuery -> [(Ref, SrcSpan)]
findQueryXRefs (SourceQuery head stms _) =
  findHeadXRefs head ++ concatMap findStmXrefs stms
  where
    findHeadXRefs = maybe [] findPatXRefs
    findStmXrefs (SourceStatement pat pat') =
     findPatXRefs pat ++ findPatXRefs pat'

findDeriveXRefs :: ResolvedDerivingDef -> [(Ref, SrcSpan)]
findDeriveXRefs DerivingDef{..} = do
  case derivingDefDeriveInfo of
    NoDeriving -> []
    Derive _ query -> findQueryXRefs query


findXRefs :: ResolvedSchemaRef -> [XRef]
findXRefs ResolvedSchema{..} = do
  let preds = HashMap.elems resolvedSchemaPredicates
      types = HashMap.elems resolvedSchemaTypes
      derives = HashMap.elems resolvedSchemaDeriving
      xrefs = concatMap (findTypeXRefs . predicateDefKeyType) preds ++
        concatMap (findTypeXRefs . typeDefType) types ++
        concatMap findDeriveXRefs derives
      grouped = groupBy (\(k,_)(k',_) -> k == k') xrefs
  map (\xrefs -> (fst $ head xrefs, map snd xrefs)) grouped

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}

module Glean.Indexer.Angle
  (main
  ) where


import Data.Default
import Data.Maybe (mapMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.List (groupBy)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Exception
import Options.Applicative
import System.FilePath ( (</>))
import Util.EventBase
import Util.IO (listDirectoryRecursive)

import Glean
import Glean.Database.Config
  (ProcessedSchema (..), processSchema, catSchemaFiles)
import Glean.LocalOrRemote
import Glean.Util.ConfigProvider
import Glean.Impl.ConfigProvider
import Glean.Schema.Builtin.Types (schema_id, Unit(..))
import Glean.Indexer.Utils
import Glean.Angle.Types
import Glean.Schema.Types
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Src as Src (allPredicates)

import qualified Glean.Schema.Anglelang.Types as Anglelang
import qualified Glean.Schema.Anglelang as Anglelang (allPredicates)

data Command = CmdIndex Options | CmdSchemaId
data Options = Options
  { optDir ::  String
  , optRepoPath :: String
  , optDb :: Glean.Repo
  , optService :: Service
  }

data ImportDecl = ImportDecl
  { importName    :: Name
  , importVersion :: Version
  , importSrcSpan :: SrcSpan
  }

-- Combining all info needed to build facts for an angle schema
-- such that we can easily iterate them on a "per schema" basis.
data AngleSchema = AngleSchema {
 rSchema :: ResolvedSchemaRef,
 fpath :: FilePath,
 toByteSpan :: ToByteSpan,
 imports :: [ImportDecl]
}

type DeclBuilder a =
  forall m. (NewFact m) => a -> m (Anglelang.Declaration, SrcSpan)

data Def = PredDef ResolvedPredicateDef | TyDef ResolvedTypeDef
type ToDef = Ref -> Maybe Def
data Ref = Pred PredicateRef | Ty TypeRef
  deriving (Eq, Ord, Show)
type XRef = (Ref, [SrcSpan])

opts :: ParserInfo Command
opts = info (helper <*> parser) fullDesc
  where
    parser :: Parser Command
    parser = subparser
      ( command "index"
        ( info (CmdIndex <$> idxParser)
        ( progDesc "Run angle indexer for all files in the given directory" ))
      <> command "schema-id"
        ( info (pure CmdSchemaId)
        ( progDesc "Get schema id. Returns scehma id and exits" )))

    idxParser :: Parser Options
    idxParser = do
      optDir <- strOption
        (long "dir"
        <> metavar "DIR"
        <> help "dir of schemas to index. Relative to the repo-path")
      optDb <- option (maybeReader Glean.parseRepo)
        (  long "db"
        <> metavar "NAME"
        <> help "database to write facts to" )
      optRepoPath <- strOption
        (long "repo-path"
        <> metavar "REPO-PATH"
        <> help ("path to repo root."
          <> "indexed files will have names relative this root"
          )
        )
      optService <- options
      return Options{..}

qualRefToSchemaName :: Text -> Text
qualRefToSchemaName name = do
  case reverse $ Text.splitOn "." name of
    [] -> name -- we assume it's a schema name only (case for import refs)
    (_:xs) -> Text.intercalate "." (reverse xs)

lookupQualRef :: HashMap.HashMap Name ResolvedSchemaRef -> Ref -> Maybe Def
lookupQualRef schemas ref = do
  let schemaName = qualRefToSchemaName $ case ref of
        Pred pref -> predicateRef_name pref
        Ty tref -> typeRef_name tref
  case HashMap.lookup schemaName schemas of
    Just ResolvedSchema{..} -> do
      case ref of
          Pred pref -> case HashMap.lookup pref resolvedSchemaPredicates of
            Just p -> Just $ PredDef p
            Nothing ->
              PredDef <$> HashMap.lookup pref resolvedSchemaReExportedPredicates
          Ty tref -> case HashMap.lookup tref resolvedSchemaTypes of
            Just t -> Just $ TyDef t
            Nothing ->
              TyDef <$> HashMap.lookup tref resolvedSchemaReExportedTypes
    _ -> Nothing

buildFacts :: ProcessedSchema -> [SourceFileInfo] -> FactBuilder
buildFacts ProcessedSchema{..} fileinfos = do
  schemaFileInfo <- mapM buildFileLines fileinfos
  let schemaFileInfoMap = toFileInfoMap schemaFileInfo
      resolvedSchemaMap = toResolvedSchemaMap procSchemaResolved
      toDef = lookupQualRef resolvedSchemaMap

  schemas <- mapM (\sSchema -> do
    let name = sourceRefName $ schemaName sSchema
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
    return $ AngleSchema rSchema file toBs imports
    ) $ srcSchemas procSchemaSource

  mapM_ (`buildSchemaFacts` toDef) schemas

buildSchemaFacts :: AngleSchema -> ToDef -> FactBuilder
buildSchemaFacts AngleSchema{rSchema = rs@ResolvedSchema{..}, ..} toDef = do
  let preds = HashMap.elems resolvedSchemaPredicates
      types = HashMap.elems resolvedSchemaTypes
      derives = HashMap.elems resolvedSchemaDeriving
      sourceRefs = findXRefs rs

  fileFact <- makeFact @Src.File $ Text.pack fpath

  -- build declarations
  mapM_ (\d -> buildDecl buildImportDeclFacts d toByteSpan fileFact ) imports
  mapM_ (\p -> buildDecl buildPredDeclFact p toByteSpan fileFact ) preds
  mapM_ (\t -> buildDecl buildTypeDeclFact t toByteSpan fileFact ) types
  mapM_ (\d -> buildDecl buildDeriveDeclFact d toByteSpan fileFact ) derives
  -- TODO build schema decl facts (so we can "go-to" def from import stms)

  -- build XRefs
  xrefs <- mapM (\xref -> buildXRef xref toDef toByteSpan) sourceRefs
  buildFileXRefsFact xrefs fileFact

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

buildDecl :: DeclBuilder a -> a -> ToByteSpan -> Src.File -> FactBuilder
buildDecl declBuilder def toByteSpan fileFact = do
  (decl, srcSpan) <- declBuilder def
  makeFact_ @Anglelang.DeclarationLocation $
    Anglelang.DeclarationLocation_key decl fileFact (toByteSpan srcSpan)

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

readSchemas :: FilePath -> IO ProcessedSchema
readSchemas dir = do
  files <- listDirectoryRecursive dir
  bytestr <- catSchemaFiles files
  case processSchema Nothing bytestr of
    Left err -> throwIO $ ErrorCall err
    Right schema -> return schema

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


send :: Backend be => be -> ProcessedSchema -> Options -> IO ()
send be schemas opts = do
  -- pre-processing before building facts
  fileInfos <- sourceFileInfos (optRepoPath opts) (optDir opts)

  withSender be (optDb opts) refs def $ \sender ->
    withWriter sender def $ \writer ->
      writeFacts writer (buildFacts schemas fileInfos)

    where
      refs = [Src.allPredicates, Anglelang.allPredicates]

main :: IO ()
main = do
  withConfigOptions opts $ \(cmd, cfgOpts) -> do
    case cmd  of
      CmdSchemaId -> putStrLn $ LBS.unpack $ Aeson.encodePretty $
              Aeson.object ["schema_id" Aeson..= Aeson.toJSON schema_id]
      CmdIndex opts -> do
        let service = optService opts
            indexDir = optRepoPath opts </> optDir opts
        schemas <- readSchemas indexDir
        withEventBaseDataplane $ \evb ->
          withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
          withBackendWithDefaultOptions evb cfgAPI service (Just schema_id)
            $ \backend -> do
              send backend schemas opts

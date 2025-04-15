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
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
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


opts :: ParserInfo Options
opts = info (helper <*> parser) fullDesc
  where
    parser :: Parser Options
    parser = do
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

buildFacts :: ProcessedSchema -> [SourceFileInfo] -> FactBuilder
buildFacts ProcessedSchema{..} fileinfos = do
  schemaFileInfo <- mapM buildFileLines fileinfos
  let schemaFileInfoMap = toFileInfoMap schemaFileInfo
      resolvedSchemaMap = toResolvedSchemaMap procSchemaResolved

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

  mapM_ buildSchemaFacts schemas

buildSchemaFacts :: AngleSchema -> FactBuilder
buildSchemaFacts AngleSchema{..} = do
  let preds = map snd $ HashMap.toList $ resolvedSchemaPredicates rSchema
  let types = map snd $ HashMap.toList $ resolvedSchemaTypes rSchema
  fileFact <- makeFact @Src.File $ Text.pack fpath

  mapM_ (\d -> buildDecl buildImportDeclFacts d toByteSpan fileFact ) imports
  mapM_ (\p -> buildDecl buildPredDeclFact p toByteSpan fileFact ) preds
  mapM_ (\t -> buildDecl buildTypeDeclFact t toByteSpan fileFact ) types
  -- TODO: deriving decls
  -- TODO: all refs


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

  where toDeriveEnum deriveInf = case deriveInf of
          NoDeriving -> Anglelang.DeriveInfo_NoDeriving
          Derive DeriveOnDemand _ -> Anglelang.DeriveInfo_OnDemand
          Derive DerivedAndStored _ -> Anglelang.DeriveInfo_Stored
          Derive DeriveIfEmpty _ -> Anglelang.DeriveInfo_IfEmpty

buildImportDeclFacts :: DeclBuilder ImportDecl
buildImportDeclFacts ImportDecl{..} = do
  name <- buildNameFact importName importVersion
  return (Anglelang.Declaration_imp name, importSrcSpan)

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



buildTypeFact :: forall m. (NewFact m) => Type_ PredicateRef TypeRef
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
      PredicateTy (PredicateRef n v) -> do
        Anglelang.Type_key_predicate_ <$> buildNameFact n v
      NamedTy (TypeRef n v) -> do
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
  withConfigOptions opts $ \(opts, cfgOpts) -> do
    let service = optService opts
        indexDir = optRepoPath opts </> optDir opts
    schemas <- readSchemas indexDir
    withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
      withBackendWithDefaultOptions evb cfgAPI service (Just schema_id)
        $ \backend -> do
          send backend schemas opts

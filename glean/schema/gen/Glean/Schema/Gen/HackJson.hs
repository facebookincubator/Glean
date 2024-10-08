{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Schema.Gen.HackJson
  ( genSchemaHackJson
  ) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict hiding (Sum)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Char as Char
import Data.Graph
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as TextBuilder
import GHC.Generics

import Util.Text (textShow)

import Glean.Angle.Types hiding (Struct, KeyValue)
import Glean.Schema.Types

data HackGenData = HackGenData
  { generated :: Text
  , namespace :: Text
  , genTypes :: HashMap Text GenType
  , enums :: HashMap Text [(Text, Text)]
  } deriving (Generic)

instance Aeson.ToJSON HackGenData where

data GenType
  = ClassType ClassGenType
  | EnumType [Text]
  | AliasType AngleTypeRepr
  deriving (Generic)

instance Aeson.ToJSON GenType where

data ReferenceTag
  = PredicateReference
  | TypeReference
  deriving (Generic,Eq)

instance Hashable ReferenceTag where
instance Aeson.ToJSON ReferenceTag where

data VersionedReference
  = Reference
    { refTag :: ReferenceTag
    , refName :: Text
    , refVersion :: Version
    , refLatest :: Bool
    } deriving (Generic,Eq)

instance Aeson.ToJSON VersionedReference where
instance Hashable VersionedReference where

data ClassGenType
  = Struct ClassGenTypeData
  | Union ClassGenTypeData
  | Wrapper ClassGenTypeData
  | KeyValue ClassGenTypeData
  deriving (Generic)

instance Aeson.ToJSON ClassGenType where

data ClassGenTypeData = ClassGenTypeData
  { classGenReference :: VersionedReference
  , classGenHasCycles :: Bool
  , classGenFieldInfos :: [FieldInfo]
  } deriving (Generic)
instance Aeson.ToJSON ClassGenTypeData where

data FieldInfo = FieldInfo
  { fieldInfoName :: Text
  , fieldInfoType :: AngleTypeRepr
  } deriving (Generic)

instance Aeson.ToJSON FieldInfo where

data Context = Context
  { contextCycles :: HashMap VersionedReference Bool
  , contextPredMap :: HashMap PredicateRef ResolvedPredicateDef
  , contextTypeMap :: HashMap TypeRef ResolvedTypeDef
  , contextPredLatest :: HashMap Text Version
  , contextTypeLatest :: HashMap Text Version
  }

type HackEnum = (Text, [(Text, Text)])

genSchemaHackJson
  :: Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> [(FilePath,Text)]
genSchemaHackJson _version preddefs typedefs = HashMap.toList files
  where
    predMap = HashMap.fromList $ map (\p -> (predicateDefRef p, p)) preddefs
    typeMap = HashMap.fromList $ map (\t -> (typeDefRef t, t)) typedefs
    predLatest = HashMap.fromListWith max $
      flip map preddefs $
        \PredicateDef{predicateDefRef=PredicateRef{..}} ->
          (predicateRef_name , predicateRef_version)
    typeLatest = HashMap.fromListWith max $
      flip map typedefs $
        \TypeDef{typeDefRef=TypeRef{..}} ->
          (typeRef_name, typeRef_version)
    cyclesMap = HashMap.fromList $ cyclesInDefs ctx allDefs
    ctx = Context
      { contextCycles = cyclesMap
      , contextPredMap = predMap
      , contextTypeMap = typeMap
      , contextPredLatest = predLatest
      , contextTypeLatest = typeLatest
      }
    allDefs = (++)
      [Left preddef | preddef <- preddefs ]
      [Right typedef | typedef <- typedefs ]
    fileDefs = HashMap.fromListWith (++) $
      map (\def -> (defFile def, [def])) allDefs
    files = HashMap.mapWithKey f fileDefs
    f file defs = Text.Lazy.toStrict $ TextBuilder.toLazyText $
      Aeson.encodePrettyToTextBuilder genData
      where
        (genTypes, enums) = runWriter $ mapM (genTypeOfDef ctx) defs
        genData = HackGenData
          { generated = generatedMessage
          , namespace = namespaceFor file
          , genTypes = HashMap.fromList genTypes
          , enums = HashMap.fromList enums
          }
    generatedMessage =
      "\0064generated" <>
        " - To regenerate this file run fbcode//glean/schema/gen/sync-www"

genTypeOfDef
  :: Context
  -> Either ResolvedPredicateDef ResolvedTypeDef
  -> Writer [HackEnum] (Text, GenType)
genTypeOfDef ctx def@(Left PredicateDef{..}) = (,) (refClassname ref) <$>
  case (predicateDefKeyType, predicateDefValueType) of
    (RecordTy fields, RecordTy[]) -> ClassType . Struct . ClassGenTypeData
      ref cycles <$> mapM (fieldInfoFor (refClassname ref) ctx) fields
    (SumTy fields, RecordTy[]) -> ClassType . Union . ClassGenTypeData
      ref cycles <$> mapM (fieldInfoFor (refClassname ref) ctx) fields
    (ty, RecordTy[]) -> ClassType . Wrapper . ClassGenTypeData
      ref cycles <$> mapM (fieldInfoFor (refClassname ref) ctx)
        [FieldDef "key" ty]
    (tyKey, tyValue) -> ClassType . KeyValue . ClassGenTypeData
      ref cycles <$> mapM (fieldInfoFor (refClassname ref) ctx)
        [FieldDef "key" tyKey, FieldDef "value" tyValue]
  where
    ref = defRef ctx def
    cycles = HashMap.lookupDefault True ref $ contextCycles ctx
genTypeOfDef ctx def@(Right TypeDef{..}) = (,) (refClassname ref) <$>
  case typeDefType of
    RecordTy fields ->
      ClassType . Struct . ClassGenTypeData ref cycles <$>
        mapM (fieldInfoFor (refClassname ref) ctx) fields
    SumTy fields ->
      ClassType . Union . ClassGenTypeData ref cycles <$>
        mapM (fieldInfoFor (refClassname ref) ctx) fields
    EnumeratedTy alts -> return $ EnumType alts
    ty -> AliasType . fieldInfoType <$>
      fieldInfoFor (refClassname ref) ctx (FieldDef "value" ty)
  where
    ref = defRef ctx def
    cycles = HashMap.lookupDefault True ref $ contextCycles ctx

fieldInfoFor
  :: Text
  -> Context
  -> ResolvedFieldDef
  -> Writer [HackEnum] FieldInfo
fieldInfoFor className Context{..} FieldDef{..} = do
  angleTypeRepr <- runReaderT (angleTypeReprFor fieldDefType) angleTypeReprCtx
  return FieldInfo
    { fieldInfoName = fieldDefName
    , fieldInfoType = angleTypeRepr
    }
  where
    angleTypeReprCtx = AngleTypeReprContext
      { ctxClassName=className
      , ctxFieldName=fieldDefName
      , ctxPredMap=contextPredMap
      , ctxTypeMap=contextTypeMap
      , ctxPredLatest=contextPredLatest
      , ctxTypeLatest=contextTypeLatest
      }

data AngleTypeReprContext = AngleTypeReprContext
  { ctxClassName :: Text
  , ctxFieldName :: Text
  , ctxPredMap :: HashMap PredicateRef ResolvedPredicateDef
  , ctxTypeMap :: HashMap TypeRef ResolvedTypeDef
  , ctxPredLatest :: HashMap Text Version
  , ctxTypeLatest :: HashMap Text Version
  }

-- There is not a simple relationship between the Angle type and
-- what type is used to represent it in Hack. Both Bytes and Nats are
-- represented as Ints. Predicates can be represented by both a Class
-- and a primitive (for simple predicates). Most named types are stored
-- as a class apart from Enumerations which get a Hack enum. This type
-- contains information on both the Hack type and the Angle type
data AngleTypeRepr
  = ByteTInt
  | NatTInt
  | StringTString
  | EnumeratedTEnum {alts::[Name], classname::Text}
  | BooleanTBool
  | ArrayTVec {inner::AngleTypeRepr}
  | SetTSet {inner::AngleTypeRepr}
  | RecordTShape {fields::[(Text, AngleTypeRepr)]}
  | SumTShape {fields::[(Text, AngleTypeRepr)]}
  | PredicateTKeyValue {pref::PredicateRef, classname::Text}
  | PredicateTNamed {pref::PredicateRef, classname::Text}
  | PredicateT {pref::PredicateRef, classname::Text, inner::AngleTypeRepr}
  | NamedTypeTNamed {tref::TypeRef, classname::Text}
  | NamedTypeTAlias {tref::TypeRef, classname::Text, inner::AngleTypeRepr}
  | NamedTypeTEnum {alts::[Name], tref::TypeRef, classname::Text}
  | MaybeTOption {inner::AngleTypeRepr}
  deriving (Generic, Show)

instance Aeson.ToJSON AngleTypeRepr where

-- Generating a class for Angle types that hold an Enumeration requires
-- creating a definition for that enum which is what the state holds
angleTypeReprFor
  :: ResolvedType
  -> ReaderT AngleTypeReprContext (Writer [HackEnum]) AngleTypeRepr
angleTypeReprFor (PredicateTy ref) = do
  ctx <- ask
  case lookupPredDefKeyValue ctx ref of
    -- If the predicate has value of type {} and the key is a String
    -- avoid wrapping it in a class. This makes writing queries more
    -- concise
    Just (StringTy, RecordTy []) ->
      PredicateT ref (classname ctx) <$> angleTypeInnerReprFor StringTy
    _ -> angleTypeInnerReprFor (PredicateTy ref)
    where
      lookupPredDefKeyValue ctx pref = do
        PredicateDef{..} <- HashMap.lookup pref $ ctxPredMap ctx
        return (predicateDefKeyType, predicateDefValueType)
      classname AngleTypeReprContext{..} =
        refClassname $ prefVref ctxPredLatest ref
angleTypeReprFor ty = angleTypeInnerReprFor ty

angleTypeInnerReprFor
  :: ResolvedType
  -> ReaderT AngleTypeReprContext (Writer [HackEnum]) AngleTypeRepr
angleTypeInnerReprFor ByteTy = return ByteTInt
angleTypeInnerReprFor NatTy = return NatTInt
angleTypeInnerReprFor StringTy = return StringTString
angleTypeInnerReprFor (ArrayTy ty) = ArrayTVec <$> angleTypeInnerReprFor ty
angleTypeInnerReprFor (RecordTy fields) =
  RecordTShape <$> mapM f fields
  where
    f FieldDef{..} =
      (,) fieldDefName <$> angleTypeInnerReprFor fieldDefType
angleTypeInnerReprFor (SumTy fields) =
  SumTShape <$> mapM f fields
  where
    f FieldDef{..} =
      (,) fieldDefName <$> angleTypeInnerReprFor fieldDefType
angleTypeInnerReprFor (SetTy ty) = SetTSet <$> angleTypeInnerReprFor ty
angleTypeInnerReprFor (PredicateTy ref) = do
  ctx <- ask
  case lookupPredDefKeyValue ctx ref of
    Nothing -> return $
      PredicateTNamed ref $ classname ctx
    Just (_k, RecordTy []) -> return $
      PredicateTNamed ref $ classname ctx
    Just (_k, _v) -> return $
      PredicateTKeyValue ref $ classname ctx
    where
      lookupPredDefKeyValue ctx pref = do
        PredicateDef{..} <- HashMap.lookup pref $ ctxPredMap ctx
        return (predicateDefKeyType, predicateDefValueType)
      classname AngleTypeReprContext{..} =
        refClassname $ prefVref ctxPredLatest ref
angleTypeInnerReprFor (NamedTy ref) = do
  ctx <- ask
  case lookupTypeDefType ctx ref of
    Just (EnumeratedTy alts) ->
      return $ NamedTypeTEnum alts ref $ classname ctx
    Nothing -> return $ NamedTypeTNamed ref $ classname ctx
    Just RecordTy{} -> return $ NamedTypeTNamed ref $ classname ctx
    Just SumTy{} -> return $ NamedTypeTNamed ref $ classname ctx
    Just t ->
      NamedTypeTAlias ref (classname ctx) <$>
        angleTypeInnerReprFor t
  where
    lookupTypeDefType ctx tref = typeDefType <$>
      HashMap.lookup tref (ctxTypeMap ctx)
    classname AngleTypeReprContext{..} =
      refClassname $ trefVref ctxTypeLatest ref
angleTypeInnerReprFor (MaybeTy t) = MaybeTOption <$> angleTypeInnerReprFor t
angleTypeInnerReprFor (EnumeratedTy alts) = do
  AngleTypeReprContext{..} <- ask
  let
    enumName =
      ctxClassName <> "EnumFor_" <> ctxFieldName
    enum = (enumName, map f alts)
    f name = (name, name)
  tell [enum]
  return $ EnumeratedTEnum alts enumName
angleTypeInnerReprFor BooleanTy = return BooleanTBool
angleTypeInnerReprFor TyVar{} = error "angleTypeInnerReprFor: TyVar"
angleTypeInnerReprFor HasTy{} = error "angleTypeInnerReprFor: HasTy"

defFile :: Either ResolvedPredicateDef ResolvedTypeDef -> FilePath
defFile (Left p) = fileFor $ predicateDefName p
defFile (Right t) = fileFor $ typeDefName t

refClassname :: VersionedReference -> Text
refClassname Reference{..} | refLatest = "GS" <> hackCase refName
refClassname Reference{..} =
  "GS" <> hackCase refName <> "_DEPRECATED" <> textShow refVersion

defRef
  :: Context
  -> Either ResolvedPredicateDef ResolvedTypeDef
  -> VersionedReference
defRef Context{..} (Left PredicateDef{..}) =
  prefVref contextPredLatest predicateDefRef
defRef Context{..} (Right TypeDef{..}) =
  trefVref contextTypeLatest typeDefRef

prefVref :: HashMap Text Version -> PredicateRef -> VersionedReference
prefVref ctxPredLatest PredicateRef{..} = Reference
  { refTag = PredicateReference
  , refName = predicateRef_name
  , refVersion = predicateRef_version
  , refLatest =
      HashMap.lookup predicateRef_name ctxPredLatest ==
        Just predicateRef_version
  }

trefVref :: HashMap Text Version -> TypeRef -> VersionedReference
trefVref ctxTypeLatest TypeRef{..} = Reference
  { refTag = TypeReference
  , refName = typeRef_name
  , refVersion = typeRef_version
  , refLatest =
      HashMap.lookup typeRef_name ctxTypeLatest == Just typeRef_version
  }

predicateDefName :: ResolvedPredicateDef -> Text
predicateDefName = predicateRef_name . predicateDefRef

typeDefName :: ResolvedTypeDef -> Text
typeDefName = typeRef_name . typeDefRef

hackCase :: Text -> Text
hackCase s =
  Text.pack $ concatMap (pascalCase . Text.unpack) $ Text.splitOn "." s
  where
    pascalCase (x:xs) = Char.toUpper x : xs
    pascalCase [] = []

fileFor :: Text -> FilePath
fileFor s = Text.unpack file <> ".json"
  where file = Text.intercalate "-" $ init $ Text.splitOn "." s

namespaceFor :: FilePath -> Text
namespaceFor file = (<>) "GS" $
  hackCase $
  Text.intercalate "." $
  Text.splitOn "-" $
  Text.pack $
  takeWhile ('.' /=) file

-- What counts as a cycle here is Hack specific. We report anything that
-- would cause a recursive shape to be generated (these are not allowed in Hack)
cyclesInDefs
  :: Context
  -> [Either ResolvedPredicateDef ResolvedTypeDef]
  -> [(VersionedReference, Bool)]
cyclesInDefs ctx defs = concatMap hasCycles sccs
 where
  hasCycles (AcyclicSCC one) = [(defRef ctx one, False)]
  hasCycles (CyclicSCC defs) = map (\def -> (defRef ctx def, True)) defs

  sccs = stronglyConnComp
    [ (def, defNode def, outEdges def) | def <- defs ]

  defNode :: Either ResolvedPredicateDef ResolvedTypeDef -> (Name,Version)
  defNode (Left PredicateDef{..}) =
    (predicateRef_name predicateDefRef, predicateRef_version predicateDefRef)
  defNode (Right TypeDef{..}) =
    (typeRef_name typeDefRef, typeRef_version typeDefRef)

  outEdges :: Either ResolvedPredicateDef ResolvedTypeDef -> [(Name,Version)]
  outEdges d = case d of
    Left PredicateDef{..} ->
      outEdgesTs [predicateDefKeyType, predicateDefValueType]
    Right TypeDef{..} ->
      outEdgesT typeDefType

  outEdgesTs = concatMap outEdgesT
  outEdgesFields fields = outEdgesTs [ ty | FieldDef _ ty <- fields ]

  outEdgesT :: ResolvedType -> [(Name,Version)]
  outEdgesT ByteTy{} = []
  outEdgesT NatTy{} = []
  outEdgesT BooleanTy{} = []
  outEdgesT StringTy{} = []
  outEdgesT (ArrayTy ty) = outEdgesT ty
  outEdgesT (MaybeTy ty) = outEdgesT ty
  outEdgesT (RecordTy fields)  = outEdgesFields fields
  outEdgesT (SumTy fields)  = outEdgesFields fields
  outEdgesT (SetTy ty) = outEdgesT ty
  outEdgesT (NamedTy (TypeRef name ver)) = [(name,ver)]
  outEdgesT (PredicateTy (PredicateRef name ver)) = [(name,ver)]
  outEdgesT EnumeratedTy{} = []
  outEdgesT TyVar{} = error "outEdgesT: TyVar"
  outEdgesT HasTy{} = error "outEdgesT: HasTy"

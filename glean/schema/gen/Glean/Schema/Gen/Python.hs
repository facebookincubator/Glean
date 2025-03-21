{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Generate "Glean.Schema"
module Glean.Schema.Gen.Python
  ( genSchemaPy
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath
import TextShow
import Data.Set (toList, fromList)
import qualified Data.Maybe

import Glean.Schema.Gen.Utils
import Glean.Angle.Types
import Glean.Schema.Types
import Glean.Types (TypeName)


genSchemaPy
  :: Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> [(FilePath,Text)]
genSchemaPy _version preddefs typedefs =
  ( "py" </> "TARGETS", genTargets declsPerNamespace extraImports):
  ( "py" </> "glean_schema_predicate.py", -- base class shared between schemas
  Text.unlines
  [ "# \x40generated"
  , "# To regenerate this file run fbcode//glean/schema/gen/sync"
  , "from typing import Generic, Optional, TypeVar"
  , "from thrift.py3.types import Struct"
  , ""
  , "# Pyre expects a Fact to be bound to a Struct in GleanClient. For our " <>
    "API, we expect a GleanSchemaPredicate as the result. To meet both " <>
    "conditions, GleanSchemaPredicate extends a Struct."
  , "class GleanSchemaPredicate(Struct):"
  , "  " <> "@staticmethod"
  , "  " <> "def angle_query(*, arg: str) -> \"GleanSchemaPredicate\":"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  , ""
  , "class InnerGleanSchemaPredicate:"
  , "  " <> "@staticmethod"
  , "  " <> "def angle_query(*, arg: str) -> \"InnerGleanSchemaPredicate\":"
  , "    " <> "raise Exception" <> "(\"this function can only be called as" <>
    " a parameter of a GleanSchemaPredicate\")"
  , ""
  , "T = TypeVar(\"T\")"
  , ""
  , "class Just(Generic[T]):"
  , "  " <> "just: T | None = None"
  , "  " <> "def __init__(self, just: T | None = None) -> None:"
  , "    " <> "self.just = just"
  , "  " <> "def get(self) -> Optional[T]:"
  , "    " <> "return self.just"
  ]) :
  [ ("py" </>
      Text.unpack (Text.concat namespaces) <.> "py",
      Text.intercalate (newline <> newline)
        ( header namespaces namePolicy allPreds :
          genPredicateImports namespaces allPreds :
          genAllPredicates AngleQuery namespaces namePolicy
            "GleanSchemaPredicate" preds :
          -- InnerGleanSchemaPredicate hides Named Types from the user's outer
          -- query. InnerGleanSchemaPredicates cannot be called directly by an
          -- Angle query, they must be used only as inner query fields.
          genAllPredicates AngleQuery namespaces namePolicy
            "InnerGleanSchemaPredicate" (predsFromTypes preds types) :
          genNamedTypesAliases namePolicy types
        )
    )
  | (namespaces, (_, preds, types)) <- schemas,
    let allPreds = preds ++ predsFromTypes preds types
  ]
  where
    schemas = HashMap.toList declsPerNamespace
    predsFromTypes preds types = genNamedTypesClasses (head preds) types
    namePolicy = mkNamePolicy preddefs typedefs
    declsPerNamespace =
      addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs
    genPredicateImports namespaces preds = Text.unlines $
      ("from glean.schema." <> namespaceToFileName namespaces <> ".types import (") :
      [ "    " <> return_class_name <> ","
          | pred <- preds
          , let ref = predicateDefRef pred
                predicateName = predicateRef_name ref
                return_class_name = returnPythonClassName predicateName
      ] ++
      [")"]
    extraImports = [
      (Text.concat namespaces,
      addBuckImportsForKeys namespaces namePolicy allPreds) |
        (namespaces, (_, preds, types)) <- schemas,
        let allPreds = preds ++ predsFromTypes preds types]

-- To handle SumTypes inner fields, the API creates dummy predicates as a
-- form of syntactic sugar for the user. The dummy queries created through them
-- do not correspond to any Angle query.
data PredicateMode = AngleQuery | DummyQuery

genAllPredicates
  :: PredicateMode
  -> NameSpaces
  -> NamePolicy
  -> Text
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates predicateMode _ namePolicy parent preds = Text.unlines $
  [ Text.unlines $ case key of
    EnumeratedTy vals -> "class " <> class_name <> "(Enum):" :
      zipWith mkEnumerator [0..] vals
    -- handle GleanSchemaPredicates with no key (ex: builtin.Unit.1)
    RecordTy [] -> [class_name <> " = Tuple[()]"]
    _ ->
      [ "class " <> class_name <> "(" <> parent <> "):"
      , "  " <> "@staticmethod"
      , "  " <> "def build_angle(__env: Dict[str, R]" <> buildAngleTypes <>
        ") -> Tuple[str, Struct]:"
      , "    " <> "query_fields = " <> angleForTypes
      , "    " <> "return f\"" <> anglePredicateAndVersion <>
        " { " <> queryFields <> " if query_fields else '_' }" <>
        "\", " <> return_class_name
      , ""
      , addClientMethods class_name kTy namePolicy key
      , addSumTypes
      ]
    | pred <- preds
    , let ref = predicateDefRef pred
          key = predicateDefKeyType pred
          kTy = valueTy class_name APIValues namePolicy key
          queryFields = Text.pack $ case key of
            SumTy{} ->  "('{ ' + query_fields + ' }')"
            RecordTy{} -> "('{ ' + query_fields + ' }')"
            _ -> "query_fields"
          angleForTypes = case buildAngleTypes of
            "" -> " " <> "_"
            _ -> valueTy class_name AngleForValues namePolicy key
          predicateName = predicateRef_name ref
          class_name = pythonClassName predicateName
          return_class_name = returnPythonClassName predicateName
          addSumTypes = genAllPredicates DummyQuery [Text.empty] namePolicy
            parent $ unionDummyPreds key pred class_name
          anglePredicateAndVersion = case predicateMode of
            AngleQuery -> predicateName <> "." <>
              showt (predicateRef_version ref)
            _ -> Text.empty
          buildAngleTypes = case buildAngleTypes_ of
            "" -> Text.empty
            _ -> ", " <> buildAngleTypes_
            where
              buildAngleTypes_ = valueTy class_name ASTValues namePolicy key
          mkEnumerator (i :: Int) val = "  " <> val <> " = " <> showt i
  ]

genNamedTypesClasses
  :: ResolvedPredicateDef
  -> [ResolvedTypeDef]
  -> [ResolvedPredicateDef]
genNamedTypesClasses pred types = map genTypePred $
  filter (\(_, t, _) -> shouldGenClass t) $ map genType types
  where
    genTypePred (n, t, v) = PredicateDef (PredicateRef n v) t t
      (predicateDefDeriving pred) (predicateDefSrcSpan pred)


genNamedTypesAliases :: NamePolicy -> [TypeDef_ PredicateRef TypeRef] -> [Text]
genNamedTypesAliases namePolicy types = map genTypeAlias $
  filter (\(_, t, _) -> shouldGenAlias t) $ map genType types
  where
    genTypeAlias (n, t, _) = pythonClassName n <> " = " <> baseTy Text.empty namePolicy t


shouldGenClass :: Type_ pref tref -> Bool
shouldGenClass t = case t of
  RecordTy{} -> True
  SumTy{} -> True
  EnumeratedTy{} -> True
  _ -> False

shouldGenAlias :: Type_ pref tref -> Bool
shouldGenAlias t = not $ shouldGenClass t

genType :: TypeDef_ pref TypeRef -> (TypeName, Type_ pref TypeRef, Version)
genType TypeDef{typeDefRef = TypeRef{..}, ..} = (typeRef_name, typeDefType, typeRef_version)

unionDummyPreds :: Type_ PredicateRef tref
  -> PredicateDef_ s PredicateRef tref
  -> Text
  -> [PredicateDef_ s PredicateRef tref]
unionDummyPreds key pred class_name = map
  (\(n, t) -> PredicateDef (predRef n) t t
    (predicateDefDeriving pred) (predicateDefSrcSpan pred))
  dummyPredicateGens
  where
    predRef n = PredicateRef (class_name <> "_" <> n) 0
    dummyPredicateGens = case key of
      RecordTy fields ->
        concatMap (\f -> handleFields (fieldName f) (fieldDefType f)) fields
      SumTy fields ->
        concatMap (\f -> handleFields (fieldName f) (fieldDefType f)) fields
      _ -> []
      where
        handleFields n t = case t of
          SumTy fields ->
            (n, SumTy fields) : concatMap extraRecords fields
          RecordTy fields ->
            (n, RecordTy fields) : concatMap extraRecords fields
          _ -> []
          where
            extraRecords t = case fieldDefType t of
              RecordTy fields ->
                (n, RecordTy fields) : handleFields n (RecordTy fields)
              SumTy fields -> handleFields n (SumTy fields)
              _ -> []

addClientMethods :: Text -> Text -> NamePolicy -> ResolvedType -> Text
addClientMethods class_name kTy namePolicy key = methods
  where
    methods = case key of
      SumTy fields ->Text.unlines $ map createMethodWrapper (unionFields fields)
      _ -> createMethod Text.empty kTy
    unionFields fields = map handleField fields
    handleField f = (fieldName f,
      (baseTy (class_name <> "_" <> fieldName f) namePolicy . fieldDefType) f)
    createMethodWrapper (name, type_) =
      createMethod ("_" <> name) (name <> ": " <> wrapOptionalArg type_)
    createMethod method_name args = Text.unlines
      [ "  " <> "@staticmethod"
      , "  " <> "def angle_query" <> method_name <> "(*, " <> args <>
        ") -> \"" <> class_name <> "\":"
      , "    " <> "raise Exception" <>
        "(\"this function can only be called from @angle_query\")"
      ]

header :: NameSpaces -> NamePolicy -> [ResolvedPredicateDef] -> Text
header namespaces namePolicy preds = Text.unlines $
  [ "# \x40generated"
  , "# To regenerate this file run fbcode//glean/schema/gen/sync"
  , "from typing import Optional, Tuple, Union, List, Dict, TypeVar"
  , "from thrift.py3 import Struct"
  , "from enum import Enum"
  , "import ast"
  , "from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate"<>
    " , Just, InnerGleanSchemaPredicate"
  , "from glean.client.py3.angle_query import angle_for, R"
  ] ++ addPythonImportsForKeys namespaces namePolicy preds

pythonClassName :: Text -> Text
pythonClassName c = Text.intercalate "" $ map cap1 $ Text.split (== '.') c

returnPythonClassName :: Text -> Text
returnPythonClassName c = last $ Text.split (== '.') c

filenameToNamespace :: Text -> Text
filenameToNamespace n = Text.concat $ Text.split (=='_') n

namespaceToFileName :: [Text] -> Text
namespaceToFileName n = Text.intercalate "_" n

genTargets
  :: HashMap NameSpaces ([NameSpaces], [ResolvedPredicateDef], [ResolvedTypeDef])
  -> [(Text, [Text])]
  -> Text
genTargets info extraImports =
  Text.unlines $
     [ "# \x40generated"
     , "# to regenerate: ./glean/schema/sync"
     , "load(\"@fbcode_macros//build_defs:python_library.bzl\", " <>
       "\"python_library\")"
     , ""
     ] ++
     [ "python_library("
     , "  name = \"glean_schema_predicate\","
     , "  srcs = [\"glean_schema_predicate.py\"],"
     , "  deps = [\"//thrift/lib/py3:types\"],"
     , ")"
     , ""
     ] ++
     concatMap genTarget (HashMap.keys info)
  where
  genTarget ns =
    let
      namespace = underscored ns
      extra = Data.Maybe.fromMaybe [""] (lookup (Text.concat ns) extraImports)
    in
    -- mini Python library for the module containing allPredicates
    [ "python_library("
    , "  name = \"" <> namespace <> "\","
    , "  srcs = [\"" <> Text.concat ns <> ".py\"],"
    , "  deps = ["
    , "    " <> "\"//glean/schema/py:glean_schema_predicate\","
    , "    " <> "\"//glean/schema/thrift:" <> namespace <> "-py3-types\","
    , "    " <> "\"//thrift/lib/py3:types\","
    , "    " <> "\"//glean/client/py3:angle_query\","
    ] ++ extra ++
    [ "  ]"
    , ")"
    , ""
    ]

data ValueTy = APIValues | ASTValues | AngleForValues
  deriving (Eq, Show)

-- | Generate a value type
valueTy :: Text -> ValueTy -> NamePolicy -> ResolvedType -> Text
valueTy predName mode namePolicy t = case t of
  RecordTy fields -> handleFields fields
  SumTy fields -> handleFields fields
  _ -> if mode  == APIValues then defaultFieldName <> ": " <>
        wrapOptionalArg (baseTy predName namePolicy t)
       else if mode == ASTValues then defaultFieldName <> ": ast.Expr"
       else " angle_for(__env, " <> defaultFieldName <> ", None" <> ")"

  where
    handleFields fields = case mode of
      AngleForValues -> intercalatedFieldsWithBrackets
      _ -> intercalatedFields
      where
        queryFields = map (createQueryField mode) fields
        intercalatedFields = intercalateQueryFields queryFields
        intercalatedFieldsWithBrackets = " \', \'.join(filter" <>
         "(lambda x: x != '', [" <> intercalatedFields <> "]))"
    createQueryField mode field = case mode of
      APIValues -> appendType $ (wrapOptionalArg .
        baseTy (predName <> "_" <> pythonVarName) namePolicy . fieldDefType)
        field
      ASTValues -> appendType "ast.Expr"
      AngleForValues -> "angle_for(__env, " <> pythonVarName <> ", \'" <>
       name <> "\')"
      where
          name = fieldDefName field
          pythonVarName = from_ name
          appendType t = pythonVarName <> ": " <> t
    defaultFieldName = Text.pack "arg"

from_ :: Text -> Text
from_ name -- avoid Python keywords
  | Text.unpack name `elem` pythonKeywords = "_" <> name
  | otherwise = name
  where
    pythonKeywords = ["from", "str"]


baseTy :: Text -> NamePolicy -> ResolvedType -> Text
baseTy typeName namePolicy t = Text.pack $ case t of
  NatTy{} -> "int"
  BooleanTy{} -> "bool"
  StringTy{} -> "str"
  ByteTy{} -> "bytes"
  PredicateTy pred -> nameLookup pred predNames namePolicy "predicate"
  NamedTy typeRef -> nameLookup typeRef typeNames namePolicy "named"
  ArrayTy field -> Text.unpack $ if type_ == "bytes"
    then type_
    else "List[" <> type_ <> "]"
    where
      type_ = baseTy typeName namePolicy field
  MaybeTy field -> Text.unpack $
    "Union[Just[" <> baseTy typeName namePolicy field <> "], Just[None]]"
  _ -> Text.unpack $ "\'" <> typeName <> "\'"
  where
    nameLookup type_ names namePolicy errMessage =
      case HashMap.lookup type_ (names namePolicy) of
        Just (ns, x) -> Text.unpack $
          "\"" <> Text.concat (map cap1 ns ++ [x]) <> "\""
        _ -> error $ errMessage ++ "PythonName: " ++ show type_

addPythonImportsForKeys
  :: NameSpaces
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> [Text]
addPythonImportsForKeys namespaces namePolicy preds = map addImport $
  predicateImports namespaces namePolicy preds

  where
    addImport :: Text -> Text
    addImport imp =
      "from glean.schema.py." <> filenameToNamespace imp <> " import *"

addBuckImportsForKeys
  :: NameSpaces
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> [Text]
addBuckImportsForKeys namespaces namePolicy preds = map addImport $
  predicateImports namespaces namePolicy preds

  where
    addImport :: Text -> Text
    addImport imp = "    " <> "\"//glean/schema/py:" <> imp <> "\","

predicateImports :: NameSpaces -> NamePolicy -> [ResolvedPredicateDef] -> [Text]
predicateImports namespaces namePolicy preds = filter
  (\imp -> imp /= "" && imp /= current_namespace) $
  mkUniq $ concatMap extractImports preds

  where
    current_namespace :: Text
    current_namespace = namespaceToFileName namespaces

    extractImports :: ResolvedPredicateDef -> [Text]
    extractImports = extractPredicateImport namePolicy . predicateDefKeyType

    extractPredicateImport :: NamePolicy -> ResolvedType  -> [Text]
    extractPredicateImport namePolicy t = case t of
      RecordTy fields -> map (extractImport namePolicy . fieldDefType) fields
      SumTy fields -> map (extractImport namePolicy . fieldDefType) fields
      _ -> [extractImport namePolicy t]
      where
        extractImport namePolicy t = Text.pack $ case t of
          PredicateTy pred -> nameLookup pred predNames namePolicy "predicate"
          NamedTy typeRef -> nameLookup typeRef typeNames namePolicy "named"
          _ -> ""
        nameLookup type_ names namePolicy errMessage =
          case HashMap.lookup type_ (names namePolicy) of
            Just (ns, _) -> Text.unpack $ namespaceToFileName ns
            _ -> error $ errMessage ++ "PythonName: " ++ show type_

fieldName :: FieldDef_ PredicateRef tref -> Text
fieldName = from_ . fieldDefName

intercalateQueryFields :: [Text] -> Text
intercalateQueryFields fields = Text.intercalate ", " fields

wrapOptionalArg :: Text -> Text
wrapOptionalArg arg = "Optional[" <> arg <> "] = None"

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

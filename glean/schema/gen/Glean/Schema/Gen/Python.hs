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
  , "from typing import Dict, Tuple, TypeVar, Type, Optional"
  , "from thrift.py3 import Struct"
  , "import json"
  , "import inspect"
  , "import ast"
  , ""
  , "R = TypeVar(\"R\", int, str, bool, Tuple[()])"
  , ""
  , "class GleanSchemaPredicate:"
  , "  " <> "@staticmethod"
  , "  " <> "def angle_query(*, arg: str) -> \"GleanSchemaPredicate\":"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  , ""
  , "def angle_for(__env: Dict[str, R], key: ast.Expr, field_name: Optional[str]) -> str:"
  , "  " <> "if key is None:"
  , "    " <> "return f''"
  , "  " <> "if isinstance(key, ast.Name):"
  , "    " <> "return _make_attribute(field_name, json.dumps(__env[key.id]))"
  , "  " <> "elif isinstance(key, ast.Constant):"
  , "    " <> "return _make_attribute(field_name, json.dumps(key.value))"
  , "  " <> "elif isinstance(key, ast.Call):"
  , "    " <> "nested_call_arg = callGleanSchemaPredicateQuery" <>
    "(key, {}, \"__target__\", __env)[\"__target__\"]"
  , "    " <> "return _make_attribute(field_name, nested_call_arg[0])"
  , "  " <> "elif isinstance(key, ast.List):"
  , "    " <> "elems = map(lambda el: angle_for(__env, el, None), key.elts)"
  , "    " <> "value = f\'[{ \", \".join(elems) }]\'"
  , "    " <> "return _make_attribute(field_name, value)"
  , "  " <> "raise NotImplementedError(f\"Query key type not implemented\")"
  , ""
  , "def _make_attribute(field_name: Optional[str], value: str) -> str:"
  , "  " <> "if field_name:"
  , "    " <> "return f'{field_name} = {value}'"
  , "  " <> "return value"
  , ""
  , "def _class_name_to_py_query(class_name: str, __env: Dict[str, R" <>
    "], method_args: Dict[str, ast.Expr]) -> Tuple[str, Type[Struct]]:"
  , "  " <> "# Before this function call, we check that the node is an" <>
    " ast.Name (part of an ast.Call)."
  , "  " <> "# It will always evaluate to a class name."
  , ""
  , "  " <> "# Using globals() to get the class_name does not work because" <>
    " it needs all the user classes imported"
  , "  " <> "# in this module. Instead: we use the frame info to access the " <>
    "global namespace of the user's code."
  , "  " <> "# The variable no_frames_to_user_code is dependent on the " <>
    "number of nested functions calls from the user code."
  , "  " <> "num_frames_to_user_code = 3"
  , "  " <> "frame = 0"
  , "  " <> "ctr = 0"
  , "  " <> "while True:"
  , "    " <> "try:"
  , "      " <> "globals_ = " <>
    "inspect.stack()[num_frames_to_user_code + ctr][frame].f_globals"
  , "      " <> "angle_query_args = " <>
    "inspect.getfullargspec(globals_[class_name].build_angle).args[1:]"
  , "      " <> "break"
  , "    " <> "except KeyError as e:"
  , "      " <> "# inner calls to GleanSchemaPredicates " <>
    "need extra num_frames_to_user_code"
  , "      " <> "ctr = ctr + num_frames_to_user_code"
  , "  " <> "fields = (method_args.get(k) for k in angle_query_args)"
  , "  " <> "return (globals_[class_name].build_angle(__env, *tuple(fields)))"
  , ""
  , "def callGleanSchemaPredicateQuery(function_call: ast.Call, variables: " <>
    "Dict[str, Tuple[str, Type[Struct]]], target: str, __env: Dict" <>
    "[str, R]) -> Dict[str, Tuple[str, Type[Struct]]]:"
  , "  " <> "method_args = function_call.keywords"
  , "  " <> "method_args_ = {}"
  , "  " <> "for method_arg in method_args:"
  , "    " <> "query_arg_name = method_arg.arg"
  , "    " <> "arg_val = method_arg.value"
  , "    " <> "method_args_[query_arg_name] = arg_val"
  , "  " <> "function_attribute = function_call.func"
  , "  " <> "if isinstance(function_attribute, ast.Attribute):"
  , "    " <> "class_name = function_attribute.value"
  , "    " <> "if isinstance(class_name, ast.Name):"
  , "      " <> "class_name = class_name.id"
  , "      " <> "variables[target] = _class_name_to_py_query(class_name, " <>
    "__env, method_args_)"
  , "  " <> "return variables"
  ]) :
  [ ("py" </>
      Text.unpack (Text.concat namespaces) <.> "py",
      Text.intercalate (newline <> newline)
        ( header namespaces namePolicy preds :
          genPredicateImports namespaces preds :
          [genAllPredicates AngleQuery namespaces namePolicy preds]
        )
    )
  | (namespaces, (_, preds, _)) <- schemas
  ]
  where
    schemas = HashMap.toList declsPerNamespace
    namePolicy = mkNamePolicy preddefs typedefs
    declsPerNamespace =
      addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs
    genPredicateImports namespaces preds = Text.unlines $
      ("from glean.schema." <> Text.concat namespaces <> ".types import (" ) :
      [ "    " <> return_class_name <> ","
          | pred <- preds
          , let ref = predicateDefRef pred
                predicateName = predicateRef_name ref
                return_class_name = returnPythonClassName predicateName
      ] ++
      [")"]
    extraImports = [
      (Text.concat namespaces,
      addBuckImportsForKeys namespaces namePolicy preds) |
        (namespaces, (_, preds, _)) <- schemas ]

-- To handle SumTypes inner fields, the API creates dummy predicates as a
-- form of syntactic sugar for the user. The dummy queries created through them
-- do not correspond to any Angle query.
data PredicateMode = AngleQuery | DummyQuery

genAllPredicates
  :: PredicateMode
  -> NameSpaces
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates predicateMode _ namePolicy preds = Text.unlines $
  [ Text.unlines
    [ "class " <> class_name <> "(GleanSchemaPredicate):"
      , "  " <> "@staticmethod"
      , "  " <> "def build_angle(__env: Dict[str, R], " <> buildAngleTypes <>
        ") -> Tuple[str, Struct]:"
      , "    " <> "return f\"" <> anglePredicateAndVersion <> angleForTypes <>
        "\", " <> return_class_name
      , ""
      , addClientMethods class_name kTy namePolicy key
      , addSumTypes
    ]
    | pred <- preds
    , let ref = predicateDefRef pred
          key = predicateDefKeyType pred
          kTy = valueTy class_name APIValues namePolicy key
          angleForTypes = valueTy class_name AngleForValues namePolicy key
          buildAngleTypes = valueTy class_name ASTValues namePolicy key
          predicateName = predicateRef_name ref
          class_name = pythonClassName predicateName
          return_class_name = returnPythonClassName predicateName
          addSumTypes = genAllPredicates DummyQuery [Text.pack ""] namePolicy $
            unionDummyPreds key pred class_name
          anglePredicateAndVersion = case predicateMode of
            AngleQuery -> predicateName <> "." <>
              showt (predicateRef_version ref)
            _ -> Text.pack ""
  ]

unionDummyPreds :: Type_ PredicateRef tref
  -> PredicateDef_ s PredicateRef tref
  -> Text
  -> [PredicateDef_ s PredicateRef tref]
unionDummyPreds key pred class_name = map
  (\(n, t) -> PredicateDef (predRef n) t t (predicateDefDeriving pred))
  dummyPredicateGens
  where
    predRef n = PredicateRef (class_name <> "_" <> n) 0
    dummyPredicateGens = case key of
      RecordTy fields ->
        concatMap (\f -> handleFields (name f) (fieldDefType f)) fields
      SumTy fields ->
        concatMap (\f -> handleFields (name f) (fieldDefType f)) fields
      _ -> []
      where
        handleFields n t = case t of
          SumTy fields -> [(n, SumTy fields)]
          _ -> []
        name = from_ . fieldDefName


addClientMethods :: Text -> Text -> NamePolicy -> ResolvedType -> Text
addClientMethods class_name kTy namePolicy key = case key of
  SumTy fields -> Text.unlines $ map createMethodWrapper unionFields
    where
      unionFields = map
        (\ f
        -> ((from_ . fieldDefName) f,
            (baseTy class_name namePolicy . fieldDefType) f))
       fields
      createMethodWrapper (name, type_) =
        createMethod ("_" <> name) (name <> ": " <> type_)

  _ -> createMethod (Text.pack "") kTy
  where
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
  , "from typing import Optional, Tuple, Union, List, Dict"
  , "from thrift.py3 import Struct"
  , "import ast"
  , "from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate"<>
    ", angle_for, R"
  ] ++ addPythonImportsForKeys namespaces namePolicy preds

pythonClassName :: Text -> Text
pythonClassName c = Text.intercalate "" $ map cap1 $ Text.split (== '.') c

returnPythonClassName :: Text -> Text
returnPythonClassName c = Text.intercalate "" $ case Text.split (== '.') c of
      [x] -> [x]
      any -> tail any

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
    ] ++ extra ++
    [ "  ]"
    , ")"
    , ""
    ]

data ValueTy = APIValues | ASTValues | AngleForValues
  deriving (Eq, Show)

-- | Generate a value type
valueTy :: Text -> ValueTy  -> NamePolicy -> ResolvedType -> Text
valueTy predName mode namePolicy t = case t of
  RecordTy fields -> handleFields fields
  SumTy fields -> handleFields fields
  _ -> if mode  == APIValues then defaultFieldName <> ": " <>
        wrapOptionalArg (baseTy predName namePolicy t)
       else if mode == ASTValues then defaultFieldName <> ": ast.Expr"
       else " { angle_for(__env, " <> defaultFieldName <> ", None" <> ") " <>
        "or \'_\' }"

  where
    handleFields fields = if mode == AngleForValues
      then intercalatedFieldsWithBrackets
      else intercalatedFields
      where
        queryFields = map (createQueryField mode) fields
        intercalatedFields = Text.intercalate ", " queryFields
        intercalatedFieldsWithBrackets = " {{ { \', \'.join(filter" <>
          "(lambda x: x != '', [" <> intercalatedFields <> "])) or \'_\' } }}"
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
    wrapOptionalArg arg = "Optional[" <> arg <> "] = None"

from_ :: Text -> Text
from_ name -- avoid Python keywards
      | Text.unpack name `elem` pythonKeywords = "_" <> name
      | otherwise = name
      where
        pythonKeywords = ["from", "str"]


baseTy :: Text -> NamePolicy -> ResolvedType -> Text
baseTy unionName namePolicy t = Text.pack $ case t of
  NatTy{} -> "int"
  BooleanTy{} -> "bool"
  StringTy{} -> "str"
  PredicateTy pred -> case HashMap.lookup pred (predNames namePolicy) of
    Just (ns, x) -> concatMap Text.unpack ("\"" : map cap1 ns ++ [x] ++ ["\""])
    _ -> error $ "predicatePythonName: " ++ show pred
  SumTy{} -> Text.unpack $ "\'" <> unionName <> "\'"
  ArrayTy field -> Text.unpack $
    "List[" <> baseTy unionName namePolicy field <> "]"
  -- TODO other types
  _ -> "Tuple[()]"

addPythonImportsForKeys
  :: NameSpaces
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> [Text]
addPythonImportsForKeys namespaces namePolicy preds = map addImport $
  predicateImports namespaces namePolicy preds

  where
    addImport :: Text -> Text
    addImport imp = "from glean.schema.py." <> imp <> " import *"

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
    current_namespace = Text.concat namespaces

    extractImports :: ResolvedPredicateDef -> [Text]
    extractImports = extractPredicateImport namePolicy . predicateDefKeyType

    extractPredicateImport :: NamePolicy -> ResolvedType  -> [Text]
    extractPredicateImport namePolicy t = case t of
      RecordTy fields -> map (extractImport namePolicy . fieldDefType) fields
      _ -> [extractImport namePolicy t]
      where
        extractImport namePolicy t = case t of
          PredicateTy pred -> case HashMap.lookup pred (predNames namePolicy) of
            Just (ns, _) -> Text.concat ns
            _ -> error $ "predicatePythonName: " ++ show pred
          _ -> Text.pack ""

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

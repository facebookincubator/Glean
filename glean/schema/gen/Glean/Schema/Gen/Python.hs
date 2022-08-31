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
  , "from typing import Dict, Tuple, TypeVar, Type"
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
  , "def angle_for(__env: Dict[str, R], key: ast.Expr):"
  , "  " <> "if key is None:"
  , "    " <> "return f'_'"
  , "  " <> "if isinstance(key, ast.Name):"
  , "    " <> "return json.dumps(__env[key.id])"
  , "  " <> "elif isinstance(key, ast.Constant):"
  , "    " <> "return json.dumps(key.value)"
  , "  " <> "elif isinstance(key, ast.Call):"
  , "    " <> "nested_call_arg = callGleanSchemaPredicateQuery" <>
    "(key, {}, \"__target__\", __env)[\"__target__\"]"
  , "    " <> "return nested_call_arg[0]"
  , "  " <> "raise NotImplementedError(f\"Query key type not implemented\")"
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
  , "  " <> "angle_query_args = inspect.getfullargspec("
  , "    " <> "inspect.stack()[num_frames_to_user_code][frame]"
  , "    " <> ".f_globals[class_name]"
  , "    " <> ".build_angle"
  , "  " <> ").args[1:]"
  , "  " <> "fields = (method_args.get(k) for k in angle_query_args)"
  , "  " <> "return ("
  , "    " <> "inspect.stack()[num_frames_to_user_code][frame]"
  , "    " <> ".f_globals[class_name]"
  , "    " <> ".build_angle(__env, *tuple(fields))"
  , "  " <> ")"
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
  , "    " <> "class_method = function_attribute.attr"
  , "    " <> "if isinstance(class_name, ast.Name):"
  , "      " <> "class_name = class_name.id"
  , "      " <> "if class_method != \"angle_query\":"
  , "        " <> "raise AttributeError(f\"{class_method} is an undefined " <>
    "method\")"
  , "      " <> "variables[target] = _class_name_to_py_query(class_name, " <>
    "__env, method_args_)"
  , "  " <> "return variables"
  ]) :
  [ ("py" </>
      Text.unpack (Text.concat namespaces) <.> "py",
      Text.intercalate (newline <> newline)
        ( header namespaces namePolicy preds :
          genPredicateImports namespaces preds :
          [genAllPredicates namespaces namePolicy preds]
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

genAllPredicates
  :: NameSpaces
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates _ namePolicy preds = Text.unlines $
  [ Text.unlines
    [ "class " <> class_name <> "(GleanSchemaPredicate):"
      , "  " <> "@staticmethod"
      , "  " <> "def build_angle(__env: Dict[str, R], " <> buildAngleTypes <>
        ") -> Tuple[str, Struct]:"
      , "    " <> "return f\"" <> predicateName <> "." <>
        showt (predicateRef_version ref) <> " " <> angleForTypes <>
        "\", " <> return_class_name
      , ""
      , "  " <> "@staticmethod"
      , "  " <> "def angle_query(*, " <> kTy <> ") -> \"" <>
        class_name <> "\":"
      , "    " <> "raise Exception" <>
        "(\"this function can only be called from @angle_query\")"
    ]
    | pred <- preds
    , let ref = predicateDefRef pred
          key = predicateDefKeyType pred
          kTy = valueTy APIValues namePolicy key
          angleForTypes = valueTy AngleForValues namePolicy key
          buildAngleTypes = valueTy ASTValues namePolicy key
          predicateName = predicateRef_name ref
          class_name = pythonClassName predicateName
          return_class_name = returnPythonClassName predicateName
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
returnPythonClassName c = Text.intercalate "" $ tail $ Text.split (== '.') c

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
valueTy :: ValueTy  -> NamePolicy -> ResolvedType -> Text
valueTy mode namePolicy t = case t of
  RecordTy fields ->
    let
      queryFields = map (createQueryField mode) fields
      intercalatedFields = Text.intercalate ", " queryFields
      intercalatedFieldsWithBrackets = "{{ " <> intercalatedFields <> " }}"
    in
      if mode == AngleForValues then intercalatedFieldsWithBrackets
      else intercalatedFields
  _ -> if mode  == APIValues then defaultFieldName <> ": " <>
          baseTy namePolicy t
       else if mode == ASTValues then defaultFieldName <> ": ast.Expr"
       else "{angle_for(__env, " <> defaultFieldName <> ")}"

  where
    createQueryField mode field = case mode of
      APIValues -> appendType $ (baseTy namePolicy . fieldDefType) field
      ASTValues -> appendType "ast.Expr"
      AngleForValues -> name <> " = {angle_for(__env, " <> pythonVarName <> ")}"
      where
          name = fieldDefName field
          pythonVarName = from_ name
          appendType t = pythonVarName <> ": " <> t
    defaultFieldName = Text.pack "arg"
    from_ name -- avoid Python keywards
      | name == Text.pack "from" = "_" <> name
      | otherwise = name



baseTy :: NamePolicy -> ResolvedType -> Text
baseTy namePolicy t = wrapOptionalArg $ case t of
  NatTy{} -> "int"
  BooleanTy{} -> "bool"
  StringTy{} -> "str"
  PredicateTy pred -> case HashMap.lookup pred (predNames namePolicy) of
    Just (ns, x) -> concatMap Text.unpack ("\"" : map cap1 ns ++ [x] ++ ["\""])
    _ -> error $ "predicatePythonName: " ++ show pred
  -- TODO other types
  _ -> "Tuple[()]"
  where
    wrapOptionalArg :: String -> Text
    wrapOptionalArg arg = "Optional[" <> Text.pack arg <> "] = None"

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

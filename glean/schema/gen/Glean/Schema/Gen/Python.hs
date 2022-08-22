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

import Glean.Schema.Gen.Utils
import Glean.Angle.Types
import Glean.Schema.Types

genSchemaPy
  :: Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> [(FilePath,Text)]
genSchemaPy _version preddefs typedefs =
  ( "py" </> "TARGETS", genTargets declsPerNamespace) :
  ( "py" </> "glean_schema_predicate.py", -- base class shared between schemas
  Text.unlines
  [ "# \x40generated"
  , "# To regenerate this file run fbcode//glean/schema/gen/sync"
  , "from typing import Tuple, Type, Union, TypeVar"
  , "from thrift.py3 import Struct"
  , ""
  , "class GleanSchemaPredicate:"
  , "  " <> "@staticmethod"
  , "  " <> "def build_angle(key: Union[" <> pythonKeyValues <>"]) -> " <>
    "Tuple[str, Struct]:"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  , ""
  , "  " <> "@staticmethod"
  , "  " <> "def angle_query(*, arg: str) -> \"GleanSchemaPredicate\":"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  ]) :
  [ ("py" </>
      Text.unpack (Text.concat namespaces) <.> "py",
      Text.intercalate (newline <> newline)
        ( header :
          genPredicateImports namespaces preds :
          [genAllPredicates namespaces preds]
        )
    )
  | (namespaces, (_, preds, _)) <- schemas
  ]
  where
    schemas = HashMap.toList declsPerNamespace
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

genAllPredicates
  :: NameSpaces
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates _ preds = Text.unlines $
  [ Text.unlines
    [ "class " <> class_name <> "(GleanSchemaPredicate):"
      , "  " <> "@staticmethod"
      , "  " <> "def build_angle(key: Union[" <> pythonKeyValues <>
        "]) -> Tuple[str, Struct]:"
      , "    " <> "return f\"" <> predicateName <> "." <>
        showt (predicateRef_version ref) <> " {" <> angleFor key <>
        "}\", " <> return_class_name
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
          kTy = valueTy key
          predicateName = predicateRef_name ref
          class_name = pythonClassName predicateName
          return_class_name = returnPythonClassName predicateName
  ]

header :: Text
header = Text.unlines
  [ "# \x40generated"
  , "# To regenerate this file run fbcode//glean/schema/gen/sync"
  , "from typing import Tuple, Type, Union, TypeVar"
  , "import json"
  , "from thrift.py3 import Struct"
  , "from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate"
  ]

pythonClassName :: Text -> Text
pythonClassName c = Text.intercalate "" $ map cap1 $ Text.split (== '.') c

returnPythonClassName :: Text -> Text
returnPythonClassName c = Text.intercalate "" $
                          tail $
                          map cap1 $ Text.split (== '.') c

genTargets
  :: HashMap NameSpaces ([NameSpaces], [ResolvedPredicateDef], [ResolvedTypeDef])
  -> Text
genTargets info =
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
    in
    -- mini Python library for the module containing allPredicates
    [ "python_library("
    , "  name = \"" <> namespace <> "\","
    , "  srcs = [\"" <> Text.concat ns <> ".py\"],"
    , "  deps = ["
    , "    " <> "\"//glean/schema/py:glean_schema_predicate\","
    , "    " <> "\"//glean/schema/thrift:" <> namespace <> "-py3-types\","
    , "    " <> "\"//thrift/lib/py3:types\","
    , "  ]"
    , ")"
    , ""
    ]

-- | Generate a value type
valueTy :: ResolvedType -> Text
valueTy t = case t of
  RecordTy [] -> createQueryField (defaultFieldName, baseTy t)
  RecordTy fields ->
    let
      fieldNames = map fieldDefName fields
      fieldTypes = map (baseTy . fieldDefType) fields
      queryFields =  zipWith (curry createQueryField) fieldNames fieldTypes
    in
      Text.intercalate ", " queryFields
  _ -> createQueryField (defaultFieldName, baseTy t)

  where
    createQueryField (name, type_) = name <> ": " <> type_
    defaultFieldName = Text.pack "arg"


pythonKeyValues :: Text
pythonKeyValues = Text.pack "int, bool, str, Tuple[()]"

angleFor :: Type_ pref tref -> Text
angleFor key = case key of
  RecordTy [] -> Text.pack "json.dumps(key)"
  RecordTy fields ->
    let
      fieldNames = map fieldDefName fields
      setValue field = field <> " = " <> "_" -- TODO accept field value
      setValues = map setValue
    in
    "{ " <> Text.intercalate ", " (setValues fieldNames) <> " }"

  _ -> Text.pack "json.dumps(key)"

baseTy :: Type_ pref tref -> Text
baseTy t = case t of
  NatTy{} -> Text.pack "int"
  BooleanTy{} -> Text.pack "bool"
  StringTy{} -> Text.pack "str"
  RecordTy [] -> Text.pack "Tuple[()]"
  -- TODO other types
  _ -> Text.pack "Tuple[()]"

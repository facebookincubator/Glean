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
  , "from typing import Tuple, Union"
  , ""
  , "class GleanSchemaPredicate:"
  , "  " <> "@staticmethod"
  , "  " <> "def build_angle(key: Union[" <> pythonKeyValues <>"]) -> str:"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  , ""
  , "  " <> "@staticmethod"
  , "  " <> "def angle_query(*, name: str) -> \"GleanSchemaPredicate\":"
  , "    " <> "raise Exception" <>
    "(\"this function can only be called from @angle_query\")"
  ]) :
  [ ("py" </>
      Text.unpack (Text.concat namespaces) <.> "py",
      Text.intercalate (newline <> newline)
        (header : [genAllPredicates namespaces preds])
    )
  | (namespaces, (_, preds, _)) <- schemas
  ]
  where
    schemas = HashMap.toList declsPerNamespace
    declsPerNamespace =
      addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs

genAllPredicates
  :: NameSpaces
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates _ preds = Text.unlines $
  [ Text.unlines
    [ "class GS" <> class_name <> "(GleanSchemaPredicate):"
      , "  " <> "@staticmethod"
      , "  " <> "def build_angle(key: Union[" <> pythonKeyValues <>
        "]) -> str:"
      , "    " <> "return f\"" <> predicateRef_name ref <> "." <>
      showt (predicateRef_version ref) <> " { " <> angleFor kTy <> " }\""
      , ""
      , "  " <> "@staticmethod"
      , "  " <> "def angle_query(*, name: " <> kTy <> ") -> \"GS" <>
        class_name <> "\":"
      , "    " <> "raise Exception" <>
        "(\"this function can only be called from @angle_query\")"
    ]
    | pred <- preds
    , let ref = predicateDefRef pred
          key = predicateDefKeyType pred
          kTy = valueTy key
          class_name = pythonClassName $ predicateRef_name ref
  ]

header :: Text
header = Text.unlines
  [ "# \x40generated"
  , "# To regenerate this file run fbcode//glean/schema/gen/sync"
  , "from typing import Tuple, Union"
  , "import json"
  , "from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate"
  ]

pythonClassName :: Text -> Text
pythonClassName c = Text.intercalate "" $ map cap1 $ Text.split (== '.') c

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
     , ")"
     , ""
     ] ++
     concatMap genTarget (HashMap.keys info)
  where
  genTarget ns =
    let
      namespace = underscored ns
    in
    -- mini Haskell library for the module containing allPredicates
    [ "python_library("
    , "  name = \"" <> namespace <> "\","
    , "  srcs = [\"" <> Text.concat ns <> ".py\"],"
    , "  deps = [\"//glean/schema/py:glean_schema_predicate\"]"
    , ")"
    , ""
    ]

-- | Generate a value type
valueTy :: ResolvedType -> Text
valueTy t = case t of
  NatTy{} -> Text.pack "int"
  BooleanTy{} -> Text.pack "bool"
  StringTy{} -> Text.pack "str"
  RecordTy{} -> Text.pack "Tuple[()]"
  -- TODO other types
  _ -> Text.pack "str"

pythonKeyValues :: Text
pythonKeyValues = Text.pack "int, bool, str, Tuple[()]"

angleFor :: Text -> Text
angleFor keyType = case Text.unpack keyType of
 "Tuple[()]" -> Text.pack "{ }"
 _ -> Text.pack "json.dumps(key)"

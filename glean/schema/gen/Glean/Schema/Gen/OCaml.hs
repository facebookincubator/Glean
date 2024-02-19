{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Generate "Glean.Schema"
module Glean.Schema.Gen.OCaml
  ( genSchemaOCaml
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Schema.Gen.Utils
    ( NameSpaces,
      NamePolicy(predNames, typeNames),
      sortDeclsByNamespace,
      newline,
      cap1,
      mkNamePolicy,
      addNamespaceDependencies )
import Glean.Angle.Types
    ( PredicateRef,
      TypeRef,
      Version,
      PredicateDef_(predicateDefRef, predicateDefKeyType,
                    predicateDefValueType),
      TypeDef_(typeDefRef, typeDefType),
      FieldDef_(fieldDefType, fieldDefName),
      Type_(..) )
import Glean.Schema.Types
    ( ResolvedPredicateDef, ResolvedTypeDef, ResolvedType )
import Data.Char (isUpper, toLower)

generated :: Text
generated = Text.concat
  [ "regenerate: buck2 run fbcode//glean/schema/gen:gen-schema  "
  , "-- --ocaml fbcode/hphp/hack/src/typing/write_symbol_info/schema "
  , "--dir DEST_DIR"
  ]

dune :: [Text] -> Text
dune modules = Text.unlines
  [ "; \x40generated"
  , "; " <> generated
  ,  "(library"
  , " (name glean_schema)"
  , " (wrapped false)"
  , "(modules"
  , " " <> Text.intercalate "\n " modules <> ")"
  , "(libraries"
  , " utils_core"
  , " base64)"
  , "(preprocess"
  , " (pps ppx_deriving.std)))"
  ]

ocamlHeader :: Text
ocamlHeader = Text.unlines
  [ "(*"
  , " * Copyright (c) Meta Platforms, Inc. and affiliates."
  , " * All rights reserved."
  , " *"
  , " * This source code is licensed under the BSD-style license found in the"
  , " * LICENSE file in the root directory of this source tree."
  , " *)"
  , ""
  , "(* \x40generated"
  , "   " <> generated <> " *)"
  , ""
  , "open Hh_json"
  , ""
  ]

factIdMl :: Text
factIdMl = ocamlHeader <> Text.unlines
  [ ""
  , "type t = int [@@deriving ord]"
  , ""
  , "let next ="
  , "  let x = ref 1 in"
  , "  fun () ->"
  , "    let r = !x in"
  , "    x := !x + 1;"
  , "    r"
  , ""
  , "let to_json_number i = JSON_Number (string_of_int i)"
  , ""
  , "module Map = Map.Make (struct"
  , "  type t = int"
  , ""
  , "  let compare = Int.compare"
  , "end)"
  , ""
  ]

utilMl :: Text
utilMl = ocamlHeader <> Text.unlines
  [ ""
  , "let key v = JSON_Object [(\"key\", v)]"
  , ""
  , "let id fact_id = JSON_Object [(\"id\", Fact_id.to_json_number fact_id)]"
  , ""
  , "let rem_opt (name, json_opt) ="
  , "  match json_opt with"
  , "  | Some json -> Some (name, json)"
  , "  | None -> None"
  , ""
  ]

targetsHeader :: Text
targetsHeader = Text.unlines
  [ "# \x40generated"
  , "# " <> generated
  , "load(\"@fbcode_macros//build_defs:ocaml_library.bzl\", \"ocaml_library\")"
  , ""
  ]

targetsLib :: Text -> [Text] -> [Text] -> Text
targetsLib name srcs deps = Text.unlines
  [ "ocaml_library("
  , "  name = \"" <> name <> "\","
  , "  srcs = ["
  , Text.intercalate "\n" (buckItem <$> srcs)
  , "  ],"
  , "  deps = ["
  , Text.intercalate "\n" (buckItem <$> deps)
  , "  ],"
  , "  preprocess = ["
  , "    \"ppx_deriving.show\","
  , "    \"ppx_deriving.eq\","
  , "    \"ppx_deriving.ord\","
  , "    \"ppx_deriving.enum\""
  , "  ],"
  , "  external_deps = ["
  , "    (\"supercaml\", None, \"base64\")"
  , "  ]"
  , ")"
  , ""
  ]
  where buckItem x = "    \"" <> x <> "\","

moduleDecl :: Int -> Text
moduleDecl 0 = "module rec "
moduleDecl _ = "and "

typeDecl :: (Text, Text, Text, Text) -> Text
typeDecl (name, type_, _var, _code ) =
  Text.concat ["  and ", name, "= ", type_, "\n  [@@deriving ord]"]

toJsonDecl :: (Text, Text, Text, Text) -> Text
toJsonDecl (name, _type, _var, _code ) =
  Text.concat ["  val to_json_", name, ": ", name, " -> json"]

toJsonImpl :: (Text, Text, Text, Text) -> Text
toJsonImpl (name, _type, var, code ) =
  Text.concat ["  and to_json_", name, " ", var, " = ", code]

genModuleType :: Int -> Text -> Text -> Text -> Text -> Text
genModuleType decl name type_ var code = Text.unlines
  [ moduleDecl decl <> name <> ": sig"
  , "  type t = " <> type_
  , "  [@@deriving ord]"
  , ""
  , "  val to_json : t -> json"
  , "end = struct"
  , "  type t = " <> type_
  , "  [@@deriving ord]"
  , ""
  , "  let to_json " <> var <> " = " <> code
  , "end"
  ]

genModulePred :: Int -> Text -> [(Text,  Text, Text, Text)] -> Text
genModulePred decl name types = Text.unlines
  [ moduleDecl decl <> name <> ": sig"
  , "  type t ="
  , "    | Id of Fact_id.t"
  , "    | Key of key"
  , "  [@@deriving ord]"
  , ""
  , Text.intercalate "\n" (typeDecl <$> types)
  , ""
  , "  val to_json: t -> json"
  , ""
  , Text.intercalate "\n" (toJsonDecl <$> types)
  , ""
  , "end = struct"
  , "  type t ="
  , "    | Id of Fact_id.t"
  , "    | Key of key"
  , "  [@@deriving ord]"
  , ""
  , Text.intercalate "\n" (typeDecl <$> types)
  , ""
  , "  let rec to_json = function"
  , "    | Id f -> Util.id f"
  , "    | Key t -> Util.key (to_json_key t)"
  , ""
  , Text.intercalate "\n" (toJsonImpl <$> types)
  , "end"
  ]

data FieldKind = Sum | Record

-- | Generate the type representing a predicate or type
genOCamlType :: NameSpaces -> NamePolicy -> ResolvedType -> Text
genOCamlType ns namePolicy t = case t of
  ByteTy -> "char"
  NatTy -> "int"
  StringTy -> "string"
  ArrayTy ByteTy -> "string list"
  ArrayTy ty -> genOCamlType ns namePolicy ty <> " list"
  RecordTy [] -> "unit"
  RecordTy fields -> "{\n" <> Text.unlines (genField Record <$> fields) <> "  }"
  SumTy fields -> "\n" <> Text.intercalate "\n" (genField Sum <$> fields)
  SetTy _ty -> error "Set"
  PredicateTy pred -> predToModule ns pred namePolicy <> ".t"
  NamedTy tref -> typeToModule ns tref namePolicy <> ".t"
  MaybeTy ty -> genOCamlType ns namePolicy ty <> " option"
  EnumeratedTy names ->
    "\n" <> Text.unlines (("    | " <>) . nameToConstructor <$> names)
  BooleanTy -> "bool"
  where
    genField fieldKind field =
      let ty = (genOCamlType' ns namePolicy . fieldDefType) field in
      case fieldKind of
        Record -> "    " <> fieldToVar field <> ": " <> ty <> ";"
        Sum -> "     | " <> fieldToConstructor field <> " of " <> ty

--   Unlike Angle, OCaml doesn't support anonymous record or sum
--   types, so we can't translate them directly. TODO.
genOCamlType' :: NameSpaces -> NamePolicy -> ResolvedType -> Text
genOCamlType' ns namePolicy t = Text.pack $ case t of
  RecordTy _ -> "unit (* unsupported *)"
  SumTy _ -> "unit (* unsupported *)"
  EnumeratedTy _ -> "unit (* unsupported *)"
  _ -> Text.unpack $ genOCamlType ns namePolicy t

genOCamlToJson :: NameSpaces -> NamePolicy -> ResolvedType -> (Text, Text)
genOCamlToJson ns namePolicy t = case t of
  ByteTy -> ("b", "JSON_Number (string_of_int (int_of_char b))")
  NatTy -> ("i", "JSON_Number (string_of_int i)")
  StringTy -> ("str", "JSON_String str")
  ArrayTy ByteTy ->
    ("v", "JSON_String (List.map Base64.encode_string v |> String.concat \"\")")
  ArrayTy ty ->
    let (var, code) = genOCamlToJson ns namePolicy ty in
    ("l", "JSON_Array (List.map (fun " <> var <> " -> " <> code  <> ") l)")
  RecordTy [] ->
    ("_", "JSON_Object []")
  RecordTy fields ->
    let fieldNames = fieldToVar <$> fields in
    let objField field =
          let var = fieldToVar field
              key = fieldToJSONKey field
              type_ = fieldDefType field
              genType = genOCamlToJson' var ns namePolicy type_
              expr =
                case type_ of
                  MaybeTy _ -> ["(\"", key, "\", ", genType, ");"]
                  _ -> ["(\"", key, "\", Some (", genType, "));"] in
              "      " <> Text.concat expr in
    let vars = "{" <> Text.intercalate "; " fieldNames <> "}" in
    let code =
          (objField <$> fields) ++ ["    ] |> List.filter_map Util.rem_opt)"] in
    (vars, "\n    JSON_Object ([\n" <> Text.unlines code)
  SumTy fields ->
    let typeSumField field =
          let genType = genOCamlToJson' var ns namePolicy (fieldDefType field)
              var = fieldToVar field
              key = fieldToJSONKey field
              constr = fieldToConstructor field in
          Text.concat [ "     | ", constr, " ", var,
                        " -> JSON_Object [(\"", key, "\", ", genType, ")]"] in
      ("", "function\n" <> Text.unlines (typeSumField <$> fields))
  SetTy _ -> error "Set"
  PredicateTy pred ->
    let moduleName = predToModule ns pred namePolicy in
        ("x",  moduleName <> ".to_json x")
  NamedTy tref ->
    let moduleName = typeToModule ns tref namePolicy in
        ("x",  moduleName <> ".to_json x")
  MaybeTy ty -> let genTy = genOCamlToJson' "x" ns namePolicy ty in
    ("x", "Option.map (fun x -> " <> genTy <> ") x")
  EnumeratedTy names ->
    let enumNames = zipWith (\idx val -> (idx, val)) [(0::Int)..] names in
    let enumCase (num, name) =
          Text.concat
            ["     | ", nameToConstructor name,
             " -> JSON_Number (string_of_int ", Text.pack $ show num, ")"] in
    let handleEnumCases = enumCase <$> enumNames in
        ("", "function\n" <> Text.unlines handleEnumCases)
  BooleanTy -> ("b", "JSON_Bool b")

genOCamlToJson' :: Text -> NameSpaces -> NamePolicy -> ResolvedType -> Text
genOCamlToJson' var ns namePolicy t = case t of
  ByteTy -> "JSON_Number (string_of_int (int_of_char " <> var <> "))"
  NatTy -> "JSON_Number (string_of_int " <> var <> ")"
  StringTy -> "JSON_String " <> var
  ArrayTy ByteTy ->
    "JSON_String (List.map Base64.encode_string " <> var
    <> "|> String.concat \"\")"
  ArrayTy ty ->
    let code = genOCamlToJson' "x" ns namePolicy ty in
    "JSON_Array (List.map (fun x -> " <> code  <> ") " <> var <> ")"
  RecordTy _ -> "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)"
  SumTy _ -> "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)"
  SetTy _ -> error "Set"
  PredicateTy pred ->
    let moduleName = predToModule ns pred namePolicy in
        (moduleName <> ".to_json " <> var)
  NamedTy tref ->
    let moduleName = typeToModule ns tref namePolicy in
        moduleName <> ".to_json " <> var
  MaybeTy ty ->
    let genTy = genOCamlToJson' "x" ns namePolicy ty in
    "Option.map (fun x -> " <> genTy <> ") " <> var
  EnumeratedTy _ -> "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)"
  BooleanTy -> "JSON_Bool " <> var

refToModule :: NameSpaces -> Maybe (NameSpaces, Text) -> Text
refToModule curNs ref =
  case ref of
    Just (ns :: NameSpaces, x) ->
      let toplevelMod = if ns == curNs then
             ""
           else
             cap1 $ Text.intercalate "_" ns <> "." in
       toplevelMod <> cap1 x
    _ -> error $ "OCamlName: " ++ show ref

typeToModule :: NameSpaces -> TypeRef -> NamePolicy -> Text
typeToModule curNs ref namePolicy =
  let map = typeNames namePolicy in
  refToModule curNs (HashMap.lookup ref map)

predToModule :: NameSpaces -> PredicateRef -> NamePolicy -> Text
predToModule curNs ref namePolicy =
  let map = predNames namePolicy in
  refToModule curNs (HashMap.lookup ref map)

nameToConstructor :: Text -> Text
nameToConstructor = cap1

fieldToJSONKey :: FieldDef_ PredicateRef TypeRef -> Text
fieldToJSONKey field = fieldDefName field

fieldToConstructor :: FieldDef_ PredicateRef TypeRef -> Text
fieldToConstructor field = cap1 (fieldDefName field)

fieldToVar :: FieldDef_ PredicateRef TypeRef -> Text
fieldToVar field =
  from_ (camelToUnderscore (fieldDefName field))
  where
    from_ name
      | Text.unpack name `elem` keywords = name <> "_"
      | otherwise = name
      where
        keywords = ["begin", "end", "method", "type", "module", "to", "val",
                    "object"]

    camelToUnderscore = Text.pack . go . Text.unpack
      where
        go [] = []
        go (x:xs)
          | isUpper x = '_' : toLower x : go xs
          | otherwise = x : go xs

nsToTarget :: NameSpaces -> Text
nsToTarget n = Text.intercalate "_" n

nsToFileName :: NameSpaces -> Text
nsToFileName n = nsToTarget n <> ".ml"

genSchema
 :: NameSpaces
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> NamePolicy
  -> Text
genSchema namespaces preds types namePolicy =
      Text.intercalate newline [ocamlHeader, predModules, typeModules]
    where
      predModules =
        Text.unlines [genModuleP num namespaces namePolicy pred |
          (num, pred) <- zipWith (\idx val -> (idx, val)) [0..]  preds]
      typeModules =
        let startValue = length preds in
        Text.unlines [genModuleT num namespaces namePolicy type_ |
          (num, type_) <- zipWith (\idx val -> (idx, val)) [startValue..] types]
      genModuleP (num :: Int) ns namePolicy pred =
        let ref = predicateDefRef pred
            key = predicateDefKeyType pred
            value = predicateDefValueType pred
            kTy = genOCamlType ns namePolicy key
            kTy' = genOCamlType ns namePolicy value
            name = predToModule ns ref namePolicy
            (var, code) = genOCamlToJson ns namePolicy key
            (var', code') = genOCamlToJson ns namePolicy value in
            case value of
              RecordTy [] ->
                genModulePred num name [("key", kTy, var, code)]
              _ ->
                genModulePred num name
                  [("key", kTy, var, code), ("value", kTy', var', code')]
      genModuleT (num :: Int) ns namePolicy type_ =
        let ref = typeDefRef type_
            key = typeDefType type_
            kTy = genOCamlType ns namePolicy key
            name = typeToModule ns ref namePolicy
            (var, code) = genOCamlToJson ns namePolicy key in
            genModuleType num name kTy var code

genTargets :: [(NameSpaces, [NameSpaces])] -> Text
genTargets deps =
  targetsHeader
  <> targetsLib "common" ["util.ml", "fact_id.ml"] [jsonDep]
  <> Text.concat (genTarget <$> deps)
  where
  jsonDep = "//hphp/hack/src/utils/hh_json:hh_json"
  genTarget (ns, imports) =
    let
      extra = (":" <>) . nsToTarget <$> imports
      name = nsToTarget ns
      file = nsToFileName ns
      targetDeps = [ ":common", jsonDep ] ++ extra in
    targetsLib name [file] targetDeps

-- entry point
-- generate all files, possibly restricted to schemas in [NameSpaces]
genSchemaOCaml
  :: Maybe [NameSpaces]
  -> Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> [(FilePath,Text)]
genSchemaOCaml onlySchemas _version preddefs typedefs =
  ( "dune", dune modules) :
  ( "TARGETS", genTargets deps) :
  ( "fact_id.ml", factIdMl) :
  ( "util.ml", utilMl) :
  [ (Text.unpack (nsToFileName namespaces),
    genSchema namespaces preds types namePolicy)
    | (namespaces, (_, preds, types)) <- schemas
  ]
  where
    filter = case onlySchemas of
      Nothing -> \_ _ -> True
      Just schemas -> (\x _ -> elem x schemas)
    declsPerNamespace = HashMap.filterWithKey filter (
      addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs)
    schemas = HashMap.toList declsPerNamespace
    deps = (\(x, (y, _, _)) -> (x, y)) <$> schemas
    namePolicy = mkNamePolicy preddefs typedefs
    modules' = nsToTarget . fst <$> deps
    modules = ["util", "fact_id"] ++ modules'

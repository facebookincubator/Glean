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
import Data.List (partition)
import qualified Data.Text as Text
import Control.Monad.State

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
  , " core"
  , " base64)"
  , "(preprocess"
  , " (pps ppx_deriving.std)))"
  ]

ocamlHeaderGen :: [Text] -> Text
ocamlHeaderGen imports = Text.unlines $
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
  ] ++ (("open " <>) <$> imports) ++ [ "" ]

-- disable warning for "unused module imports". Not all generated
-- files use it.
ocamlHeader :: Text
ocamlHeader =  ocamlHeaderGen ["Hh_json", "Core [@@warning \"-33\"]"]

ocamlHeader' :: Text
ocamlHeader' =  ocamlHeaderGen ["Hh_json"]

-- TODO use "Core.Map"
factIdMl :: Text
factIdMl = ocamlHeader' <> Text.unlines
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
  , "    (\"supercaml\", None, \"base64\"),"
  , "    (\"supercaml\", None, \"core\")"
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
genOCamlType :: NameSpaces -> NamePolicy -> ResolvedType -> State () Text
genOCamlType ns namePolicy t = case t of
  ByteTy -> return "char"
  NatTy -> return "int"
  StringTy -> return "string"
  ArrayTy ByteTy -> return "string list"
  ArrayTy ty -> do
    t <- genOCamlType ns namePolicy ty
    return $ t <> " list"
  RecordTy [] -> return "unit"
  RecordTy fields -> do
    f <- mapM (genField Record) fields
    return $ "{\n" <> Text.unlines f <> "  }"
  SumTy fields -> do
    f <- mapM (genField Sum) fields
    return $ "\n" <> Text.intercalate "\n" f
  SetTy _ty -> error "Set"
  PredicateTy pred -> return $ predToModule ns pred namePolicy <> ".t"
  NamedTy tref -> return $ typeToModule ns tref namePolicy <> ".t"
  MaybeTy ty -> do
    t <- genOCamlType ns namePolicy ty
    return $ t <> " option"
  EnumeratedTy names ->
    return $ "\n" <> Text.unlines (("    | " <>) . nameToConstructor <$> names)
  BooleanTy -> return "bool"
  TyVar{} -> error "genOCamlType: TyVar"
  HasTy{} -> error "genOCamlType: HasTy"
  where
    genField fieldKind field = do
      ty <- genOCamlTypeFromField ns namePolicy (fieldDefType field)
      return $ case fieldKind of
        Record -> "    " <> fieldToVar field <> ": " <> ty <> ";"
        Sum -> "     | " <> fieldToConstructor field <> " of " <> ty


--   Unlike Angle, OCaml doesn't support anonymous record or sum
--   types, so we can't translate them directly. TODO.
genOCamlTypeFromField
  :: NameSpaces -> NamePolicy -> ResolvedType -> State () Text
genOCamlTypeFromField ns namePolicy t = case t of
  RecordTy (_ : _) -> return "unit (* unsupported *)"
  SumTy _ -> return "unit (* unsupported *)"
  EnumeratedTy _ -> return "unit (* unsupported *)"
  _ -> genOCamlType ns namePolicy t

genOCamlToJson
  :: Text -> NameSpaces -> NamePolicy -> ResolvedType -> State () (Text, Text)
genOCamlToJson var ns namePolicy t = case t of
  ByteTy ->
    return (var, "JSON_Number (string_of_int (int_of_char " <> var <> "))")
  NatTy -> return (var, "JSON_Number (string_of_int " <> var <> ")")
  StringTy -> return (var, "JSON_String " <> var)
  ArrayTy ByteTy ->
    return (var, "JSON_String (List.map ~f:Base64.encode_string " <> var
    <> "|> String.concat ~sep:\"\")")
  ArrayTy ty -> do
    (_, code) <- genOCamlToJson "x" ns namePolicy ty
    return (var, "JSON_Array (List.map ~f:(fun x -> " <> code  <> ") "
      <> var <> ")")
  RecordTy [] ->
    return ("_", "JSON_Object []")
  RecordTy fields -> do
    let fieldNames = fieldToVar <$> fields
        vars = "{" <> Text.intercalate "; " fieldNames <> "}"
        fieldToVarPair field = do
          let camlVar = fieldToVar field
              jsonKey = fieldToJSONKey field
              type_ = fieldDefType field
          (_, camlType) <- genOCamlToJsonFromField camlVar ns namePolicy type_
          let isOpt = case type_ of
                MaybeTy _ -> True
                _ -> False
              camlPair = "(\"" <> jsonKey <> "\", " <> camlType <> ")"
          return (camlVar, camlPair, isOpt)
    fields_ <- mapM fieldToVarPair fields
    let (optFields, reqFields) = partition (\(_, _, isOpt) -> isOpt) fields_
        reqFieldsDefs = Text.unlines (
          let toRegString  (_, pair, _) = "      " <> pair <> ";" in
          [ "    let fields = [" ] <>
          (toRegString <$> reqFields) <>
          [ "    ] in" ])
        optFieldsDefs =
          let toOptFieldDef (var, pair, _) = Text.unlines
                [ "    let fields =",
                  "      match " <> var <> " with",
                  "      | None -> fields",
                  "      | Some " <> var <> " -> " <> pair <>  " :: fields in" ]
          in
          Text.concat $ toOptFieldDef <$> optFields
    return (vars, "\n" <> reqFieldsDefs <> optFieldsDefs
      <> "    JSON_Object fields\n")
  SumTy fields -> do
    let typeSumField field = do
          let var = fieldToVar field
          (_, genType) <-
            genOCamlToJsonFromField var ns namePolicy (fieldDefType field)
          let key = fieldToJSONKey field
          let constr = fieldToConstructor field
          return $ Text.concat [ "     | ", constr, " ", var,
                        " -> JSON_Object [(\"", key, "\", ", genType, ")]"]
    fields <- mapM typeSumField fields
    return ("", "function\n" <> Text.unlines fields)
  SetTy _ -> error "Set"
  PredicateTy pred ->
    let moduleName = predToModule ns pred namePolicy in
        return (var, moduleName <> ".to_json " <> var)
  NamedTy tref ->
    let moduleName = typeToModule ns tref namePolicy in
        return (var, moduleName <> ".to_json " <> var)
  MaybeTy ty -> do
    (_, genTy) <- genOCamlToJson "x" ns namePolicy ty
    return (var, "Option.map ~f:(fun x -> " <> genTy <> ") " <> var)
  EnumeratedTy names ->
    let enumNames = zipWith (\idx val -> (idx, val)) [(0::Int)..] names in
    let enumCase (num, name) =
          Text.concat
            ["     | ", nameToConstructor name,
             " -> JSON_Number (string_of_int ", Text.pack $ show num, ")"] in
    let handleEnumCases = enumCase <$> enumNames in
        return ("", "function\n" <> Text.unlines handleEnumCases)
  BooleanTy -> return (var, "JSON_Bool " <> var)
  TyVar{} -> error "genOCamlToJson: TyVar"
  HasTy{} -> error "genOCamlToJson: HasTy"

genOCamlToJsonFromField
  :: Text -> NameSpaces -> NamePolicy -> ResolvedType -> State () (Text, Text)
genOCamlToJsonFromField var ns namePolicy t = case t of
  RecordTy [] ->
    return (var, "(ignore " <>  var <> "; JSON_Object [])")
  RecordTy _ ->
    return (var, "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)")
  SumTy _ ->
    return (var, "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)")
  EnumeratedTy _ ->
    return (var, "(ignore " <>  var <> "; JSON_Object []) (* unsupported *)")
  MaybeTy ty -> genOCamlToJsonFromField var ns namePolicy ty
  _ -> genOCamlToJson var ns namePolicy t


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
            kTy = evalState (genOCamlType ns namePolicy key) ()
            kTy' = evalState (genOCamlType ns namePolicy value) ()
            name = predToModule ns ref namePolicy
            (var, code) =
              evalState (genOCamlToJson "x" ns namePolicy key) ()
            (var', code') =
              evalState (genOCamlToJson "x" ns namePolicy value) ()
        in case value of
              RecordTy [] -> genModulePred num name [("key", kTy, var, code)]
              _ -> genModulePred num name
                  [("key", kTy, var, code), ("value", kTy', var', code')]
      genModuleT (num :: Int) ns namePolicy type_ =
            genModuleType num name kTy var code
        where
            ref = typeDefRef type_
            key = typeDefType type_
            kTy = evalState (genOCamlType ns namePolicy key) ()
            name = typeToModule ns ref namePolicy
            (var, code) = evalState (genOCamlToJson "x" ns namePolicy key) ()

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

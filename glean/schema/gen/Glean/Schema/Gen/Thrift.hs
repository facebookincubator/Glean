{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-- | Glean's first auto-generation attempt
--
-- Run with
--
-- >  ./buck-out/opt/gen/glean/hs/predicates --thrift=predicates.thrift
--
-- Create fbcode/glean/if/predicates.thrift
--
module Glean.Schema.Gen.Thrift
  ( genSchemaThrift
  ) where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath
import TextShow

import Glean.Schema.Gen.Utils
import Glean.Angle.Types
import Glean.Schema.Util

genSchemaThrift
  :: Mode
  -> Maybe String
  -> Version
  -> [PredicateDef]
  -> [TypeDef]
  -> [(FilePath, Text)]
genSchemaThrift mode versionDir version preddefs typedefs =
  (dir </> "TARGETS",
    genTargets mode slashVn version declsPerNamespace) :
  [ ( dir </> Text.unpack (underscored namespaces) ++ ".thrift"
    , genNamespace mode slashVn namespaces version namePolicy deps preds types)
  | (namespaces, (deps, preds, types)) <- HashMap.toList declsPerNamespace ]
  where
  dir = case mode of
    Data -> "thrift"
    Query -> "thrift/query"

  slashVn = case versionDir of
    Nothing -> ""
    Just str -> "/" <> Text.pack str
  namePolicy = mkNamePolicy preddefs typedefs
  declsPerNamespace =
    addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs


genTargets
  :: Mode
  -> Text   -- "/v1" or ""
  -> Version
  -> HashMap NameSpaces ([NameSpaces], [PredicateDef], [TypeDef])
  -> Text
genTargets mode slashVn version info =
  Text.unlines
     ([ "# @" <> "generated"
     , "# to regenerate: ./glean/schema/sync"
     , "load(\"@fbcode_macros//build_defs:custom_rule.bzl\", \"custom_rule\")"
     , "" ] ++
     concatMap genTarget (HashMap.toList info)) <>
  if mode == Data
    then genSchemaRules version
    else ""
  where
  genTarget (ns, (deps, nsPredicates, _)) =
    let
      namespace = underscored ns
      rustCrate = underscored [rustBaseModule mode, namespace]
      cppSplits = showt $ min 20 (max 1 (length nsPredicates))
    in
    [ "thrift_library("
    , "  name = \"" <> namespace <> "\","
    , "  hs_namespace = \"" <> hsNamespace mode <> "\","
    , "  py3_namespace = \"" <> py3Namespace mode <> "\","
    , "  py_base_module = \"" <> pyBaseModule mode <> "\","
    , "  rust_crate_name = \"" <> rustCrate <> "\","
    , "  hs_includes = [\"" <> namespace <> "_include.hs\"],"
    , "  thrift_rust_options = [\"serde\", \"skip_none_serialization\"],"
    , "  thrift_cpp2_options = [" <> Text.intercalate ", " [
      "\"json\"",
      "\"types_cpp_splits=" <> cppSplits <> "\""
      ] <> "],"
    , "  thrift_py_options = \"utf8strings\","
    , "  languages = [" <> Text.intercalate ", " (filter (\n -> n /= "") [
        "\"hs2\"",
        "\"py\"",
        "\"py3\"",
        "\"java-swift\"",
        "\"rust\"",
        "\"cpp2\""
      ]) <> "],"
    , "  thrift_srcs = { \"" <> namespace <> ".thrift\" : [] },"
    , "  deps = [" <> Text.intercalate ","
      ( "\"//glean/if:glean\"" : depTargets mode ++
          (case mode of
            Query -> depTargets Data
            Data -> [])) <> "],"
    , "  hs2_deps = ["
    , "    \"//glean/hs:typed\","
    , "    \"//glean/hs:query-angle\","
    , "    \"//glean/if:glean-hs2\","
    ] ++ (
      case mode of
        Data -> []
        Query ->
          ["    \"//" <> thriftDir Data slashVn <> ":" <> namespace <> "-hs2\""]
    ) ++
    [ "  ],"
    , ")"
    ]
    where
    depTargets mode =
      [ "\"//" <> thriftDir mode slashVn <> ":" <> underscored dep <> "\""
      | dep <- deps ]

-- TODO: this shouldn't really go under schema/thrift
genSchemaRules :: Version -> Text
genSchemaRules version = rule_schema_gen
  where
    rule_schema_gen = Text.unlines
      [ "custom_rule("
      , "    name = \"cpp_schema\","
      , "    build_args ="
      , "        \"--cpp schema.h \" +"
      , "        \"--version " <> showt version <> " \" +"
      , "        \"--dir $(location //glean/schema/source:sources)\","
      , "    build_script_dep = \"//glean/schema/gen:gen-schema\","
      , "    output_gen_files = [\"schema.h\"],"
      , ")"
      ]

thriftDir :: Mode -> Text -> Text
thriftDir Query slashVn = "glean/schema" <> slashVn <> "/thrift/query"
thriftDir _ slashVn = "glean/schema" <> slashVn <> "/thrift"

hsNamespace :: Mode -> Text
hsNamespace Query = "Glean.Schema.Query"
hsNamespace _ = "Glean.Schema"

cpp2Namespace :: Mode -> Text
cpp2Namespace Query = "facebook.glean.schema.query"
cpp2Namespace _ = "facebook.glean.schema"

phpNamespace :: Mode -> Text
phpNamespace Query = "glean_schema_query"
phpNamespace _ = "glean_schema"

pyBaseModule :: Mode -> Text
pyBaseModule Query = "glean.schema.query"
pyBaseModule _ = "glean.schema"

py3Namespace :: Mode -> Text
py3Namespace Query = "glean.schema.query"
py3Namespace _ = "glean.schema"

javaBaseModule :: Mode -> Text
javaBaseModule Query = "com.facebook.glean.schema.query"
javaBaseModule _ = "com.facebook.glean.schema"

rustBaseModule :: Mode -> Text
rustBaseModule Query = "glean_schema_query"
rustBaseModule _ = "glean_schema"

{- -----------------------------------------------------------------------------
Namespaces

We're going to generate:

   sys.thrift
   cxx.thrift
   buck.thrift
   clang.thrift
   clang_pp.thrift
   etc.

inside cxx.thrift we will have

  import clang.thrift -- refer to things in here as clang.Foo

  namespace hs Glean.Schema  -- module will be Glean.Schema.Cxx
  namespace py glean.schema.cxx -- module will be glean.schema.cxx
  namespace php glean_schema

  struct RecordDeclaration {
    1: clang.Class name;
    ...
  }
-}

genNamespace
  :: Mode
  -> Text                               -- "/v1" or ""
  -> NameSpaces
  -> Version
  -> NamePolicy
  -> [NameSpaces]
  -> [PredicateDef]
  -> [TypeDef]
  -> Text
genNamespace mode slashVn namespaces version namePolicy deps preddefs typedefs
 = Text.intercalate (newline <> newline) (header : pieces)
 where
  someDecls = map PredicateDecl preddefs ++ map TypeDecl typedefs
  ordered = orderDecls someDecls
  (gen :: [[Text]], extra :: [Text]) = runM mode [] namePolicy typedefs $ do
     ps <- mapM (genDecl namespaces) ordered
     return (map fst ps ++ map snd ps)
  pieces = concat gen ++ reverse extra

  genDecl :: NameSpaces -> SomeDecl -> M ([Text],[Text])
  genDecl ns (PredicateDecl p) = genPred ns p
  genDecl ns (TypeDecl t) = ([],) <$> genType ns t

  namespace = dotted namespaces

  preddefToVersionPair :: PredicateDef -> M Text
  preddefToVersionPair PredicateDef{..} = do
    (_, name) <- predicateName predicateDefRef
    let ver = showt (predicateRef_version predicateDefRef)
      in return $ "\"" <> name <> "\": " <> ver <> ","

  predicateVersionPairs = fst $
    runM mode [] namePolicy typedefs (mapM preddefToVersionPair preddefs)

  predicateVersionMap =
    [ "const map<string, i64> PREDICATE_VERSIONS = {" ] ++
    indentLines predicateVersionPairs ++
    [ "}" ]

  header = Text.unlines $
    [ "// To re-generate this file:"
    , "//"
    , "//   ./glean/schema/sync"
    , "//"
    , "// @" <> "generated"
    , "// @" <> "nolint"
    , ""
    , "include \"glean/if/glean.thrift\"" ] ++
    [ "include \"" <> thriftDir mode slashVn <> "/" <> underscored dep
        <> ".thrift\""
    | dep <- deps ] ++
    [ ""
    , "namespace cpp2 " <> cpp2Namespace mode <> "." <> dotted namespaces
    , "namespace hs " <> hsNamespace mode
    , "namespace php " <> phpNamespace mode <> "_" <> underscored namespaces
    , "namespace py " <> pyBaseModule mode <> "." <> underscored namespaces
    , "namespace py3 " <> py3Namespace mode
    , "namespace java.swift " <> javaBaseModule mode <> "."
      <> underscored namespaces
    , "namespace rust " <> rustBaseModule mode <> "_" <> underscored namespaces
    , ""
    , "hs_include \"glean/schema" <> slashVn
      <> "/thrift/" <> (case mode of Data -> ""; Query -> "query/")
      <>  underscored namespaces <> "_include.hs\""
    ] ++
    predicateVersionMap ++
    -- builtin only
    if namespace /= "builtin" then [] else
    [ "// Schema version"
    , "const i64 version = " <> showt version
    ]



-- -----------------------------------------------------------------------------

addExtraDecls :: M [Text] -> M [Text]
addExtraDecls act = do
  most <- act
  extra <- popDefs
  return (extra ++ most)

indentLines :: [Text] -> [Text]
indentLines = map (\t -> if Text.null t then t else "  " <> t)

-- The schema validator has assumed that we don't have any problematic
-- identifiers, so we can just use the name directly.

optionalize :: Text -> Text
optionalize name = "optional " <> name

shareTypeDef :: NameSpaces -> Type -> M Text
shareTypeDef here t = do
  (no, name) <- nameThisType t
  case no of
    Old -> return ()
    New -> do
      let dNew = TypeDef
            { typeDefRef = TypeRef name 0
            , typeDefType = t }
      pushDefs =<< genType here dNew
  return name


thriftTy :: NameSpaces -> Type -> M Text
thriftTy here t = case t of
  -- Basic types
  Byte{} -> return "glean.Byte"
  Nat{} -> return "glean.Nat"
  Boolean{} -> return "bool"
  String{} -> return "string"
  -- Containers
  Array Byte -> return "binary"
  Array ty -> do
    query <- isQuery <$> getMode
    if query then do
      -- like shareTypeDef but we want to hint the typedef name, not
      -- the element type. This is so that the element type is
      -- consistent with the non-query type.
      (_, name) <- withRecordFieldHint "array" $ nameThisType t
      let dNew = TypeDef
            { typeDefRef = TypeRef name 0
            , typeDefType = t }
      pushDefs =<< genType here dNew
      return name
    else do
      inner <- thriftTy here ty
      return $ "list<" <> inner  <> ">"
  Record{} -> shareTypeDef here t
  Sum{} -> shareTypeDef here t
  Maybe tInner -> do
    query <- isQuery <$> getMode
    if query then
      shareTypeDef here (lowerMaybe tInner)
    else do
      inner <- thriftTy here tInner
      return (optionalize inner)
  -- References
  Predicate pred -> do
    mode <- getMode
    thriftName mode here <$> predicateName pred
  NamedType typeRef -> do
    mode <- getMode
    thriftName mode here <$> typeName typeRef
  Enumerated _ -> shareTypeDef here t

mkField :: [Text] -> Text -> Int -> Name -> Text -> Text
mkField annots structOrUnion i p t =
  showt i <> ": " <> t <> " " <> p <> annotText <> ";"
  where
  -- The java.swift codegen likes to strip underscores from the end of
  -- names for some reason.
  javaAnnot
    | "_" `Text.isSuffixOf` p = ["java.swift.name = \"" <> p <> "\""]
    | otherwise = []

  py3Annot
    | p == "from" = ["py3.name = \"from_\""]
    | p == "type" && structOrUnion == "union"  = ["py3.name = \"type_\""]
    | p == "name" && structOrUnion == "union" = ["py3.name = \"name_\""]
    | p == "value" && structOrUnion == "union" = ["py3.name = \"value_\""]
    | otherwise = []

  allAnnots = javaAnnot ++ py3Annot ++ annots

  annotText
    | null allAnnots = ""
    | otherwise = " (" <> Text.intercalate ", " allAnnots <> ")"

makeField :: Text -> Int -> Name -> Text -> Text
makeField = mkField []

makeRefField :: Text -> Int -> Name -> Text -> Text
makeRefField = mkField
  [ "cpp.ref = \"true\""
  , "cpp2.ref = \"true\""
  , "rust.box"
  , "swift.recursive_reference = \"true\""
  ]

genPred :: NameSpaces -> PredicateDef -> M ([Text], [Text])
genPred here PredicateDef{..} = do
  mode <- getMode
  let query = isQuery mode
      structOrUnion = if query then "union" else "struct"

  pName <- predicateName predicateDefRef
  let name = thriftName mode here pName

  -- Note: This withPredicateDefHint covers either Mode Data or Mode Query
  withPredicateDefHint (snd pName) $ do

  keyNeedsRef <- needsRefType predicateDefKeyType
  valNeedsRef <- needsRefType predicateDefValueType

  let
    appendName suffix = case pName of (ns, x) -> (ns, x <> suffix)

    name_id = appendName "_id"
    name_key = appendName "_key"
    name_value = appendName "_value"
    has_value = predicateDefValueType /= unitT

    -- key and value are optional unless we're generating query types
    maybeOpt = if query then id else optionalize
    maybeRef needed
      | query = makeField structOrUnion
      | needed = makeRefField structOrUnion
      | otherwise = makeField structOrUnion

  -- generate
  --   typedef Foo_id = glean.Id
  -- These aren't very useful except for documentation purposes, so that
  -- you can refer to an Id by its typeref name in APIs.
  (type_id, define_id) <- do
    let target_type = NamedType (TypeRef "glean.Id" 0)
    type_id <- thriftTy here target_type
    let d = "typedef " <> type_id <> " " <> thriftName mode here name_id
    new_alias <- thriftTy here (NamedType (TypeRef (joinDot name_id) 0))
    return (new_alias, d)

  (type_key, define_key) <-
    if shouldNameKeyType predicateDefKeyType
      then define_kt here predicateDefKeyType name_key
      else do
        keyTy <- thriftTy here predicateDefKeyType
        return (keyTy, [])

  -- Define a type for the value, only if it's not Unit
  (type_value, define_value) <-
    if not has_value then return ("^", []) else do
      define_kt here predicateDefValueType name_value
  let
    annotation = myUnlines
      [ "@glean.PredicateAnnotation" <> "{"
      , myUnlines . indentLines $
        [ "name=\"" <> predicateRef_name predicateDefRef <> "\";"
        , "version=" <> showt (predicateRef_version predicateDefRef)  <> ";"
        ]
      , "}"
      ]

    -- Predicate types can be recursive so those need to be refs in C++.
    key i = Just $ maybeRef keyNeedsRef i "key" (maybeOpt type_key)
    val i
      | has_value = Just $ maybeRef valNeedsRef i "value" (maybeOpt type_value)
      | otherwise = Nothing
    define = (:[]) $ myUnlines . concat $
      [ [ annotation ]
      , [ structOrUnion <> " " <> name <> " {" ]
      , indentLines . catMaybes . zipWith (flip ($)) [1..] $
        [ \i -> Just $ mkField ["hs.strict"] structOrUnion i "id" type_id
        , key ] ++
        (if query then
           [ \i -> Just $ makeField structOrUnion i "get" "builtin.Unit" ]
         else
           [ val ]
        )
      , [ "}" <>
            -- The Haskell Thrift compiler will generate a constructor
            -- <name>_key for the key alternative of the union type,
            -- which would conflict with the <name>_key constructor we
            -- generate for the type of the key field. So we use a custom
            -- (hs_prefix = "<name>_with_") for the union type to avoid
            -- the name clash. This gives us constructors like
            -- Predicate_with_key, Predicate_with_id.
            (if query
              then " (hs.prefix = \"" <> name <> "_with_\")"
              else "")
        ]
      ]
  extra <- popDefs
  return (define_id : define,  extra ++ define_key ++ define_value)

needsRefType :: Type -> M Bool
needsRefType t = do
  realType <- repType t
  case realType of
    Nothing ->
      -- type not found in this module or known imported environment,
      -- so no recusive loop, so we do not need a ref type
      return False
    Just rep -> case rep of
      Record{} -> return True
      Sum{} -> return True
      Predicate{} -> return True
      Array t -> needsRefType t
      Maybe t -> needsRefType t
      _ -> return False

-- Make the thriftTy type text, and the [Text] declaration block
define_kt :: NameSpaces -> Type -> (NameSpaces, Text) -> M (Text, [Text])
define_kt here typ name_kt = do
  let gname = joinDot name_kt
  ref <- thriftTy here (NamedType (TypeRef gname 0))
  def <- genType here TypeDef
    { typeDefRef = TypeRef gname 0
    , typeDefType = typ }
  return (ref,def)

makeEnumerated :: Text -> [Name] -> M [Text]
makeEnumerated name vals = do
  let
    pieces = Text.intercalate "," (zipWith mkEnumerator [0..] vals)
    declare = "enum " <> name <> " {" <> pieces <> "\n}"
  return [declare]
  where
    mkEnumerator (i :: Int) val = "\n  " <> val <> " = " <> showt i <> annotText
      where
        -- I do not think we need py3.name for type to type_ here
        py3Annot
          | val == "name" = ["py3.name = \"name_\""]
          | val == "value" = ["py3.name = \"value_\""]
          | otherwise = []
        annotText
          | null py3Annot = ""
          | otherwise = " (" <> Text.intercalate ", " py3Annot <> ")"

genType :: NameSpaces -> TypeDef -> M [Text]
genType here TypeDef{..} = addExtraDecls $ do
  tName@(_, root) <- typeName typeDefRef
  mode <- getMode
  let query = isQuery mode
  let name = thriftName mode here tName

  withTypeDefHint root $ do

  let
    structLike structOrUnion fields withFieldHint extraFields = do
      fieldTexts <- forM (zip [1..] fields) $ \(ix, FieldDef nm ty) -> do
         tyName <- withFieldHint nm (thriftTy here ty)
         if query
           then do
             b <- needsRefType ty
             return $
               (if b then makeRefField else makeField)
                 structOrUnion ix nm (optionalize tyName)
           else
             return $ makeField structOrUnion ix nm tyName
      let
        define | null fields = "struct " <> name <> " {}"
               | otherwise = myUnlines $ concat
          [ [ structOrUnion <> " " <> name <> " {" ]
          , indentLines (fieldTexts ++ extraFields)
          , [ "}" ]
          ]
      return [define]

  case typeDefType of
    Record fields -> structLike "struct" fields withRecordFieldHint []
    Sum fields -> structLike structOrUnion fields withUnionFieldHint anyField
      where
        structOrUnion = if query then "struct" else "union"
        anyField
          | query = [ showt (length fields + 1) <> ": bool any = false;" ]
          | otherwise = []

    -- queries on arrays:
    Array ty | query, not (isByt ty) -> do
      inner <- thriftTy here ty
      let
        fields =
          [ "1: " <> inner <> " every;"
          , "2: list<" <> inner <> "> exact;"
          ]
        define = myUnlines $
          [ "union " <> name <> " {" ] ++ indentLines fields ++ [ "}" ]
      return [define]

    Enumerated vals -> makeEnumerated name vals

    _other_ty -> do
      t <- thriftTy here typeDefType
      return ["typedef " <> t <> " " <> name]

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
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
import Glean.Schema.Types
import Glean.Types (SchemaId(..))

genSchemaThrift
  :: Maybe String
  -> SchemaId
  -> Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> [(FilePath, Text)]
genSchemaThrift versionDir hash version preddefs typedefs =
  (dir </> "TARGETS",
    genTargets slashVn declsPerNamespace) :
  [ ( dir </> Text.unpack (underscored namespaces) ++ ".thrift"
    , genNamespace slashVn namespaces version
        hash namePolicy deps preds types)
  | (namespaces, (deps, preds, types)) <- HashMap.toList declsPerNamespace ]
  where
  dir = "thrift"

  slashVn = case versionDir of
    Nothing -> ""
    Just str -> "/" <> Text.pack str
  namePolicy = mkNamePolicy preddefs typedefs
  declsPerNamespace =
    addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs


genTargets
  :: Text   -- "/v1" or ""
  -> HashMap NameSpaces ([NameSpaces], [ResolvedPredicateDef], [ResolvedTypeDef])
  -> Text
genTargets slashVn info =
  Text.unlines
     ([ "# \x40generated"
     , "# to regenerate: ./glean/schema/sync"
     , "load(\"@fbcode_macros//build_defs:custom_rule.bzl\", \"custom_rule\")"
     , "load(\"@fbcode_macros//build_defs:thrift_library.bzl\", \"thrift_library\")"
     , "" ] ++
     concatMap genTarget (HashMap.toList info))
  where
  genTarget (ns, (deps, nsPredicates, _)) =
    let
      namespace = underscored ns
      cppSplits = showt $ min 20 (max 1 (length nsPredicates))
    in
    [ "thrift_library("
    , "  name = \"" <> namespace <> "\","
    , "  hs_namespace = \"" <> hsNamespace <> "\","
    , "  py3_namespace = \"" <> py3Namespace <> "\","
    , "  py_base_module = \"" <> pyBaseModule <> "\","
    ] ++
    [ "  hs_includes = [\"" <> namespace <> "_include.hs\"]," ] ++
    [ "  thrift_rust_options = ["
    , "    \"serde\","
    , "    \"skip_none_serialization\","
    , "    \"deprecated_default_enum_min_i32\","
    , "],"
    , "  thrift_cpp2_options = [" <> Text.intercalate ", " [
      "\"json\"",
      "\"types_cpp_splits=" <> cppSplits <> "\""
      ] <> "],"
    , "  thrift_py_options = \"utf8strings\","
    , "  languages = [" <> Text.intercalate ", " langs <> "],"
    , "  thrift_srcs = { \"" <> namespace <> ".thrift\" : [] },"
    , "  deps = [" <> Text.intercalate ","
      ( "\"//glean/if:glean\"" : depTargets ) <>
      ", \"//thrift/annotation:thrift\"],"
    , "  hs2_deps = ["
    , "    \"//glean/hs:angle\","
    , "    \"//glean/hs:typed\","
    , "    \"//glean/hs:query-angle\","
    , "    \"//glean/if:glean-hs2\","
    ] ++
    [ "  ],"
    , ")"
    ]
    where
    depTargets =
      [ "\"//" <> thriftDir slashVn <> ":" <> underscored dep <> "\""
      | dep <- deps ]

    langs :: [Text]
    langs = map (\x -> "\"" <> x <> "\"") [
        "hs2",
        "py",
        "py3",
        "python",
        "java-swift",
        "rust",
        "cpp2"
        ]

thriftDir :: Text -> Text
thriftDir slashVn = "glean/schema" <> slashVn <> "/thrift"

package :: Text
package = "facebook.com/glean/schema"

hsNamespace :: Text
hsNamespace = "Glean.Schema"

cpp2Namespace :: Text
cpp2Namespace = "facebook.glean.schema"

phpNamespace :: Text
phpNamespace = "glean_schema"

pyBaseModule :: Text
pyBaseModule = "glean.schema"

py3Namespace :: Text
py3Namespace = "glean.schema"

javaBaseModule :: Text
javaBaseModule = "com.facebook.glean.schema"

rustBaseModule :: Text
rustBaseModule = "glean_schema"

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
  :: Text                               -- "/v1" or ""
  -> NameSpaces
  -> Version
  -> SchemaId
  -> NamePolicy
  -> [NameSpaces]
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> Text
genNamespace slashVn namespaces version
    hash namePolicy deps preddefs typedefs
 = Text.intercalate (newline <> newline) (header : pieces)
 where
  someDecls = map PredicateDecl preddefs ++ map TypeDecl typedefs
  ordered = orderDecls someDecls
  (gen :: [[Text]], extra :: [Text]) = runM [] namePolicy typedefs $ do
     ps <- mapM (genDecl namespaces) ordered
     return (map fst ps ++ map snd ps)
  pieces = concat gen ++ reverse extra

  genDecl :: NameSpaces -> SomeDecl -> M ([Text],[Text])
  genDecl ns (PredicateDecl p) = genPred ns p
  genDecl ns (TypeDecl t) = ([],) <$> genType ns t

  namespace = dotted namespaces

  preddefToVersionPair :: ResolvedPredicateDef -> M Text
  preddefToVersionPair PredicateDef{..} = do
    (_, name) <- predicateName predicateDefRef
    let ver = showt (predicateRef_version predicateDefRef)
      in return $ "\"" <> name <> "\": " <> ver <> ","

  predicateVersionPairs = fst $
    runM [] namePolicy typedefs (mapM preddefToVersionPair preddefs)

  predicateVersionMap =
    [ "const map<string, i64> PREDICATE_VERSIONS = {" ] ++
    indentLines predicateVersionPairs ++
    [ "}" ]

  header = Text.unlines $
    [ "// To re-generate this file:"
    , "//"
    , "//   ./glean/schema/sync"
    , "//"
    , "// \x40generated"
    , "// @" <> "nolint"
    , ""
    , "include \"glean/if/glean.thrift\"" ] ++
    [ "include \"" <> thriftDir slashVn <> "/" <> underscored dep
        <> ".thrift\""
    | dep <- deps ] ++
    [ "include \"thrift/annotation/thrift.thrift\""
    , ""
    , allowReservedIdentifierNameAnnotation (underscored namespaces)
    <> "package \"" <> package <> "/" <> underscored namespaces <> "\""
    ] ++
    [ ""
    , "namespace cpp2 " <> cpp2Namespace <> "." <> dotted namespaces
    , "namespace hs " <> hsNamespace
    , "namespace php " <> phpNamespace <> "_" <> underscored namespaces
    , "namespace py " <> pyBaseModule <> "." <> underscored namespaces
    , "namespace py3 " <> py3Namespace
    , "namespace java.swift " <> javaBaseModule <> "."
      <> underscored namespaces
    , "namespace rust " <> rustBaseModule <> "_" <> underscored namespaces
    , ""
    ] ++
    [ "hs_include \"glean/schema" <> slashVn
      <> "/thrift/"
      <>  underscored namespaces <> "_include.hs\""
    ] ++
    predicateVersionMap ++
    -- builtin only
    if namespace /= "builtin" then [] else
    [ "// Schema version"
    , "const i64 version = " <> showt version
    , "const glean.SchemaId schema_id = " <> Text.pack (show (unSchemaId hash))
    ]



-- -----------------------------------------------------------------------------

allowReservedIdentifierNameAnnotation :: Text -> Text
allowReservedIdentifierNameAnnotation identifier
    | "fbthrift"
      == Text.toLower (Text.take (Text.length "fbthrift") (Text.dropWhile (=='_') identifier))
        = "@thrift.AllowReservedIdentifierName "
    | otherwise = ""

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

shareTypeDef :: NameSpaces -> ResolvedType -> M Text
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


thriftTy :: NameSpaces -> ResolvedType -> M Text
thriftTy here t = case t of
  -- Basic types
  ByteTy{} -> return "glean.Byte"
  NatTy{} -> return "glean.Nat"
  BooleanTy{} -> return "bool"
  StringTy{} -> return "string"
  -- Containers
  ArrayTy ByteTy -> return "binary"
  ArrayTy ty -> do
    inner <- thriftTy here ty
    return $ "list<" <> inner  <> ">"
  RecordTy{} -> shareTypeDef here t
  SumTy{} -> shareTypeDef here t
  SetTy ty -> do
    inner <- thriftTy here ty
    return $ "set<" <> inner <> ">"
  MaybeTy tInner -> do
    inner <- thriftTy here tInner
    return (optionalize inner)
  -- References
  PredicateTy pred ->
    thriftName here <$> predicateName pred
  NamedTy typeRef -> do
    thriftName here <$> typeName typeRef
  EnumeratedTy _ -> shareTypeDef here t
  TyVar{} -> error "thriftTy: TyVar"
  HasTy{} -> error "thriftTy: HasTy"

mkField :: [Text] -> Text -> Int -> Name -> Text -> Text
mkField annots structOrUnion i p t =
  allowReservedIdentifierNameAnnotation p
    <> showt i <> ": " <> t <> " " <> p <> annotText <> ";"
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

genPred :: NameSpaces -> ResolvedPredicateDef -> M ([Text], [Text])
genPred here PredicateDef{..} = do
  let structOrUnion = "struct"

  pName <- predicateName predicateDefRef
  let name = thriftName here pName

  withPredicateDefHint (snd pName) $ do

  keyNeedsRef <- needsRefType predicateDefKeyType
  valNeedsRef <- needsRefType predicateDefValueType

  let
    appendName suffix = case pName of (ns, x) -> (ns, x <> suffix)

    name_id = appendName "_id"
    name_key = appendName "_key"
    name_value = appendName "_value"
    has_value = predicateDefValueType /= unitT

    -- key and value are optional
    maybeOpt = optionalize
    maybeRef needed
      | needed = makeRefField structOrUnion
      | otherwise = makeField structOrUnion

  -- generate
  --   typedef Foo_id = glean.Id
  -- These aren't very useful except for documentation purposes, so that
  -- you can refer to an Id by its typeref name in APIs.
  (type_id, define_id) <- do
    let target_type = NamedTy (TypeRef "glean.Id" 0)
    type_id <- thriftTy here target_type
    new_alias <- thriftTy here (NamedTy (TypeRef (joinDot name_id) 0))
    let name = thriftName here name_id
        d = allowReservedIdentifierNameAnnotation name <> "typedef "
          <> type_id <> " " <> name
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
      , [ allowReservedIdentifierNameAnnotation name
        <> structOrUnion <> " " <> name <> " {" ]
      , indentLines . catMaybes . zipWith (flip ($)) [1..] $
        [ \i -> Just $ mkField ["hs.strict"] structOrUnion i "id" type_id
        , key, val ]
      , [ "}" ]
      ]
  extra <- popDefs
  return (define_id : define,  extra ++ define_key ++ define_value)

needsRefType :: ResolvedType -> M Bool
needsRefType t = do
  realType <- repType t
  case realType of
    Nothing ->
      -- type not found in this module or known imported environment,
      -- so no recusive loop, so we do not need a ref type
      return False
    Just rep -> case rep of
      RecordTy{} -> return True
      SumTy{} -> return True
      PredicateTy{} -> return True
      ArrayTy t -> needsRefType t
      MaybeTy t -> needsRefType t
      _ -> return False

-- Make the thriftTy type text, and the [Text] declaration block
define_kt :: NameSpaces -> ResolvedType -> (NameSpaces, Text) -> M (Text, [Text])
define_kt here typ name_kt = do
  let gname = joinDot name_kt
  ref <- thriftTy here (NamedTy (TypeRef gname 0))
  def <- genType here TypeDef
    { typeDefRef = TypeRef gname 0
    , typeDefType = typ }
  return (ref,def)

makeEnumerated :: Text -> [Name] -> M [Text]
makeEnumerated name vals = do
  let
    pieces = Text.intercalate "," (zipWith mkEnumerator [0..] vals)
    declare = allowReservedIdentifierNameAnnotation name
      <> "enum " <> name <> " {" <> pieces <> "\n}"
  return [declare]
  where
    mkEnumerator (i :: Int) val = "\n  "
      <> allowReservedIdentifierNameAnnotation val
      <> val <> " = " <> showt i <> annotText
      where
        -- I do not think we need py3.name for type to type_ here
        py3Annot
          | val == "name" = ["py3.name = \"name_\""]
          | val == "value" = ["py3.name = \"value_\""]
          | otherwise = []
        annotText
          | null py3Annot = ""
          | otherwise = " (" <> Text.intercalate ", " py3Annot <> ")"

genType :: NameSpaces -> ResolvedTypeDef -> M [Text]
genType here TypeDef{..} = addExtraDecls $ do
  tName@(_, root) <- typeName typeDefRef
  let name = thriftName here tName

  withTypeDefHint root $ do

  let
    structLike structOrUnion fields withFieldHint extraFields = do
      fieldTexts <- forM (zip [1..] fields) $ \(ix, FieldDef nm ty) -> do
         tyName <- withFieldHint nm (thriftTy here ty)
         return $ makeField structOrUnion ix nm tyName
      let
        define | null fields = allowReservedIdentifierNameAnnotation name
          <> "struct " <> name <> " {}"
               | otherwise = myUnlines $ concat
          [ [ allowReservedIdentifierNameAnnotation name
            <> structOrUnion <> " " <> name <> " {" ]
          , indentLines (fieldTexts ++ extraFields)
          , [ "}" ]
          ]
      return [define]

  case typeDefType of
    RecordTy fields -> structLike "struct" fields withRecordFieldHint []
    SumTy fields -> structLike structOrUnion fields withUnionFieldHint anyField
      where
        structOrUnion = "union"
        anyField = []

    EnumeratedTy vals -> makeEnumerated name vals

    _other_ty -> do
      t <- thriftTy here typeDefType
      return [allowReservedIdentifierNameAnnotation name
        <> "typedef " <> t <> " " <> name]

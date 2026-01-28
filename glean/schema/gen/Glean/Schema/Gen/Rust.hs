{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | Glean's Rust schema generation
--
module Glean.Schema.Gen.Rust
  ( genSchemaRust
  ) where

import Control.Monad
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
import Data.List.Extra (nubOrd)

genSchemaRust
  :: SchemaId
  -> Version
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> Maybe Oncall
  -> [(FilePath, Text)]
genSchemaRust hash version preddefs typedefs _ =
  [ ( dir </> Text.unpack (underscored namespaces) ++ ".rs"
    , genNamespace namespaces version
        hash namePolicy preds types)
  | (namespaces, (_deps, preds, types)) <- HashMap.toList declsPerNamespace ]
  where
  dir = "schema"
  namePolicy = mkNamePolicy preddefs typedefs
  declsPerNamespace =
    addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs

{- -----------------------------------------------------------------------------

We're going to generate:

   python.rs
   src.rs
   builtin.rs
   etc.

-}

genNamespace
  :: NameSpaces
  -> Version
  -> SchemaId
  -> NamePolicy
  -> [ResolvedPredicateDef]
  -> [ResolvedTypeDef]
  -> Text
genNamespace namespaces version
    hash namePolicy preddefs typedefs
 = Text.intercalate newline $
    header : [Text.intercalate (newline <> newline) pieces]
 where
  someDecls = map PredicateDecl preddefs ++
    map (\TypeDef{..} -> TypeDecl typeDefRef typeDefType) typedefs
  ordered = orderDecls someDecls
  (gen :: [[Text]], extra :: [Text]) = runM [] namePolicy typedefs $ do
     ps <- mapM (genDecl namespaces) ordered
     return (map fst ps ++ map snd ps)
  pieces = concat gen ++ reverse extra

  genDecl :: NameSpaces -> SomeDecl -> M ([Text],[Text])
  genDecl ns (PredicateDecl p) = genPred ns p
  genDecl ns (TypeDecl tref ty) = ([],) <$> genType ns tref ty

  namespace = dotted namespaces

  header = Text.unlines $
    [ "/*"
    , " * Copyright (c) Meta Platforms, Inc. and affiliates."
    , " *"
    , " * This source code is licensed under the MIT license found in the"
    , " * LICENSE file in the root directory of this source tree."
    , " *"
    , " * \x40generated"
    , " * Regenerate with glean/schema/gen/Glean/Schema/Gen/Rust.hs"
    , " *  buck2 run glean/schema/gen:gen-schema -- --dir glean/schema/source --rust pyrefly/pyrefly/lib/report/glean"
    , " */"
    , ""
    , "#![allow(warnings)]"
    , "use serde::Deserialize;"
    , "use serde::Serialize;"
    , "use serde_json::Value;"
    , "use serde_repr::*;"
    , ""
    , "use crate::report::glean::schema::*;"
    , "use crate::report::glean::facts::GleanPredicate;"
    ] ++
    -- builtin only
    if namespace /= "builtin" then [] else
    [ "// Schema version"
    , "pub const VERSION: i64 = " <> showt version <> ";"
    , "pub const SCHEMA_ID: &str = \"" <> unSchemaId hash <> "\";"
    ]

-- -----------------------------------------------------------------------------

addExtraDecls :: M [Text] -> M [Text]
addExtraDecls act = do
  most <- act
  extra <- popDefs
  return (nubOrd (extra ++ most))

indentLines :: [Text] -> [Text]
indentLines = map (\t -> if Text.null t then t else "    " <> t)

addIndent :: Int -> Text
addIndent n = Text.replicate n "    "

optionalize :: Text -> Text
optionalize name = "Option<" <> safe name <> ">"

box :: Text -> Text
box name = "Box<" <> safe name <> ">"

safe :: Text -> Text
safe name
  | name `elem`
      [ "type", "super", "crate", "self", "as", "mut", "mod", "box"
      , "ref", "const", "dyn", "use", "extern", "unsafe", "trait"
      , "macro", "Value"
      ]
      = name <> "_"
  | otherwise = if name == "builtin.Unit" then "builtin::Unit" else name

shareTypeDef :: NameSpaces -> ResolvedType' s ->  M Text
shareTypeDef here t = do
  (no, name) <- nameThisType t
  case no of
    Old -> return ()
    New -> do
      let tref = TypeRef name 0
      pushDefs =<< genType here tref t
  return name

rustTy :: NameSpaces -> ResolvedType' s -> M Text
rustTy here t = case t of
  -- Basic types
  ByteTy{} -> return "u8"
  NatTy{} -> return "u64"
  BooleanTy{} -> return "bool"
  StringTy{} -> return "String"
  -- Containers
  ArrayTy ByteTy -> return "Vec<u8>"
  ArrayTy ty -> do
    inner <- rustTy here ty
    return $ "Vec<" <> inner  <> ">"
  RecordTy{} -> shareTypeDef here t
  SumTy{} -> shareTypeDef here t
  SetTy ty -> do
    inner <- rustTy here ty
    return $ "Vec<" <> inner <> ">"
  MaybeTy tInner -> do
    inner <- rustTy here tInner
    return (optionalize inner)
  -- References
  PredicateTy _ pred ->
    rustName here <$> predicateName pred
  NamedTy _ typeRef -> do
    rustName here <$> typeName typeRef
  EnumeratedTy _ -> shareTypeDef here t
  TyVar{} -> error "rustTy: TyVar"
  HasTy{} -> error "rustTy: HasTy"
  HasKey{} -> error "rustTy: HasKey"
  ElementsOf{} -> error "rustTy: ElementsOf"

rustName :: NameSpaces -> (NameSpaces, Text) -> Text
rustName here (ns, name)
  | ns == here = safe name
  | null ns = safe name
  | Text.null (underscored ns) = safe name
  | otherwise = underscored ns <> "::" <> safe name

data ShouldUseRepr = UseRepr | NoRepr
annotation :: ShouldUseRepr -> Text
annotation UseRepr = "#[derive(Clone, Debug, Deserialize_repr, Eq, Hash, PartialEq, Serialize_repr)]"
annotation NoRepr =  "#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]"

wrapInBoxIf :: Bool -> Text -> Text
wrapInBoxIf useBox text =
  if useBox
    then "Box::new(" <> text <> ")"
    else text

-- Returns a tuple of the param string and the construction string
getFieldsForConstructor :: NameSpaces -> ResolvedType' s -> Text -> Text -> Bool
                        -> M ([Text], Text)
getFieldsForConstructor here ty typeName defaultParamName useBox = case ty of
  RecordTy fields -> do
      params <- forM fields $ \(FieldDef nm fieldTy) -> do
        tyName <- withRecordFieldHint nm (rustTy here fieldTy)
        return $ safe nm <> ": " <> tyName
      let fieldAssignments = map (\(FieldDef nm _) -> safe nm) fields
          construction = typeName <> " {"
                          <>  "\n" <> addIndent 4
                          <> Text.intercalate
                              (",\n" <> addIndent 4)
                              fieldAssignments <>
                              "\n" <> addIndent 3 <> "}"
      return (params, wrapInBoxIf useBox construction)

  _ -> do
    let param = defaultParamName <> ": " <> typeName
    return ([param], wrapInBoxIf useBox defaultParamName)

genPred :: NameSpaces -> ResolvedPredicateDef -> M ([Text], [Text])
genPred here PredicateDef{..} = do
  pName <- predicateName predicateDefRef
  let
    name = rustName here pName
    type_id = "u64"

  withPredicateDefHint (snd pName) $ do

  let
    appendName suffix = case pName of (ns, x) -> (ns, x <> suffix)
    name_key = appendName "_key"
    name_value = appendName "_value"
    has_value = predicateDefValueType /= unitT


  (type_key, define_key) <-
    if shouldNameKeyType predicateDefKeyType
      then define_kt here predicateDefKeyType name_key
      else do
        keyTy <- rustTy here predicateDefKeyType
        return (keyTy, [])

  -- Define a type for the value, only if it's not Unit
  (type_value, define_value) <-
    if not has_value
        then return ("()", [])
        else do
            define_kt here predicateDefValueType name_value
  -- Generate the new() function parameters and construction
  (keyParams, keyConstruction) <-
    getFieldsForConstructor here predicateDefKeyType type_key "key" True
  (valueParams, valueConstruction) <-
    if has_value
      then getFieldsForConstructor here predicateDefValueType type_value "value" False
      else return ([], "")
  let allParams = keyParams ++ valueParams
      paramList = Text.intercalate ", " allParams

  let
    -- Predicate struct definition
    define = (:[]) $ myUnlines $ concat
        [ [ annotation NoRepr ]
        , [ "pub struct " <> name <> " {" ]
        , indentLines $ catMaybes
          [ Just $ "pub id: " <> type_id <> ","
          , Just $ "pub key: " <> box type_key <> ","
          , if has_value
            then Just $ "pub value: " <> type_value <> ","
            else Nothing
          ]
        , [ "}" ]
        , [ ""]
        , [ "impl " <> name <> " {" ]
        , indentLines
            [ "pub fn new(" <> paramList <> ") -> Self {"
            , addIndent 1 <> name <> " {"
            , addIndent 2 <> "id: 0,"
            , addIndent 2 <> "key: " <> keyConstruction <> ","
            ] ++
            [addIndent 3
              <> "value: " <> valueConstruction
              <> ","
            | has_value] ++
            [ addIndent 2 <> "}"
            , addIndent 1 <> "}"
            ]
        , [ "}" ]
        , [ "" ]
        , [ "impl GleanPredicate for " <> name <> " {"]
        , indentLines
            [ "fn GLEAN_name() -> String {"
            , addIndent 1
              <> "String::from(\""
              <> predicateRef_name predicateDefRef
              <> "."
              <> showt (predicateRef_version predicateDefRef)
              <> "\")"
            , "}"
            , ""
            ]
          , [ "}" ]
        ]
  extra <- popDefs
  return (define,  nubOrd (extra ++ define_key ++ define_value))

define_kt ::
  NameSpaces -> ResolvedType -> (NameSpaces, Text) -> M (Text, [Text])
define_kt here typ name_kt = do
  let gname = joinDot name_kt
      tref = TypeRef gname 0
  ref <- rustTy here (NamedTy () (TypeRef gname 0))
  def <- genType here tref typ
  return (ref,def)

makeEnum :: Text -> [Name] -> M [Text]
makeEnum name vals = do
  let
    variants = Text.unlines $ indentLines (map (<> ",") vals)
    declare = annotation UseRepr <> newline
      <> "#[repr(u8)]" <> newline
      <> "pub enum " <> name <> " {" <> newline <> variants <> "}"
  return [declare]


genType :: NameSpaces -> TypeRef -> ResolvedType' s -> M [Text]
genType here tref ty = addExtraDecls $ do
  tName@(_, root) <- typeName tref
  let name = rustName here tName

  withTypeDefHint root $ do
  case ty of
    RecordTy fields -> do
        fieldTexts <- forM fields $ \(FieldDef nm ty) -> do
          tyName <- withRecordFieldHint nm (rustTy here ty)
          let isOption = case ty of MaybeTy _ -> True; _ -> False
          let fieldLine = "pub " <> safe nm <> ": " <> tyName <> ","
          return $ if isOption
                   then [
                    "#[serde(skip_serializing_if = \"Option::is_none\")]"
                    , fieldLine
                    ]
                   else [fieldLine]
        let
          define | null fields =
            annotation NoRepr <> newline <>
            "pub struct " <> name <> " {}"
                | otherwise = myUnlines $ concat
            [ [ annotation NoRepr ]
            , [ "pub struct " <> name <> " {" ]
            , indentLines (concat fieldTexts)
            , [ "}" ]
            ]
        return [define]
    SumTy fields -> do
        variantTexts <- forM fields $ \(FieldDef nm ty) -> do
          tyName <- withUnionFieldHint nm (rustTy here ty)
          return $ safe nm <> "(" <> safe tyName <> "),"
        let
          define | null fields =
            annotation NoRepr <> newline <>
            "pub enum " <> name <> " {}"
                 | otherwise = myUnlines $ concat
            [ [ annotation NoRepr ]
            , [ "pub enum " <> name <> " {" ]
            , indentLines variantTexts
            , [ "}" ]
            ]
        return [define]

    EnumeratedTy vals -> makeEnum name vals

    _other_ty -> do
      t <- rustTy here ty
      return ["pub type " <> name <> " = " <> t <> ";"]

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
    [ "/*!"
    , " *  Copyright (c) Meta Platforms, Inc. and affiliates."
    , " *"
    , " *  This source code is licensed under the MIT license found in the"
    , " *  LICENSE file in the root directory of this source tree."
    , " */"
    , ""
    , "#![allow(warnings)]"
    , "use serde::Deserialize;"
    , "use serde::Serialize;"
    , "use serde_json::Value;"
    , ""
    , "use crate::report::glean::schema::*;"
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
  return (extra ++ most)

indentLines :: [Text] -> [Text]
indentLines = map (\t -> if Text.null t then t else "    " <> t)

optionalize :: Text -> Text
optionalize name = "Option<" <> name <> ">"

box :: Text -> Text
box name = "Box<" <> name <> ">"

shareTypeDef :: NameSpaces -> ResolvedType' s -> M Text
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
  ByteTy{} -> return "Byte"
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
  | ns == here = name
  | otherwise = underscored ns <> "::" <> name


annotation :: Text
annotation = "#[derive(Clone, Debug, Deserialize, Eq, Hash, "
    <> "PartialEq, Serialize)]"

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
  let
    -- Predicate struct definition
    define = (:[]) $ myUnlines $ concat
        [ [ annotation ]
        , [ "pub struct " <> name <> " {" ]
        , indentLines $ catMaybes
          [ Just $ "pub id: " <> type_id <> ","
          , Just $ "pub key: " <> box type_key <> ","
          , if has_value
            then Just $ "pub value: " <> type_value <> ","
            else Nothing
          ]
        , [ "}" ]
        ]
  extra <- popDefs
  return (define,  extra ++ define_key ++ define_value)

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
    declare = annotation <> newline
      <> "pub enum " <> name <> " {" <> newline <> variants <> "}"
  return [declare]


genType :: NameSpaces -> TypeRef -> ResolvedType' s -> M [Text]
genType here tref ty = addExtraDecls $ do
  tName@(_, root) <- typeName tref
  let name = rustName here tName

  withTypeDefHint root $ do

  let
    structLike structType fields withFieldHint = do
      fieldTexts <- forM fields $ \(FieldDef nm ty) -> do
         tyName <- withFieldHint nm (rustTy here ty)
         return $ "pub " <> nm <> ": " <> tyName <> ","
      let
        define | null fields =
          annotation <> newline <>
          "pub " <> structType <> " " <> name <> " {}"
               | otherwise = myUnlines $ concat
          [ [ annotation ]
          , [ "pub " <> structType <> " " <> name <> " {" ]
          , indentLines fieldTexts
          , [ "}" ]
          ]
      return [define]

  case ty of
    RecordTy fields -> structLike "struct" fields withRecordFieldHint
    SumTy fields -> do
      variantTexts <- forM fields $ \(FieldDef nm ty) -> do
        tyName <- withUnionFieldHint nm (rustTy here ty)
        return $ nm <> "(" <> tyName <> "),"
      let
        define | null fields =
          annotation <> newline <>
          "pub enum " <> name <> " {}"
               | otherwise = myUnlines $ concat
          [ [ annotation ]
          , [ "pub enum " <> name <> " {" ]
          , indentLines variantTexts
          , [ "}" ]
          ]
      return [define]

    EnumeratedTy vals -> makeEnum name vals

    _other_ty -> do
      t <- rustTy here ty
      return ["pub type " <> name <> " = " <> t <> ";"]

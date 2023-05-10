{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-- | Generate "Glean.Schema"
module Glean.Schema.Gen.Haskell
  ( genSchemaHS
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Extra ( nubSort )
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import System.FilePath
import TextShow

import Glean.Schema.Gen.Utils
import Glean.Angle.Types
import Glean.Schema.Types

genSchemaHS :: Version -> [ResolvedPredicateDef] -> [ResolvedTypeDef] -> [(FilePath,Text)]
genSchemaHS _version preddefs typedefs =
  ("hs" </> "TARGETS", genTargets declsPerNamespace) :
  [ ("thrift" </> Text.unpack (underscored namespaces) <> "_include" <.> "hs",
      Text.intercalate (newline <> newline)
        (header Data namespaces deps : doGen Data preds types))
  | (namespaces, (deps, preds, types)) <- schemas
  ] ++
  [ ("hs" </> "Glean" </> "Schema" </>
      Text.unpack (Text.concat (map cap1 namespaces)) <.> "hs",
    genAllPredicates namespaces preds)
  | (namespaces, (_deps, preds, _types)) <- schemas
  ]
  where
    schemas = HashMap.toList declsPerNamespace
    declsPerNamespace =
      addNamespaceDependencies $ sortDeclsByNamespace preddefs typedefs

    namePolicy = mkNamePolicy preddefs typedefs

    doGen
      :: Mode
      -> [ResolvedPredicateDef]
      -> [ResolvedTypeDef]
      -> [Text]
    doGen mode preds types = concat gen ++ reverse extra
      where
      (gen :: [[Text]], extra :: [Text]) = runM mode [] namePolicy typedefs $ do
         ps <- mapM (genPredicate mode) preds
         ts <- mapM (genType mode) types
         return (ps ++ ts)

genTargets
  :: HashMap NameSpaces ([NameSpaces], [ResolvedPredicateDef], [ResolvedTypeDef])
  -> Text
genTargets info =
  Text.unlines $
     [ "# \x40generated"
     , "# to regenerate: ./glean/schema/sync"
     , "load(\"@fbcode_macros//build_defs:haskell_library.bzl\", " <>
       "\"haskell_library\")"
     , "" ] ++
     concatMap genTarget (HashMap.keys info)
  where
  genTarget ns =
    let
      namespace = underscored ns
    in
    -- mini Haskell library for the module containing allPredicates
    [ "haskell_library("
    , "  name = \"" <> namespace <> "\","
    , "  srcs = [\"Glean/Schema/" <> Text.concat (map cap1 ns) <>
        ".hs\"],"
    , "  deps = [\"//glean/if:glean-hs2\"]"
    , ")"
    , ""
    ]


genAllPredicates
  :: NameSpaces
  -> [ResolvedPredicateDef]
  -> Text
genAllPredicates namespace preds = Text.unlines $
  [ "-- @" <> "generated"
  , "module Glean.Schema." <>
    Text.concat (map cap1 namespace) <> " (allPredicates) where"
  , ""
  , "import Glean.Types"
  , ""
  , "allPredicates :: [PredicateRef]"
  , "allPredicates ="
  ] ++
  indentLines (encsep "[ " ", " "]"
    [ "(PredicateRef \"" <> predicateRef_name ref <> "\" " <>
         showt (predicateRef_version ref) <> ")"
    | pred <- preds
    , let ref = predicateDefRef pred
    ])
  where
    encsep start _ end [] = [start <> end]
    encsep start mid end xs =
      zipWith (<>) (start : repeat mid) xs ++ [end]

header :: Mode -> NameSpaces -> [NameSpaces] -> Text
header mode here deps = Text.unlines $
  [ "-- @" <> "generated"
  , "{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}"
  , "{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}"
  , "{-# LANGUAGE UndecidableInstances #-}"
    -- UndecidableInstances is needed for the RecordFields type instances
  , "import qualified Data.ByteString"
  , "import qualified Data.Default"
  , "import qualified Data.Text"
  , ""
    -- we should use qualified imports as far as possible to avoid
    -- clashing with Thrift-generated code
  , "import qualified Glean.Types as Glean"
  , "import qualified Glean.Typed as Glean"
  , "import qualified Glean.Query.Angle as Angle"
  , "import qualified Glean.Angle.Types as Angle"
  , ""
  ] ++
  -- import dependencies
  map (importSchema mode) deps ++
  -- inject this instance into the Builtin schema, because there's no
  -- other good place to put it.
  (case (here, mode) of
    (["builtin"], Data) ->
      [ ""
      , "type instance Angle.SumFields (Prelude.Maybe t) ="
      , "  'Angle.TField \"nothing\" Unit ("
      , "  'Angle.TField \"just\" t"
      , "  'Angle.TNoFields)"
      ]
    _ -> [])


importSchema :: Mode -> NameSpaces -> Text
importSchema mode ns
  | Query <- mode = ""
  | otherwise = data_
  where
  data_ = "import qualified Glean.Schema." <> upperSquashNS <> ".Types"
  upperSquashNS = Text.concat (map cap1 ns)


-- Check against hardcoded list of what glean.h provides
provided :: Text -> Bool
provided = (`elem` known)
  where known = []

-- -----------------------------------------------------------------------------

indent :: Text -> Text
indent t = if Text.null t then t else "  " <> t

indentLines :: [Text] -> [Text]
indentLines = map indent

-- | These names are for predicates/struct/union of Maybe-tree
haskellTypeName :: Mode -> (NameSpaces, Text) -> Text
haskellTypeName query (ns, x) = haskellThriftName query (ns, cap1 x)

fieldName :: (NameSpaces, Text) -> Text
fieldName (ns, x) = haskellThriftName Data (ns, low1 x)

-- | apply stupid and wrong heuristic
paren :: Text -> Text
paren inner =
  let needsParen = Text.any (' ' ==) inner
                   && (Text.take 1 inner `notElem` [ "(", "[" ])
  in if needsParen then "(" <> inner <> ")"
                   else inner

-- | Slightly pretty breaking of long lines at "="
(=@) :: Text -> Text -> Text
(=@) lhs rhs = if Text.length lhs + Text.length rhs + 3 + 4 <= 80
               then lhs <> " = " <> rhs
               else let pre = Text.takeWhile (' '==) lhs
                    in lhs <> " =\n  " <> pre <> indent rhs
infixr 5 =@

optionalize :: Text -> Text
optionalize name =
  let inner = fromMaybe name (Text.stripPrefix "inner " name)
  in "Prelude.Maybe" <>! inner

die :: HasCallStack => String -> String -> a
die x y = error ("Error in genSchemaHS in " <> x <> " : " <> y)

(<>!) :: Text -> Text -> Text
(<>!) a b = a <> " " <> paren b

infixr 6 <>!

genericParamNames :: Text -> Int -> [Text]
genericParamNames base n = map mk [1..n]
  where mk i = base <> Text.pack (show i)

localOrExternal :: NameSpaces -> Text -> (NameSpaces, Text)
localOrExternal here name = if null ns then (here,x) else (ns,x)
  where (ns,x) = splitDot name

shareTypeDef :: Mode -> Bool -> NameSpaces -> ResolvedType -> M Text
shareTypeDef mode genSub here t = do
  (no, name) <- nameThisType t
  case no of
    New | genSub -> do
      let dNew = TypeDef
            { typeDefRef = TypeRef (joinDot (here,name)) 0
            , typeDefType = t }
      pushDefs =<< genType mode dNew
    _otherwise -> return ()
  return (haskellTypeName mode (localOrExternal here name))

haskellTy :: NameSpaces -> ResolvedType -> M Text
haskellTy = haskellTy_ PredName Data True

-- | how to render predicate types in haskellTy
data PredTy = PredName | PredKey

haskellTy_
  :: PredTy
  -> Mode
  -> Bool -- ^ generate nested type definitions
  -> NameSpaces
  -> ResolvedType
  -> M Text
haskellTy_ withId mode genSub here t = case t of
  -- Leafs
  ByteTy{} -> return "Glean.Byte"
  NatTy{} -> return "Glean.Nat"
  BooleanTy{} -> return "Prelude.Bool"
  StringTy{} -> return "Data.Text.Text"
  ArrayTy ByteTy -> return "Data.ByteString.ByteString"
  ArrayTy tInner -> do
    inner <- haskellTy_ PredName mode genSub here tInner
    return $ "[" <> inner <> "]"
  RecordTy{} -> shareTypeDef mode genSub here t
  SumTy{} -> shareTypeDef mode genSub here t
  MaybeTy ty -> do
    inner <- haskellTy_ PredName mode genSub here ty
    return (optionalize inner)
  -- References
  PredicateTy pred -> do
    let wrap = case withId of
          PredName -> id
          PredKey -> ("Glean.KeyType " <>)
    wrap . haskellTypeName mode <$> predicateName pred

  NamedTy typeRef ->
    haskellTypeName mode <$> typeName typeRef
  EnumeratedTy _ -> shareTypeDef mode genSub here t


genPredicate :: Mode -> ResolvedPredicateDef -> M [Text]
genPredicate mode PredicateDef{..}
  | provided (predicateRef_name predicateDefRef) = return []
  | otherwise = do
    pName <- predicateName predicateDefRef
    let
      here = fst pName
      name = haskellTypeName Data pName -- e.g. Clang_File

    withPredicateDefHint (snd pName) $ do
    let
      appendName suffix = let (ns, x) = pName in (ns, x <> suffix)
      glean_name = predicateRef_name predicateDefRef -- e.g. clang.File
      has_value = predicateDefValueType /= unitT
      field_id = fieldName $ appendName "_id" -- field name for Id
      name_key = appendName "_key" -- type of Key
      name_value = appendName "_value" -- type of Value

    (type_key, define_key) <-
      if shouldNameKeyType predicateDefKeyType then
        define_kt mode here predicateDefKeyType name_key
      else do
        ty <- haskellTy_ PredName mode True here predicateDefKeyType
        return (ty, [])

    (type_value, define_value) <-
      if not has_value then return ("Unit", []) else do
        define_kt mode here predicateDefValueType name_value

    let extra = define_key ++ define_value
    let ver = predicateRef_version predicateDefRef
        inst cls body =
          "instance " <> cls <> " " <> name <> " where"
          : indentLines body
        def_Predicate = inst "Glean.Predicate" $
          ["type KeyType " <> name =@ type_key]
          ++
          ["type ValueType " <> name =@ type_value | has_value]
          ++
          [ "getName _proxy " =@ "Glean.PredicateRef " <>
                Text.pack (show glean_name) <> -- adds quotes, does escaping
                showt ver
          , "getId = Glean.IdOf . Glean.Fid . " <> field_id
          , "mkFact (Glean.IdOf (Glean.Fid x)) k "
              <> (if has_value then "v" else "_")
              <> " = " <> name <> " x k"
              <> (if has_value then " v" else "")
          , "getFactKey = " <> fieldName name_key
          , if has_value
              then "getFactValue = " <> fieldName name_value
              else "getFactValue _ = Prelude.Just ()"
          ]

        def_Type = inst "Glean.Type"
          [ "buildRtsValue b = Glean.buildRtsValue b . Glean.getId"
          , "decodeRtsValue = Glean.decodeRef"
          , "decodeAsFact = Glean.decodeFact"
          , "sourceType = Glean.predicateSourceType"
          ]

    return $ extra ++ case mode of
      Data -> map myUnlines [def_Predicate, def_Type]
      Query -> []


-- Make the thriftTy type text, and the needed [Text] blocks
define_kt
  :: HasCallStack
  => Mode
  -> NameSpaces
  -> ResolvedType
  -> (NameSpaces, Text)
  -> M (Text, [Text])
define_kt mode here typ name_kt = case typ of
  ByteTy{} -> leaf
  NatTy{} -> leaf
  StringTy{} -> leaf
  RecordTy [] -> leaf
  RecordTy _fields -> alias typ
  ArrayTy{} -> alias typ
  SumTy [] -> leaf
  SumTy _fields -> alias typ
  MaybeTy{} -> alias typ
  PredicateTy{} -> leaf
  NamedTy{} -> alias typ
  BooleanTy{} -> leaf
  _other -> die "define_kt" (show typ)
 where
   gname = joinDot name_kt

   leaf = (,) <$> return (haskellTypeName mode name_kt) <*> return []

   alias t = do
    ref <- haskellTy here (NamedTy (TypeRef gname 0))
    def <- genType mode TypeDef
      { typeDefRef = TypeRef (joinDot name_kt) 0
      , typeDefType = t }
    return (ref,def)

genType :: Mode -> ResolvedTypeDef -> M [Text]
genType mode TypeDef{typeDefRef = TypeRef{..}, ..}
  | provided typeRef_name = return []
  | otherwise =
  case typeDefType of
    RecordTy fields -> structDef mode typeRef_name typeRef_version fields
    SumTy fields -> unionDef mode typeRef_name typeRef_version fields
    EnumeratedTy vals -> enumDef mode typeRef_name typeRef_version vals
    _ -> return []

structDef :: Mode -> Name -> Version -> [ResolvedFieldDef] -> M [Text]
structDef mode ident ver fields = do
  let typeRef = TypeRef ident ver
  sName@(here,root) <- typeName typeRef
  let name = haskellTypeName Data sName

  withTypeDefHint root $ do
  let
    fieldParamNames = genericParamNames "x" (length fields)
    makeTypeName (FieldDef p tField) =
      withRecordFieldHint p (haskellTy_ PredKey mode True here tField)
    spaced = Text.intercalate " " fieldParamNames
    nameAndParams = if null fieldParamNames then name
                    else name <> " " <> spaced
    encodeMe = case length fields of
      0 -> [ "buildRtsValue _b " <> name <> " = Prelude.return ()" ]
      _ -> ("buildRtsValue b " <> paren nameAndParams <> " = do")
            : indentLines (map ("Glean.buildRtsValue b " <>) fieldParamNames)
    decodeMe = case length fields of
      0 -> [ "decodeRtsValue = Prelude.pure " <> name]
      n -> "decodeRtsValue = " <> name
           : indent "<$> Glean.decodeRtsValue"
           : indentLines (replicate (pred n) "<*> Glean.decodeRtsValue")

  tys <- mapM makeTypeName fields
  let def_Type =
        "instance Glean.Type " <> name <> " where"
        : indentLines
        ( encodeMe
        <> decodeMe
        <> [sourceTypeDef ident ver]
        )

      def_RecordFields =
        [ emitFieldTypes "Angle.RecordFields" name (zip fields tys) ]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_RecordFields]
    Query -> []


unionDef :: Mode -> Name -> Version -> [ResolvedFieldDef] -> M [Text]
unionDef mode ident ver fields = do
  let typeRef = TypeRef ident ver
  uName@(here,root) <- typeName typeRef
  let name = haskellTypeName Data uName
  withTypeDefHint root $ do
  let
    shortConNames = map fieldDefName fields
    conNames = map toConName shortConNames
    toConName shortName = cap1 (prefix <> shortName)
      where prefix = name <> "_"
    makeTypeName pred mode gen (FieldDef p tField) =
      withUnionFieldHint p (haskellTy_ pred mode gen here tField)

  -- walk over the fields to generate nested types for the current mode first
  mapM_ (makeTypeName PredName mode True) fields

  conTypeNames <- mapM (makeTypeName PredName Data False) fields
  keyTypeNames <- mapM (makeTypeName PredKey Data False) fields

  let def_Type = case conNames of
        [] ->
          "instance Glean.Type " <> name <> " where" : indentLines
            [ "buildRtsValue _ _ = Prelude.return ()"
            , "decodeRtsValue = Prelude.error $ \"decodeRtsValue\" <> " <> name
            ]
        _ ->
          let emptyCon = toConName "EMPTY"
              builds =
                  mkBuildEmpty conNames emptyCon ++
                  concat (zipWith mkBuild [0..] conNames)
              decodes =
                "decodeRtsValue = Glean.sumD" :
                indentLines
                  ( "(Prelude.pure " <> emptyCon <> ")"
                  : asArray (map mkDecode conNames)
                  )
          in concat
            [ ["instance Glean.Type " <> name <> " where"]
            , indentLines builds
            , indentLines decodes
            , indentLines [sourceTypeDef ident ver]
            ]

      distinctTypes = length (nubSort conTypeNames) == length conTypeNames

      def_Sum
        | not distinctTypes = []
        | otherwise = zipWith mk conNames conTypeNames
        where
            inst conTypeName = "instance Glean.SumBranches "
              <> conTypeName <> " " <> name <> " where"
            makeInjectBranch conName = [ "injectBranch = " <> conName ]
            makeProjectBranch conName =
                ("projectBranch (" <> conName <> " x) = Prelude.Just x")
                : ["projectBranch _ = Prelude.Nothing"
                  | 1 < length conTypeNames ]
            mk conName conTypeName  =
              inst conTypeName
              : indentLines (makeInjectBranch conName
                  ++ makeProjectBranch conName)

      def_SumFields =
        [ emitFieldTypes "Angle.SumFields" name (zip fields keyTypeNames) ]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_SumFields] ++ def_Sum
    Query -> []
  where
    mkBuildEmpty constructors emptyCon =
      let index = length constructors in
      "buildRtsValue b " <> emptyCon <> "=" : indentLines
        [ "Glean.buildRtsSelector b " <> Text.pack (show index) ]

    mkBuild :: Int -> Text -> [Text]
    mkBuild i c =
      "buildRtsValue b (" <> c <> " x) = do" : indentLines
        [ "Glean.buildRtsSelector b " <> Text.pack (show i)
        , "Glean.buildRtsValue b x" ]

    mkDecode :: Text -> Text
    mkDecode c = "Glean.mapD " <> c

    asArray :: [Text] -> [Text]
    asArray = \case
      [] -> ["[]"]
      (x:xs) -> concat
        [ ["[ " <> x]
        , [", " <> e | e <- xs]
        , ["]"]
        ]

enumDef :: Mode -> Name -> Version -> [Name] -> M [Text]
enumDef mode ident ver eVals = do
  let typeRef = TypeRef ident ver
  eName@(_,root) <- typeName typeRef
  let name = haskellTypeName Data eName

  withTypeDefHint root $ do
  let
    def_Type = "instance Glean.Type " <> name <> " where"
            : indentLines [ "buildRtsValue = Glean.thriftEnum_buildRtsValue "
                          , "decodeRtsValue = Glean.thriftEnumD "
                          , sourceTypeDef ident ver
                          ]
    def_SumFields =
       [ emitFieldTypes "Angle.SumFields" name
           [ (FieldDef n unitT, "Glean.Schema.Builtin.Types.Unit")
           | n <- eVals ]
       ]

    def_AngleEnum =
      "instance Angle.AngleEnum " <> name <> " where": indentLines
        [ "type AngleEnumTy " <> name <> " = " <> name
        , "enumName v = Text.pack (Prelude.drop " <>
            showt (Text.length root + 1) <> " (Prelude.show v))"]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_SumFields, def_AngleEnum]
    Query -> []

sourceTypeDef :: Name -> Version -> Text
sourceTypeDef name version =
  "sourceType _ = Angle.NamedTy " <> paren sourceRef
  where
    sourceRef = Text.unwords
      [ "Angle.SourceRef"
      , Text.pack (show name)
      , paren ("Prelude.Just " <> showt version)
      ]


emitFieldTypes :: Text -> Text -> [(ResolvedFieldDef, Text)] -> Text
emitFieldTypes family name fields =
  "type instance " <> family <> " " <> name <> " = " <> go fields
  where
    go [] = "'Angle.TNoFields"
    go ((FieldDef name _, ty) : rest) =
       "'Angle.TField \"" <> name <> "\" (" <> ty <> ") (" <> go rest <> ")"

-- Copyright 2004-present Facebook. All Rights Reserved.
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-- | Generate "Glean.Schema"
module Glean.Schema.Gen.Haskell
  ( genSchemaHS
  ) where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Extra ( nubSort )
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath
import TextShow

import Glean.Schema.Gen.Utils
import Glean.Angle.Types


genSchemaHS :: Version -> [PredicateDef] -> [TypeDef] -> [(FilePath,Text)]
genSchemaHS _version preds allTypes =
  [ (Text.unpack (underscored namespaces) <> "_include" <.> "hs",
      Text.intercalate (newline <> newline)
        (header Data namespaces deps : doGen Data preds types))
  | (namespaces, (deps, preds, types)) <- schemas
  ] ++
  [ ("query" </> Text.unpack (underscored namespaces) <> "_include" <.> "hs",
      Text.intercalate (newline <> newline)
        (header Query namespaces deps : doGen Query preds types))
  | (namespaces, (deps, preds, types)) <- schemas
  ] ++
  [ ("Glean" </> "Schema" </>
      Text.unpack (Text.concat (map cap1 namespaces)) <.> "hs",
    genAllPredicates namespaces predIndices preds)
  | (namespaces, (_deps, preds, _types)) <- schemas
  ]
  where
    -- each predicate gets a unique Id (its "index") which is used to
    -- lookup the Pid quickly when writing facts. These indices must
    -- be globally unique across all schemas.
    predIndices = HashMap.fromList
      [ (predicateDefRef pred, ix)
      | (pred, ix) <- zip preds [0..]
      ]

    schemas = HashMap.toList $
      addNamespaceDependencies (sortDeclsByNamespace preds allTypes)

    namePolicy = mkNamePolicy preds allTypes

    doGen
      :: Mode
      -> [PredicateDef]
      -> [TypeDef]
      -> [Text]
    doGen mode preds types = concat gen ++ reverse extra
      where
      (gen :: [[Text]], extra :: [Text]) = runM mode [] namePolicy allTypes $ do
         ps <- forM preds $ \pred -> do
           let ix = fromJust $ HashMap.lookup (predicateDefRef pred) predIndices
           genPredicate mode ix pred
         ts <- mapM (genType mode) types
         return (ps ++ ts)

genAllPredicates
  :: NameSpaces
  -> HashMap PredicateRef Int
  -> [PredicateDef]
  -> Text
genAllPredicates namespace indices preds = Text.unlines $
  [ "-- @" <> "generated"
  , "module Glean.Schema." <>
    Text.concat (map cap1 namespace) <> " (allPredicates) where"
  , ""
  , "import Glean.Types"
  , ""
  , "allPredicates :: [(PredicateRef, Int)]"
  , "allPredicates ="
  ] ++
  indentLines (encsep "[ " ", " "]"
    [ "(PredicateRef \"" <> predicateRef_name ref <> "\" " <>
         showt (predicateRef_version ref) <> ", " <> showt ix <> ")"
    | pred <- preds
    , let ref = predicateDefRef pred
    , Just ix <- [HashMap.lookup ref indices]
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
  , ""
  ] ++
  -- import dependencies
  map (importSchema mode) deps ++
  -- if this is a Query module, import the Data module
  (case mode of
    Query -> [importSchema Data here]
    _ -> []) ++
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
  | Query <- mode = Text.unlines [data_, query]
  | otherwise = data_
  where
  data_ = "import qualified Glean.Schema." <> upperSquashNS <> ".Types"
  query = "import qualified Glean.Schema.Query." <> upperSquashNS <> ".Types"
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

qFieldName :: (NameSpaces, Text) -> Text
qFieldName (ns, x) = haskellThriftName Query (ns, low1 x)

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

die :: String -> String -> a
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

shareTypeDef :: Mode -> Bool -> NameSpaces -> Type -> M Text
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

haskellTy :: NameSpaces -> Type -> M Text
haskellTy = haskellTy_ PredId Data True

-- | how to render predicate types in haskellTy
data PredTy = PredName | PredId | PredKey

haskellTy_
  :: PredTy
  -> Mode
  -> Bool -- ^ generate nested type definitions
  -> NameSpaces
  -> Type
  -> M Text
haskellTy_ withId mode genSub here t = case t of
  -- Leafs
  Byte{} -> return "Glean.Byte"
  Nat{} -> return "Glean.Nat"
  Boolean{} -> return "Prelude.Bool"
  String{} -> return "Data.Text.Text"
  Array Byte -> return "Data.ByteString.ByteString"
  Array tInner -> do
    inner <- haskellTy_ withId mode genSub here tInner
    return $ "[" <> inner <> "]"
  Record{} -> shareTypeDef mode genSub here t
  Sum{} -> shareTypeDef mode genSub here t
  Maybe ty -> do
    inner <- haskellTy_ withId mode genSub here ty
    return (optionalize inner)
  -- References
  Predicate pred -> do
    let wrap = case withId of
          PredName -> id
          PredId -> ("Glean.IdOf" <>!)
          PredKey -> ("Glean.KeyType " <>)
    wrap . haskellTypeName mode <$> predicateName pred

  NamedType typeRef ->
    haskellTypeName mode <$> typeName typeRef
  Enumerated _ -> shareTypeDef mode genSub here t


genPredicate :: Mode -> Int -> PredicateDef -> M [Text]
genPredicate mode index PredicateDef{..}
  | provided (predicateRef_name predicateDefRef) = return []
  | otherwise = do
    pName <- predicateName predicateDefRef
    let
      here = fst pName
      name = haskellTypeName Data pName -- e.g. Clang_File
      queryName = haskellTypeName Query pName -- e.g. Clang_File

    -- Note: This withPredicateDefHint covers both Mode Data and Mode Query
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

    toQueryKey <- toQuery here predicateDefKeyType

    let extra = define_key ++ define_value
    let inst cls body =
          "instance " <> cls <> " " <> name <> " where"
          : indentLines body
        def_Predicate = inst "Glean.Predicate" $
          ["type KeyType " <> name =@ type_key]
          ++
          ["type ValueType " <> name =@ type_value | has_value]
          ++
          [ "getName _proxy " =@ "Glean.PredicateRef " <>
                Text.pack (show glean_name) <> -- adds quotes, does escaping
                showt (predicateRef_version predicateDefRef)
          , "getIndex _proxy " =@ Text.pack (show index)
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

        def_Query = inst "Glean.PredicateQuery"
          [ "toQueryId = " <> queryName <>
              "_with_id . Glean.fromFid . Glean.idOf"
          , "toQueryKey = " <> queryName <> "_with_key . " <> toQueryKey ]

        def_Type = inst "Glean.Type"
          [ "buildRtsValue b = Glean.buildRtsValue b . Glean.getId"
          , "decodeRtsValue = Glean.decodeRef" ]

        def_QueryResult_QueryOf =
          ["type instance Glean.QueryResult " <> queryName <> " = " <> name
          ,"type instance Glean.QueryOf " <> name <> " = " <> queryName ]

        def_ToQuery =
          [ "instance Glean.ToQuery " <> name ]

    return $ extra ++ case mode of
      Data -> map myUnlines [def_Predicate, def_Type]
      Query -> map myUnlines [def_Query, def_QueryResult_QueryOf, def_ToQuery]


-- Make the thriftTy type text, and the needed [Text] blocks
define_kt
  :: Mode
  -> NameSpaces
  -> Type
  -> (NameSpaces, Text)
  -> M (Text, [Text])
define_kt mode here typ name_kt = case typ of
  Byte{} -> leaf
  Nat{} -> leaf
  String{} -> leaf
  Record [] -> leaf
  Record _fields -> alias typ
  Array{} -> alias typ
  Sum [] -> leaf
  Sum _fields -> alias typ
  Maybe{} -> alias typ
  Predicate{} -> leaf
  NamedType{} -> alias typ
  _other -> die "define_kt" (show typ)
 where
   gname = joinDot name_kt

   leaf = (,) <$> return (haskellTypeName mode name_kt) <*> return []

   alias t = do
    ref <- haskellTy here (NamedType (TypeRef gname 0))
    def <- genType mode TypeDef
      { typeDefRef = TypeRef (joinDot name_kt) 0
      , typeDefType = t }
    return (ref,def)

genType :: Mode -> TypeDef -> M [Text]
genType mode TypeDef{typeDefRef = TypeRef{..}, ..}
  | provided typeRef_name = return []
  | otherwise =
  case typeDefType of
    Record fields -> structDef mode typeRef_name typeRef_version fields
    Sum fields -> unionDef mode typeRef_name typeRef_version fields
    Enumerated vals -> enumDef mode typeRef_name typeRef_version vals
    _ -> return []

structDef :: Mode -> Name -> Version -> [FieldDef] -> M [Text]
structDef mode sName ver fields = do
  let typeRef = TypeRef sName ver
  sName@(here,root) <- typeName typeRef
  let name = haskellTypeName Data sName
      queryName = haskellTypeName Query sName

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
  toQueries <- forM fields $ \field ->
    withRecordFieldHint (fieldDefName field) $
      toQuery here (fieldDefType field)
  let def_Type =
        "instance Glean.Type " <> name <> " where"
        : indentLines (encodeMe <> decodeMe)

      def_Query =
        ["type instance Glean.QueryResult " <> queryName <> " = " <> name
        ,"type instance Glean.QueryOf " <> name <> " = " <> queryName
        ]

      def_ToQuery =
        "instance Glean.ToQuery " <> name <> " where" : indentLines
          [ "toQuery " <> paren nameAndParams <> " = " <> queryName <>
            mconcat
              [ " (Prelude.Just (" <> toq <> " " <> x <> "))"
              | (toq, x) <- zip toQueries fieldParamNames ]
          ]

      def_RecordFields =
        [ emitFieldTypes "Angle.RecordFields" name (zip fields tys) ]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_RecordFields]
    Query -> [def_Query, def_ToQuery]

-- | We cannot generate ToQuery instances for arrays and maybes,
-- because they would overlap. e.g. a type [T] corresponds to a query
-- type Foo_array (where Foo is derived from the typedef or field
-- containing this type), so we would need a
--
--   type instance QueryOf [T] Foo_array
--
-- and this would overlap with other instance for [T]. So when we need
-- `toQuery` for an array or maybe type, we have to expand it
-- inline. Getting this right is super painful, but I don't know a
-- better solution.
--
toQuery :: NameSpaces -> Type -> M Text
toQuery here ty = go False here ty
  where
  go named here ty = case ty of
    Array elemTy | elemTy /= Byte -> do
      (_, name) <-
        (if named then id else withRecordFieldHint "array") $ nameThisType ty
      let qname = haskellTypeName Query (here, name)
      elemToQuery <- toQuery here elemTy
      return $ "(" <> qname <> "_exact . Prelude.map " <> elemToQuery <> ")"
    Maybe elemTy -> do
      (_, name) <- nameThisType ty
      let qname = qFieldName (here, name)
      elemToQuery <- withRecordFieldHint "just_" $ toQuery here elemTy
      return $ "(Prelude.maybe (Data.Default.def { " <>
        qname <> "_nothing = Prelude.Just Data.Default.def}) " <>
        "(\\x -> Data.Default.def { " <> qname <> "_just = Prelude.Just (" <>
        elemToQuery <> " x)}))"
    NamedType t -> do
      (here,root) <- typeName t
      withTopLevelHint root $ do
        m <- typeDef t
        case m of
          Nothing -> error "toQuery: arrayTyName"
          Just ty -> go True here ty
    _other -> return "Glean.toQuery"

-- | Help "SumQuery" when the branch type is Array of a named type.
-- We need to append @"_<fieldDefName>_array"@ to the main type.
--
-- Compare with Glean.Schema.Gen.Thrift.thriftTy for mode Query, which
-- add this @"_array"@.
--
-- I do not have a @Maybe@ case to test, so this may need extending
-- in the future.
--
-- First argument is the normal type renaming.
toQueryType :: (FieldDef -> M Text) -> NameSpaces -> FieldDef -> M Text
toQueryType fieldTypeName here field = case fieldDefType field of
  Array elemTy | elemTy /= Byte -> do
    (_, name) <- withUnionFieldHint (fieldDefName field) $
      withRecordFieldHint "array" $ nameThisType (fieldDefType field)
    let qname = haskellTypeName Query (here, name)
    return qname
  _other -> toQueryConTypeName <$> fieldTypeName field

-- | Helper for 'unionDef' because @instance SumQuery (QueryOf _)@ gives
-- @Illegal type synonym family application in instance@ so I will write
-- my own transform
toQueryConTypeName :: Text -> Text
toQueryConTypeName "Bool" = "Prelude.Bool"
toQueryConTypeName "Text" = "Data.Text.Text"
toQueryConTypeName "ByteString" = "Data.ByteString.ByteString"
toQueryConTypeName "Glean.Nat" = "Glean.Nat"
toQueryConTypeName "Glean.Byte" = "Glean.Byte"
toQueryConTypeName n = n


unionDef :: Mode -> Name -> Version -> [FieldDef] -> M [Text]
unionDef mode ident ver fields = do
  let typeRef = TypeRef ident ver
  uName@(here,root) <- typeName typeRef
  let name = haskellTypeName Data uName
      queryName = haskellTypeName Query uName
  withTypeDefHint root $ do
  let
    shortConNames = map fieldDefName fields
    conNames = map (cap1 . (prefix <>)) shortConNames
      where prefix = name <> "_"
    queryConNames = map (prefixQuery <>) shortConNames
      where nameQuery = qFieldName uName
            prefixQuery = cap1 nameQuery <> "_"
    makeTypeName pred mode gen (FieldDef p tField) =
      withUnionFieldHint p (haskellTy_ pred mode gen here tField)

  -- walk over the fields to generate nested types for the current mode first
  mapM_ (makeTypeName PredName mode True) fields

  conTypeNames <- mapM (makeTypeName PredName Data False) fields
  keyTypeNames <- mapM (makeTypeName PredKey Data False) fields
  queryConTypeNames <- mapM
    (toQueryType (makeTypeName PredName Query False) here)
    fields

  toQueries <- forM fields $ \field ->
    withUnionFieldHint (fieldDefName field) $
      toQuery here (fieldDefType field)

  let
    wrap :: Text -> Text
    wrap s = "(" <> s <> ")"
    conWithX c = wrap (c <> " x")
  let mkBuild (i::Int) c _t =
        "buildRtsValue b " <> conWithX c <> " = do"
        : indentLines [ "Glean.buildRtsSelector b " <> Text.pack (show i)
                      , "Glean.buildRtsValue b x" ]
      mkDecode c _t = "Glean.mapD " <> c -- so far all has one constructor
  let def_Type = case conNames of
        [] -> "instance Glean.Type " <> name <> " where"
              : indentLines [ "buildRtsValue _ _ = Prelude.return ()"
                 , "decodeRtsValue = Prelude.error $ \"decodeRtsValue\" <> " <>
                     name ]
        (c:cs) ->
          let builds = concat $ zipWith3 mkBuild [0..] conNames conTypeNames
              decodes = concat
                [ [ "decodeRtsValue = Glean.sumD" ]
                  , indentLines
                      ( "[ " <> mkDecode c (head conTypeNames)
                      : map (", " <>)
                          (zipWith mkDecode cs (tail conTypeNames))
                      )
                  , indentLines [ "]" ]
                ]
          in concat
            [ ["instance Glean.Type " <> name <> " where"]
            , indentLines builds
            , indentLines decodes
            ]

      def_Query =
        ["type instance Glean.QueryResult " <> queryName <> " = " <> name
        ,"type instance Glean.QueryOf " <> name <> " = " <> queryName
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

      def_SumQuery
        | not distinctTypes = []
        | otherwise = zipWith mk queryConNames queryConTypeNames
        where
            instQ queryConTypeName = "instance Glean.SumQuery "
              <> queryConTypeName <> " " <> queryName <> " where"
            makeInjectQuery queryConName = "injectQuery q = Data.Default.def"
              : indentLines [ "{ " <> queryConName <> " = Prelude.Just q }" ]
            mk queryConName queryConTypeName =
              instQ queryConTypeName
              : indentLines (makeInjectQuery queryConName)

      def_ToQuery =
          ("instance Glean.ToQuery " <> name <> " where") :
          [ "  toQuery (" <> con <> " x) = Data.Default.def { " <>
              queryCon <> " = Prelude.Just (" <> toQ <> " x) }"
          | (con, queryCon, toQ) <- zip3 conNames queryConNames toQueries ]

      def_SumFields =
        [ emitFieldTypes "Angle.SumFields" name (zip fields keyTypeNames) ]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_SumFields] ++ def_Sum
    Query -> [def_Query, def_ToQuery] ++ def_SumQuery

enumDef :: Mode -> Name -> Version -> [Name] -> M [Text]
enumDef mode ident ver eVals = do
  let typeRef = TypeRef ident ver
  eName@(_,root) <- typeName typeRef
  let name = haskellTypeName Data eName
      queryName = haskellTypeName Query eName

      conNames = map (cap1 . ((name <> "_") <>)) eVals
      queryConNames = map (cap1 . ((queryName <> "_") <>)) eVals

  withTypeDefHint root $ do
  let
    def_Type = "instance Glean.Type " <> name <> " where"
            : indentLines [ "buildRtsValue = Glean.thriftEnum_buildRtsValue "
                          , "decodeRtsValue = Glean.thriftEnumD "
                          ]
    def_Query =
      ["type instance Glean.QueryResult " <> queryName <> " = " <> name
      ,"type instance Glean.QueryOf " <> name <> " = " <> queryName
      ]

    def_ToQuery =
      "instance Glean.ToQuery " <> name <> " where" : indentLines
        [ "toQuery " <> con <> " = " <> qcon
        | (con, qcon) <- zip conNames queryConNames
        ]

    def_SumFields =
       [ emitFieldTypes "Angle.SumFields" name
           [ (FieldDef n unitT, "Glean.Schema.Builtin.Types.Unit")
           | n <- eVals ]
       ]

  return $ map myUnlines $ case mode of
    Data -> [def_Type, def_SumFields]
    Query -> [def_Query, def_ToQuery]

emitFieldTypes :: Text -> Text -> [(FieldDef, Text)] -> Text
emitFieldTypes family name fields =
  "type instance " <> family <> " " <> name <> " = " <> go fields
  where
    go [] = "'Angle.TNoFields"
    go ((FieldDef name _, ty) : rest) =
       "'Angle.TField \"" <> name <> "\" (" <> ty <> ") (" <> go rest <> ")"

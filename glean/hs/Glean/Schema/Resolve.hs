{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}

module Glean.Schema.Resolve
  ( resolveSchema
  , parseAndResolveSchema
  , Schemas(..)
  , resolveType
  , lookupResultToExcept
  , resolveEvolves
  ) where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char
import Data.Graph
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding (group)
import TextShow

import Glean.Angle.Parser
import Glean.Angle.Types
import Glean.Schema.Types
import Glean.Schema.Util

-- | A set of schemas
data Schemas = Schemas
  { schemasHighestVersion :: Maybe Version
  , schemasResolved :: [ResolvedSchemaRef]
    -- ^ Resolved schemas in dependency order
  }

-- | Useful packaging of 'parseSchema' and 'resolveSchema'. Note that
-- parsing and resolution of a schema is a pure function.
parseAndResolveSchema :: ByteString -> Either String (SourceSchemas, Schemas)
parseAndResolveSchema str =
  case parseSchema str of
    Left str -> Left str
    Right ss -> case resolveSchema ss of
      Left txt -> Left (Text.unpack txt)
      Right r -> Right (ss, r)

--
-- | Turn 'SourceSchemas' into a 'Schemas' by resolving all the
-- references and checking for validity.
--
resolveSchema :: SourceSchemas -> Either Text Schemas
resolveSchema SourceSchemas{..} = runExcept $ do
  let
    -- dependency analysis: we want to process schemas in dependency
    -- order, and detect cycles in evolves declarations.
    sccs = stronglyConnComp $
      edges (\s -> schemaDependencies s <> schemaEvolves s)

    schemaDependencies SourceSchema{..} =
      schemaInherits ++ [ name | SourceImport name <- schemaDecls ]

    schemaEvolves SourceSchema{..} =
      HashMap.lookupDefault [] schemaName evolves

    edges outNames =
      [ (schema, schemaName schema, outNames schema)
      | schema <- srcSchemas ]

    evolves = HashMap.fromListWith (++)
      [ (new, [old]) | SourceEvolves _ new old <- srcEvolves ]

    resolveSchemas env [] = return env
    resolveSchemas env (AcyclicSCC one : rest) = do
      let schemaEvolves = HashMap.lookupDefault [] (schemaName one) evolves
      resolved <- resolveOneSchema env srcAngleVersion schemaEvolves one
      resolveSchemas (HashMap.insert (schemaName one) resolved env) rest
    resolveSchemas _ (CyclicSCC some : _) = throwError $
      "cycle in schema definitions between: " <>
        Text.intercalate ", " (map schemaName some)

  -- Resolve all the references in each individual schema
  finalEnv <- resolveSchemas HashMap.empty sccs

  let
    resolved =
        [ schema
        | AcyclicSCC one <- sccs
        , Just schema <- [HashMap.lookup (schemaName one) finalEnv ] ]

    allSchemas =
      [ schema
      | schema@ResolvedSchema{..} <- HashMap.elems finalEnv
      , resolvedSchemaName == "all"
      ]

  -- Check whether any schema is evolved by multiple schemas.
  _ <- either throwError return $ resolveEvolves resolved

  return Schemas
    { schemasHighestVersion =
        if null allSchemas
           then Nothing
           else Just (maximum $ map resolvedSchemaVersion allSchemas)
    , schemasResolved = resolved
    }


-- | Create a mapping from a schema to the schema that evolves it. This will
--  - check if any schema is evolved by multiple schemas.
--  - check if all schema evolutions are legal.
--
-- If 'B evolves A' and 'C evolves A', when a query for A comes we won't
-- know whether to serve facts from C or from B. Therefore we disallow
-- multiple schemas to evolve a single one.
resolveEvolves
  :: [ResolvedSchemaRef] -- ^ in dependency order
  -> Either Text (HashMap SchemaRef SchemaRef)
resolveEvolves resolved = do
  checkLawfulEvolves
  HashMap.traverseWithKey checkMultipleEvolves
    $ HashMap.fromListWith (++)
    $ concatMap evolvedByResolved resolved
  where
    evolvedByResolved new =
        [ (oldRef, [schemaRef new])
        | oldRef <- Set.toList $ resolvedSchemaEvolves new ]

    checkMultipleEvolves old newList =
      case nub newList of
        [new] -> Right new
        _     -> Left $ "multiple schemas evolve "
          <> showSchemaRef old
          <> ": "
          <> Text.unwords (map showSchemaRef newList)

    resolvedByRef :: Map SchemaRef ResolvedSchemaRef
    resolvedByRef = Map.fromList [(schemaRef s, s) | s <- resolved ]


    -- Later definitions override earlier ones in case of db overrides
    types = HashMap.unions $ reverse $ map resolvedSchemaTypes resolved

    checkLawfulEvolves =
      foldM_ (evolveOneSchema types) mempty
        [ (new, old)
        | new <- resolved
        , oldRef <- Set.toList $ resolvedSchemaEvolves new
        , Just old <- [Map.lookup oldRef resolvedByRef]
        ]


-- Check for back compatibility and map each predicate to their evolved
-- counterpart in the the evolvedBy map
evolveOneSchema
  :: HashMap TypeRef ResolvedTypeDef  -- ^ all type definitions
  -> HashMap PredicateRef PredicateRef -- ^ all predicate evolutions till now.
                                       -- value evolves key
  -> (ResolvedSchemaRef, ResolvedSchemaRef)
  -> Either Text (HashMap PredicateRef PredicateRef)
evolveOneSchema types evolvedBy (new, old) = do
  checkBackCompatibility
  return evolvedBy'
  where
    checkBackCompatibility :: Either Text ()
    checkBackCompatibility =
      forM_ (resolvedSchemaPredicates old) $ \oldDef -> do
        newDef <- matchNew oldDef
        evolveDef newDef oldDef

    matchNew oldDef =
      let name = predicateRef_name $ predicateDefRef oldDef
      in case HashMap.lookup name newPredsByName of
        Just newDef -> return newDef
        Nothing ->
          throwError $ "missing evolved predicate "
          <> name <> " from " <> showSchemaRef (schemaRef old)

    evolveDef
      (PredicateDef ref key val _)
      (PredicateDef _ oldkey oldval _) =
      let
          keyErr = key `canEvolve'` oldkey
          valErr = val `canEvolve'` oldval
      in case keyErr <|> valErr of
        Nothing -> return ()
        Just err -> throwError $
          "cannot evolve predicate " <> predicateRef_name ref <> ": " <> err

    canEvolve' :: ResolvedType -> ResolvedType -> Maybe Text
    canEvolve' = canEvolve types evolvedBy'

    -- add evolutions from current schema
    evolvedBy' :: HashMap PredicateRef PredicateRef
    evolvedBy' = foldr addEvolution evolvedBy oldPreds
      where
        oldPreds = HashMap.keys (exportedPredicates old)
        addEvolution oldPred acc =
          case HashMap.lookup (predicateRef_name oldPred) newPredsByName of
            Nothing -> acc
            Just newPred -> HashMap.insert oldPred (predicateDefRef newPred) acc

    newPredsByName :: HashMap Name ResolvedPredicateDef
    newPredsByName = mapKeys predicateRef_name (exportedPredicates new)

    mapKeys f = HashMap.fromList . map (first f) . HashMap.toList

    exportedPredicates ResolvedSchema{..} =
      resolvedSchemaPredicates <> resolvedSchemaReExportedPredicates

type Environment = HashMap Name ResolvedSchemaRef

resolveOneSchema
  :: Environment
  -> AngleVersion
  -> [Name]
  -> SourceSchema
  -> Except Text ResolvedSchemaRef

resolveOneSchema env angleVersion evolves SourceSchema{..} =
  flip catchError (\e -> throwError $ "In " <> schemaName <> ":\n  " <> e) $ do
  let
    SourceRef namespace maybeVer = parseRef schemaName

    schemaByName name = case HashMap.lookup name env of
      Nothing -> throwError $ "unknown schema: " <> name
      Just schema -> return schema

  checkNameSpace namespace

  -- Version of this schema
  version <- case maybeVer of
    Nothing -> throwError $ "missing version: " <> schemaName
    Just v -> return v

  -- All the schemas we're inheriting from
  inherits <- traverse schemaByName schemaInherits

  -- All the schemas we imported
  imports <- traverse schemaByName [ name | SourceImport name <- schemaDecls ]

  let
    qualify :: Name -> Name
    qualify x = namespace <> "." <> x

    unqualify :: Name -> Name
    unqualify = snd . splitDot

    localPreds =
      [ let
          SourceRef name explicitVersion = predicateDefRef p
          thisVersion = fromMaybe version explicitVersion
          qname = qualify name
        in
          (name, PredicateRef qname thisVersion, p)
      | SourcePredicate p <- schemaDecls
      ]

    localTypes =
      [ let
          SourceRef name explicitVersion = typeDefRef p
          thisVersion = fromMaybe version explicitVersion
          qname = qualify name
        in
          (name, TypeRef qname thisVersion, p)
      | SourceType p <- schemaDecls
      ]

  -- Check for multiple definitions of the same name/version.
  -- Multiple definitions of the same name is OK: an unqualified
  -- reference will be rejected as ambiguous, but can be resolved by
  -- using an explicit version.
  let
    numRefs = HashMap.fromListWith (+) $
      [ ((predicateRef_name, predicateRef_version), 1::Int)
      | (_, PredicateRef{..}, _) <- localPreds ] ++
      [ ((typeRef_name, typeRef_version), 1)
      | (_, TypeRef{..}, _) <- localTypes]
  forM_ (HashMap.toList numRefs) $ \((name,ver), num) -> do
    when (num > 1) $ throwError $
      "multiple definitions for: " <> name <> "." <> showt ver

  -- Build the scope: a mapping from unversioned names to RefTarget
  let
    -- inherited definitions are in scope unqualified and qualified
    --   (but unqualified local names override unqualified inherited names)
    qualInheritedScope :: NameEnv RefResolved
    qualInheritedScope = HashMap.fromListWith Set.union
      [ entry
      | schema <- inherits
      , (name, ver, target) <- allRefs schema
      , entry <- [
          (SourceRef name Nothing, Set.singleton target),
          (SourceRef name (Just ver), Set.singleton target) ]
      ]

    unqualInheritedScope :: NameEnv RefResolved
    unqualInheritedScope = HashMap.fromListWith Set.union
      [ entry
      | schema <- inherits
      , (name, ver, target) <- allRefs schema
      , entry <- [
          (SourceRef (unqualify name) Nothing, Set.singleton target),
          (SourceRef (unqualify name) (Just ver), Set.singleton target) ]
      ]

    -- imported names are in scope qualified.
    importedScope :: NameEnv RefResolved
    importedScope = HashMap.fromListWith Set.union
        [ entry
        | schema <- imports
        , (name, ver, target) <- allRefs schema
        , entry <- [
            (SourceRef name Nothing, Set.singleton target),
            (SourceRef name (Just ver), Set.singleton target) ]
        ]

    allRefs :: ResolvedSchemaRef -> [(Name, Version, RefResolved)]
    allRefs ResolvedSchema{..} =
      [ (typeRef_name ref, typeRef_version ref, RefType (typeDefRef def))
      | (ref, def) <- HashMap.toList $
          resolvedSchemaTypes <> resolvedSchemaReExportedTypes ] ++
      [ (predicateRef_name ref, predicateRef_version ref,
          RefPred (predicateDefRef def))
      | (ref, def) <- HashMap.toList $
          resolvedSchemaPredicates <> resolvedSchemaReExportedPredicates ]

    -- local definitions are in scope unqualified and qualified
    unqualLocalScope :: NameEnv RefResolved
    unqualLocalScope = HashMap.fromListWith Set.union unqualLocalEntities

    qualLocalScope :: NameEnv RefResolved
    qualLocalScope = HashMap.fromListWith Set.union
      [ (SourceRef (qualify name) ver, target)
      | (SourceRef name ver, target) <- unqualLocalEntities ]

    unqualLocalEntities =
      [ entry
      | (name, r, _) <- localPreds
      , let target = Set.singleton (RefPred r)
      , entry <- [
          (SourceRef name Nothing, target),
          (SourceRef name (Just (predicateRef_version r)), target) ]
      ] ++
      [ entry
      | (name, r, _) <- localTypes
      , let target = Set.singleton (RefType r)
      , entry <- [
          (SourceRef name Nothing, target),
          (SourceRef name (Just (typeRef_version r)), target) ]
      ]

    scope =
      HashMap.union unqualLocalScope $
      HashMap.union unqualInheritedScope $
      HashMap.unionWith Set.union qualLocalScope $
      HashMap.unionWith Set.union qualInheritedScope
      importedScope

  -- Check for inheriting multiple versions of a predicate/types
  --
  -- As a special case, if the schema is called "all" then allow name
  -- clashes in the inheritance set. This is to facilitate our "all.N"
  -- convention for declaring the "top-level" schema which will
  -- inevitably have name clashes, but we don't care about what it
  -- exports.
  unless (namespace == "all") $
    forM_ (HashMap.toList unqualInheritedScope) $ \(ref, targets) -> do
      case Set.elems targets of
        (_:_:_) -> throwError $
          "inherited schemas give multiple definitions for: " <> showRef ref
        _ -> return ()

  -- resolve type definitions
  types <- forM localTypes $
    \(name, ref, TypeDef{..}) -> do
      checkName name
      type' <- resolveType angleVersion scope typeDefType
      return (name, TypeDef
        { typeDefRef = ref
        , typeDefType = type' })

  -- resolve predicate definitions
  predicates <- forM localPreds $
    \(name, ref, PredicateDef{..}) -> do
      checkName name
      key <- resolveType angleVersion scope predicateDefKeyType
      value <- resolveType angleVersion scope predicateDefValueType
      return (name, PredicateDef
        { predicateDefRef = ref
        , predicateDefKeyType = key
        , predicateDefValueType = value
        , predicateDefDeriving = NoDeriving })

  let
    -- scope of predicates that we can specify queries for. Namely
    -- locally-defined predicates and inherited predicates.
    predScope = resolveRef $
      HashMap.union unqualLocalScope $
      HashMap.union unqualInheritedScope $
      HashMap.unionWith Set.union qualLocalScope
      qualInheritedScope

  -- resolve queries
  localDeriving <- forM [ d | d@SourceDeriving{} <- schemaDecls ] $
    \(SourceDeriving ref derive) -> do
      ty <- lookupResultToExcept ref $ predScope ref
      ref <- case ty of
        RefPred ref -> return ref
        RefType ref -> throwError $
          "cannot define a query for a typedef: " <> showRef ref
      return (ref, derive)

  let
    localTypeNames = HashSet.fromList $ map fst types

    localTypes = HashMap.fromList
      [ (typeDefRef def, def) | (_, def) <- types ]

    -- The types we re-export from this schema are all the types that
    -- are inherited but not shadowed by a locally-defined type.
    reExportedTypes =
      HashMap.fromList
        [ (typeDefRef def, def)
        | schema <- inherits
        , (ref, def) <- HashMap.toList $
            resolvedSchemaTypes schema <>
            resolvedSchemaReExportedTypes schema
        , let name =  unqualify (typeRef_name ref)
        , not (name `HashSet.member` localTypeNames)
        ]

    localPredicateNames = HashSet.fromList $ map fst predicates

    localPredicates = HashMap.fromList
      [ (predicateDefRef def, def) | (_, def) <- predicates ]

    -- Similarly for predicates.
    reExportedPredicates =
      HashMap.fromList
        [ (predicateDefRef def, def)
        | schema <- inherits
        , (ref, def) <- HashMap.toList $
            resolvedSchemaPredicates schema <>
            resolvedSchemaReExportedPredicates schema
        , let name =  unqualify (predicateRef_name ref)
        , not (name `HashSet.member` localPredicateNames)
        ]

  schemaEvolves <- traverse schemaByName evolves

  return ResolvedSchema
    { resolvedSchemaName = namespace
    , resolvedSchemaVersion = version
    , resolvedSchemaAngleVersion = angleVersion
    , resolvedSchemaTypes = localTypes
    , resolvedSchemaReExportedTypes = reExportedTypes
    , resolvedSchemaPredicates = localPredicates
    , resolvedSchemaReExportedPredicates = reExportedPredicates
    , resolvedSchemaScope = scope
    , resolvedSchemaDeriving = HashMap.fromList localDeriving
    , resolvedSchemaEvolves = Set.fromList (schemaRef <$> schemaEvolves)
    }

data Opt = Option | FieldOpt

-- | Check if a type is backward and forward compatible.
--
-- For backward compatibility to work if predicate A depends on predicate
-- B, evolved A must depend on evolved B.  That is, the following diagram must
-- commute
--
--     A --------> evolved(A)
--     |           |
--     |           | depends-on
--     ∨           ∨
--     B --------> evolved(B)
--      evolved-by
--
canEvolve
  :: HashMap TypeRef ResolvedTypeDef -- ^ type definitions
  -> HashMap PredicateRef PredicateRef -- ^ current evolutions map
  -> ResolvedType                    -- ^ updated type
  -> ResolvedType                    -- ^ old type
  -> Maybe Text              -- ^ compatibility error
canEvolve types evolvedBy new old = go new old
  where
    get ty = case HashMap.lookup ty types of
      Just v -> typeDefType v
      Nothing -> error $ "unknown type " <> show ty

    go (NamedTy new) (NamedTy old)
      | new == old = Nothing
      | otherwise = go (get new) (get old)
    go (NamedTy t) old = go (get t) old
    go new (NamedTy t) = go new (get t)
    go (MaybeTy new) (MaybeTy old) = go new old
    go ByteTy ByteTy = Nothing
    go NatTy NatTy = Nothing
    go StringTy StringTy = Nothing
    go BooleanTy BooleanTy = Nothing
    go (ArrayTy new) (ArrayTy old) = go new old
    go (PredicateTy new) (PredicateTy old)
      | evolved new /= evolved old = Just
          $ "type changed from " <> showRef old
          <> " to " <> showRef new
      | otherwise = Nothing
    go (EnumeratedTy new) (EnumeratedTy old) =
      compareFieldList Option new' old'
      where
        new' = map unitOpt new
        old' = map unitOpt old
        unitOpt name = FieldDef name (RecordTy [])
    go (SumTy new) (SumTy old) = compareFieldList Option new old
    go (RecordTy new) (RecordTy old) = compareFieldList FieldOpt new old
    go _ _ = Just "type changed"

    -- get most evolved version of a predicate
    evolved p = case HashMap.lookup p evolvedBy of
      Nothing -> p
      Just p' -> if p' == p then p else evolved p'

    compareFieldList optName new old =
      removedFieldsError <|> newRequiredFieldsError <|>
      asum (map compareField matchingFields)
      where
        names = map fieldDefName
        oldByName = Map.fromList (zip (names old) old)
        newByName = Map.fromList (zip (names new) new)
        addedFields = Map.difference newByName oldByName
        removedFields = Map.difference oldByName newByName
        matchingFields =
          [ (name, fNew, fOld)
          | FieldDef name fNew <- new
          , Just (FieldDef _ fOld) <- [Map.lookup name oldByName]
          ]
        compareField (name, new, old) = addLocation <$> go new old
          where
            addLocation err =
              "in " <> showOpt optName <> " '" <> name <> "', " <> err

        newRequiredFields = Map.keys $
          Map.filter (not . hasDefaultValue . fieldDefType) addedFields

        hasDefaultValue ty = case ty of
          MaybeTy _ -> True
          _ -> False

        removedFieldsError = case Map.keys removedFields of
          [] -> Nothing
          fields -> Just $ plural optName fields <>
            " missing: " <> Text.unwords fields

        newRequiredFieldsError = case optName of
          Option -> Nothing
          FieldOpt -> case newRequiredFields of
            [] -> Nothing
            _ -> Just $ Text.unlines
              [ Text.unwords [ "required" , plural optName newRequiredFields
                , "added:" , Text.unwords newRequiredFields ]
              , "For backward and forward compatibility, predicate evolutions"
                <> " require that all new fields be optional"
              ]


    plural s [_] = showOpt s
    plural s _ = showOpt s <> "s"
    showOpt Option = "option"
    showOpt FieldOpt = "field"

resolveType
  :: (ShowRef t, ShowRef p)
  => AngleVersion
  -> NameEnv (RefTarget p t)
  -> SourceType
  -> Except Text (Type_ p t)
resolveType ver scope typ = go typ
  where
  go typ = case typ of
    ByteTy -> return ByteTy
    NatTy -> return NatTy
    StringTy -> return StringTy
    ArrayTy ty -> ArrayTy <$> go ty
    RecordTy fields -> do checkFields fields; RecordTy <$> mapM goField fields
    SumTy fields -> do checkFields fields; SumTy <$> mapM goField fields
    PredicateTy ref -> goRef ref
    NamedTy ref -> goRef ref  -- shouldn't happen, but handle it anyway
    MaybeTy ty -> MaybeTy <$> go ty
    EnumeratedTy names -> do mapM_ checkName names; return (EnumeratedTy names)
    BooleanTy -> return BooleanTy

  goRef ref = do
    target <- lookupResultToExcept ref $ resolveRef scope ref
    case target of
      RefType ref -> return (NamedTy ref)
      RefPred ref -> return (PredicateTy ref)

  checkFields fields = do
    sequence_
      [ throwError $ "duplicate field: " <> x
        | x:_:_ <- group $ sort $ map fieldDefName fields ]
    when (checkReservedWordsInFieldNames ver) $
      mapM_ (checkName . fieldDefName) fields
    when (caseRestriction ver) $
      mapM_ (checkFieldName . fieldDefName) fields

  goField (FieldDef name ty) = FieldDef name <$> go ty


lookupResultToExcept
  :: (ShowRef t, ShowRef p)
  => SourceRef
  -> LookupResult (RefTarget p t)
  -> Except Text (RefTarget p t)
lookupResultToExcept ref res =
  either throwError return (lookupResultToEither ref res)


checkFieldName :: Name -> Except Text ()
checkFieldName n =
  when (isUpper (Text.head n)) $
    throwError $ "field names must begin with a lowercase letter: " <> n

checkName :: Name -> Except Text ()
checkName n = when (n `HashSet.member` reservedWords) $
  throwError $ n <> " is a reserved word, it cannot be used"

checkNameSpace :: Name -> Except Text ()
checkNameSpace n = mapM_ checkName (splitDot n)

reservedWords :: HashSet Text
reservedWords = HashSet.fromList [
    -- Thrift reserved words, seems like a good idea to avoid these
    "binary", "bool", "byte", "const", "cpp_include",
    "double", "enum", "exception", "extends", "false", "float",
    "hash_map", "hash_set", "hs_include", "i16", "i32", "i64", "include",
    "list", "map", "namespace", "oneway", "optional", "required", "senum",
    "service", "set", "stream", "string", "struct", "throws", "true",
    "typedef", "union", "view", "void",

    -- keywords that the fbthrift compiler rejects (see
    -- thrift/compiler/parse/thrift.ll)
    "abstract",
    "and",
    "args",
    "as",
    "assert",
    "auto",
    "break",
    "case",
    "char",
    "class",
    "continue",
    "declare",
    "def",
    "default",
    "del",
    "do",
    "elif",
    "else",
    "elseif",
    "except",
    "exec",
    "extern",
    "finally",
    "for",
    "foreach",
    "function",
    "global",
    "goto",
    "if",
    "implements",
    "import",
    "in",
    "int",
    "inline",
    "instanceof",
    "interface",
    "is",
    "lambda",
    "long",
    "native",
    "new",
    "not",
    "or",
    "pass",
    "public",
    "print",
    "private",
    "protected",
    "raise",
    "register",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "switch",
    "synchronized",
    "template",
    "this",
    "throw",
    "transient",
    "try",
    "unsigned",
    "var",
    "virtual",
    "volatile",
    "while",
    "with",
    "yield",
    "Object",
    "Client",
    "IFace",
    "Processor",

    -- Other words to avoid, because they lead to Thrift-generated code that
    -- does not compile for one or more languages.
    "None", "operator",
    -- Reserved for temporary predicates
    "_tmp_"
  ]


instance Pretty Schemas where
  pretty Schemas{} = mempty -- TODO

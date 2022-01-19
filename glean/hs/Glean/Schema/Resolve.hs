{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}

module Glean.Schema.Resolve
  ( resolveSchema
  , Schema(..)
  , Schemas(..)
  , ResolvedSchema(..)
  , parseAndResolveSchema
  , Scope
  , RefTarget(..)
  , resolveRef
  , resolveType
  , LookupResult(..)
  , lookupResultToExcept
  , SchemaRef(..)
  , schemaRef
  , resolveEvolves
  ) where

import Control.Monad.Except
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char
import Data.Graph
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding (group)
import Data.Tree
import GHC.Generics
import TextShow

import Glean.Angle.Parser
import Glean.Angle.Types
import Glean.Schema.Util

-- | A set of schemas
data Schemas = Schemas
  { schemasSchemas :: HashMap Version Schema
    -- ^ These correspond to the schemas named all.<version>. Later we
    -- will move towards using named schemas exclusively rather than
    -- these monolithic schemas.
  , schemasCurrentVersion :: Maybe Version
  , schemasResolved :: [ResolvedSchema]
    -- ^ Resolved schemas in dependency order
  }

-- | A single schema
data Schema = Schema
  { schemaVersion :: Version
  , schemaTypes :: HashMap Name (Set Version)
  , schemaPredicates :: HashMap Name (Set Version)
  }
  deriving Eq

-- | Identify a schema
data SchemaRef = SchemaRef Name Version
  deriving (Eq, Ord, Show, Generic)

instance Hashable SchemaRef

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
    -- dependency analysis: we want to process schemas in dependency order
    sccs = stronglyConnComp edges

    edges =
      [ (schema, schemaName schema, outNames schema)
      | schema <- srcSchemas ]
      where
      outNames SourceSchema{..} =
        schemaInherits
        ++ [ name | SourceImport name <- schemaDecls ]
        ++ HashMap.lookupDefault [] schemaName evolves

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

  -- Pick out the schemas called "all.<version>", find all their
  -- dependencies, and construct our top level schemas from these.
  -- Eventually when we move to using named schemas more consistently,
  -- this set of hacks can go away.
  let
    -- We also need to do a DFS for each all.<version> schema so that
    -- we can find the transitive closure of types and predicates that
    -- it refers to. Sadly this means building the graph again because
    -- stronglyConnComp doesn't let us get the graph it built.
    (graph, fromVertex, toVertex) = graphFromEdges edges

    resolvedAllSchemas =
      [ (n, schema)
      | (n, schema) <- HashMap.toList finalEnv
      , resolvedSchemaName schema == "all" ]

  allTheSchemas <-
    forM resolvedAllSchemas $ \(name,r) -> do
      v <- case toVertex name of
        Just v -> return v
        _ -> throwError "internal error: resolveSchema"
      let
        reachable = map fromVertex $ concat $ map flatten $ dfs graph [v]
        deps =
          [ r | (_,n,_) <- reachable, Just r <- [HashMap.lookup n finalEnv] ]
        types = HashMap.unions (map resolvedSchemaTypes deps)
        preds = HashMap.unions (map resolvedSchemaPredicates deps)
      return Schema
        { schemaVersion = resolvedSchemaVersion r
        , schemaTypes = HashMap.fromListWith Set.union
            [ (name, Set.singleton version)
            | TypeRef name version <- HashMap.keys types
            ]
        , schemaPredicates = HashMap.fromListWith Set.union
            [ (name, Set.singleton version)
            | PredicateRef name version <- HashMap.keys preds
            ]
        }

  let
    resolved =
        [ schema
        | AcyclicSCC one <- sccs
        , Just schema <- [HashMap.lookup (schemaName one) finalEnv ] ]

  -- Check whether any schema is evolved by multiple schemas.
  _ <- either throwError return $ resolveEvolves resolved

  return Schemas
    { schemasSchemas = HashMap.fromList
        [ (schemaVersion s, s) | s <- allTheSchemas ]
    , schemasCurrentVersion =
        if null allTheSchemas
           then Nothing
           else Just (maximum $ map schemaVersion allTheSchemas)
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
  :: [ResolvedSchema] -- ^ in dependency order
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

    resolvedByRef :: Map SchemaRef ResolvedSchema
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

showSchemaRef :: SchemaRef -> Text
showSchemaRef (SchemaRef name version) =
  name <> "." <> Text.pack (show version)

-- Check for back compatibility and map each predicate to their evolved
-- counterpart in the the evolvedBy map
evolveOneSchema
  :: HashMap TypeRef TypeDef           -- ^ all type definitions
  -> HashMap PredicateRef PredicateRef -- ^ all predicate evolutions till now.
                                       -- value evolves key
  -> (ResolvedSchema, ResolvedSchema)
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
          keyErr = key `canEvolve` oldkey
          valErr = val `canEvolve` oldval
      in case keyErr <|> valErr of
        Nothing -> return ()
        Just err -> throwError $
          "cannot evolve predicate " <> predicateRef_name ref <> ": " <> err

    canEvolve :: Type -> Type -> Maybe Text
    canEvolve = backCompatible types evolvedBy'

    -- add evolutions from current schema
    evolvedBy' :: HashMap PredicateRef PredicateRef
    evolvedBy' = foldr addEvolution evolvedBy oldPreds
      where
        oldPreds = HashMap.keys (exportedPredicates old)
        addEvolution oldPred acc =
          case HashMap.lookup (predicateRef_name oldPred) newPredsByName of
            Nothing -> acc
            Just newPred -> HashMap.insert oldPred (predicateDefRef newPred) acc

    newPredsByName :: HashMap Name PredicateDef
    newPredsByName = mapKeys predicateRef_name (exportedPredicates new)

    mapKeys f = HashMap.fromList . map (first f) . HashMap.toList

    exportedPredicates ResolvedSchema{..} =
      resolvedSchemaPredicates <> resolvedSchemaReExportedPredicates

schemaRef :: ResolvedSchema -> SchemaRef
schemaRef ResolvedSchema{..} =
  SchemaRef resolvedSchemaName resolvedSchemaVersion

type Environment = HashMap Name ResolvedSchema

data ResolvedSchema = ResolvedSchema
  { resolvedSchemaName :: Name
  , resolvedSchemaVersion :: Version
  , resolvedSchemaAngleVersion :: AngleVersion
  , resolvedSchemaTypes :: HashMap TypeRef TypeDef
    -- ^ types that are defined by this schema
  , resolvedSchemaReExportedTypes :: HashMap TypeRef TypeDef
    -- ^ types that are inherited and re-exported by this schema
  , resolvedSchemaPredicates :: HashMap PredicateRef PredicateDef
    -- ^ predicates that are defined by this schema
  , resolvedSchemaReExportedPredicates :: HashMap PredicateRef PredicateDef
    -- ^ predicates that are inherited and re-exported by this schema
  , resolvedSchemaScope :: Scope
    -- ^ we save the scope here because it will be used for typechecking
    -- the DerivingInfo later.
  , resolvedSchemaDeriving :: HashMap PredicateRef SourceDerivingInfo
    -- ^ deriving declarations, for predicates defined in this schema
    -- or an inherited schema.
  , resolvedSchemaEvolves :: Set SchemaRef
  }

data RefTarget = RefType TypeRef | RefPred PredicateRef
  deriving (Eq,Ord,Show)

showTarget :: RefTarget -> Text
showTarget (RefType (TypeRef name version)) =
  name <> "." <> showt version
showTarget (RefPred (PredicateRef name version)) =
  name <> "." <> showt version

type Scope = SourceRef -> LookupResult

resolveOneSchema
  :: Environment
  -> AngleVersion
  -> [Name]
  -> SourceSchema
  -> Except Text ResolvedSchema

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

  localPreds <- forM [ p | SourcePredicate p <- schemaDecls ] $
    \def@PredicateDef{..} -> case predicateDefRef of
      SourceRef name Nothing -> do
        let ref = PredicateRef (namespace <> "." <> name) version
        return (name,  ref, def)
      SourceRef name (Just explicitVersion) -> do
        let ref = PredicateRef (namespace <> "." <> name) explicitVersion
        return (name, ref, def)

  localTypes <- forM [ p | SourceType p <- schemaDecls ] $
    \def@TypeDef{..} ->
      case typeDefRef of
        SourceRef name Nothing -> do
          let ref = TypeRef (namespace <> "." <> name) version
          return (name, ref, def)
        SourceRef name (Just explicitVersion) -> do
          let ref = TypeRef (namespace <> "." <> name) explicitVersion
          return (name, ref, def)

  -- Build the scope: a mapping from unversioned names to RefTarget
  let
    -- local definitions are in scope unqualified and qualified
    unqualLocalScope :: HashMap Name [RefTarget]
    unqualLocalScope = HashMap.fromListWith (++) $
      [ (name, [RefPred r]) | (name, r, _) <- localPreds ] ++
      [ (name, [RefType r]) | (name, r, _) <- localTypes ]

    qualLocalScope :: HashMap Name [RefTarget]
    qualLocalScope = HashMap.fromListWith (++) $
      [ (qualify name, [RefPred r]) | (name, r, _) <- localPreds ] ++
      [ (qualify name, [RefType r]) | (name, r, _) <- localTypes ]

    -- inherited definitions are in scope unqualified and qualified
    --   (but unqualified local names override unqualified inherited names)
    qualInheritedScope :: HashMap Name (Set RefTarget)
    qualInheritedScope = HashMap.fromListWith Set.union
      [ (name, Set.singleton target)
      | schema <- inherits
      , (name, target) <- allRefs schema
      ]

    unqualInheritedScope :: HashMap Name (Set RefTarget)
    unqualInheritedScope = HashMap.fromListWith Set.union
      [ (unqualify name, Set.singleton target)
      | schema <- inherits
      , (name, target) <- allRefs schema
      ]

    -- imported names are in scope qualified.
    importedScope :: HashMap Name (Set RefTarget)
    importedScope =
      foldr (HashMap.unionWith Set.union) HashMap.empty
        [ fmap Set.singleton $ HashMap.fromList $ allRefs schema
        | schema <- imports
        ]

    allRefs :: ResolvedSchema -> [(Name, RefTarget)]
    allRefs ResolvedSchema{..} =
      [ (typeRef_name ref, RefType ref)
      | ref <- HashMap.keys $
          resolvedSchemaTypes <> resolvedSchemaReExportedTypes ] ++
      [ (predicateRef_name ref, RefPred ref)
      | ref <- HashMap.keys $
          resolvedSchemaPredicates <> resolvedSchemaReExportedPredicates ]

    qualify :: Name -> Name
    qualify x = namespace <> "." <> x

    unqualify :: Name -> Name
    unqualify = snd . splitDot

    scope = resolveRef $
      HashMap.union (fmap Set.fromList unqualLocalScope) $
      HashMap.union unqualInheritedScope $
      HashMap.unionWith Set.union (fmap Set.fromList qualLocalScope) $
      HashMap.unionWith Set.union qualInheritedScope $
      importedScope

  -- Check for multiple definitions of the same name/version.
  -- Multiple definitions of the same name is OK: an unqualified
  -- reference will be rejected as ambiguous, but can be resolved by
  -- using an explicit version.
  forM_ (HashMap.toList unqualLocalScope) $ \(_, targets) -> do
    let perTarget = Map.fromListWith (+) $
          [ ((predicateRef_name, predicateRef_version), 1::Int)
          | RefPred PredicateRef{..} <- targets] ++
          [ ((typeRef_name, typeRef_version), 1)
          | RefType TypeRef{..} <- targets]
    forM (Map.toList perTarget) $ \((name,ver), num) ->
      when (num > 1) $ throwError $
       "multiple definitions for: " <> name <> "." <> showt ver

  -- Check for inheriting multiple versions of a predicate/types
  --
  -- As a special case, if the schema is called "all" then allow name
  -- clashes in the inheritance set. This is to facilitate our "all.N"
  -- convention for declaring the "top-level" schema which will
  -- inevitably have name clashes, but we don't care about what it
  -- exports.
  unless (namespace == "all") $
    forM_ (HashMap.toList unqualInheritedScope) $ \(name, targets) -> do
      case Set.toList targets of
        (_:_:_) -> throwError $
          "inherited schemas give multiple definitions for: " <> name
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
      HashMap.union (fmap Set.fromList unqualLocalScope) $
      HashMap.union unqualInheritedScope $
      HashMap.unionWith Set.union (fmap Set.fromList qualLocalScope)
      qualInheritedScope

  -- resolve queries
  localDeriving <- forM [ d | d@SourceDeriving{} <- schemaDecls ] $
    \(SourceDeriving ref derive) -> do
      ty <- lookupResultToExcept ref $ predScope ref
      ref <- case ty of
        RefPred ref -> return ref
        RefType ref -> throwError $
          "cannot define a query for a typedef: " <> showTarget (RefType ref)
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

data FieldMatching = ExactMatch | AllowNew

data Opt = Option | Field

-- | For a backward compatibility to work if predicate A depends on predicate
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
backCompatible
  :: HashMap TypeRef TypeDef -- ^ type definitions
  -> HashMap PredicateRef PredicateRef -- ^ current evolutions map
  -> Type                    -- ^ updated type
  -> Type                    -- ^ old type
  -> Maybe Text              -- ^ compatibility error
backCompatible types evolvedBy new old = go new old
  where
    get ty = case HashMap.lookup ty types of
      Just v -> typeDefType v
      Nothing -> error $ "unknown type " <> show ty

    go (NamedType new) (NamedType old)
      | new == old = Nothing
      | otherwise = go (get new) (get old)
    go (NamedType t) old = go (get t) old
    go new (NamedType t) = go new (get t)
    go (Maybe new) (Maybe old) = go new old
    go Byte Byte = Nothing
    go Nat Nat = Nothing
    go String String = Nothing
    go Boolean Boolean = Nothing
    go (Array new) (Array old) = go new old
    go (Predicate new) (Predicate old)
      | evolved new /= evolved old = Just
          $ "type changed from " <> showPredicateRef old
          <> " to " <> showPredicateRef new
      | otherwise = Nothing
    go (Enumerated new) (Enumerated old) =
      compareFieldList ExactMatch Option new' old'
      where
        new' = map unitOpt new
        old' = map unitOpt old
        unitOpt name = FieldDef name (Record [])
    go (Sum new) (Sum old) = compareFieldList ExactMatch Option new old
    go (Record new) (Record old) = compareFieldList AllowNew Field new old
    go _ _ = Just "type changed"

    -- get most evolved version of a predicate
    evolved p = case HashMap.lookup p evolvedBy of
      Nothing -> p
      Just p' -> if p' == p then p else evolved p'

    compareFieldList match optName new old =
      case match of
        ExactMatch | not (null addedFields) ->
          Just $ plural optName addedFields
            <> " added: " <> Text.unwords addedFields
        _ | not (null removedFields) ->
          Just $ plural optName removedFields
            <> " missing: " <> Text.unwords removedFields
        _ ->
          asum $ map compareField matchingFields
      where
        names = map fieldDefName
        oldByName = Map.fromList (zip (names old) old)
        newByName = Set.fromList (names new)
        removedFields = filter (not . flip Set.member newByName) (names old)
        addedFields = filter (not . flip Map.member oldByName) (names new)
        matchingFields =
          [ (name, fNew, fOld)
          | FieldDef name fNew <- new
          , Just (FieldDef _ fOld) <- [Map.lookup name oldByName]
          ]
        compareField (name, new, old) = addLocation <$> go new old
          where
            addLocation err =
              "in " <> showOpt optName <> " '" <> name <> "', " <> err


    plural s [_] = showOpt s
    plural s _ = showOpt s <> "s"
    showOpt Option = "option"
    showOpt Field = "field"


resolveType :: AngleVersion -> Scope -> SourceType -> Except Text Type
resolveType ver scope typ = go typ
  where
  go typ = case typ of
    Byte -> return Byte
    Nat -> return Nat
    String -> return String
    Array ty -> Array <$> go ty
    Record fields -> do checkFields fields; Record <$> mapM goField fields
    Sum fields -> do checkFields fields; Sum <$> mapM goField fields
    Predicate ref -> goRef ref
    NamedType ref -> goRef ref  -- shouldn't happen, but handle it anyway
    Maybe ty -> Maybe <$> go ty
    Enumerated names -> do mapM_ checkName names; return (Enumerated names)
    Boolean -> return Boolean

  goRef ref = do
    target <- lookupResultToExcept ref $ scope ref
    case target of
      RefType ref -> return (NamedType ref)
      RefPred ref -> return (Predicate ref)

  checkFields fields = do
    sequence_
      [ throwError $ "duplicate field: " <> x
        | x:_:_ <- group $ sort $ map fieldDefName fields ]
    when (checkReservedWordsInFieldNames ver) $
      mapM_ (checkName . fieldDefName) fields
    when (caseRestriction ver) $
      mapM_ (checkFieldName . fieldDefName) fields

  goField (FieldDef name ty) = FieldDef name <$> go ty

data LookupResult
  = OutOfScope
  | Ambiguous [RefTarget]
  | ResolvesTo RefTarget

lookupResultToExcept :: SourceRef -> LookupResult -> Except Text RefTarget
lookupResultToExcept ref OutOfScope =
  throwError $ "not in scope: " <> showSourceRef ref
lookupResultToExcept ref (Ambiguous targets) =
  throwError $ showSourceRef ref <> " is ambiguous. It could refer to: " <>
      Text.intercalate ", " (map showTarget targets)
lookupResultToExcept _ (ResolvesTo target) = return target

resolveRef :: HashMap Name (Set RefTarget) -> Scope
resolveRef scope (SourceRef name Nothing) =
  case maybe [] Set.toList $ HashMap.lookup name scope of
    [] -> OutOfScope
    [one] -> ResolvesTo one
    many -> Ambiguous many
resolveRef scope (SourceRef name (Just ver)) =
  let targets = maybe [] Set.toList $ HashMap.lookup name scope
  in
      case [ RefType r | RefType r <- targets, typeRef_version r == ver ]
        ++ [ RefPred r | RefPred r <- targets, predicateRef_version r == ver ]
      of
        [] -> OutOfScope
        [one] -> ResolvesTo one
        many -> Ambiguous many


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

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
  ) where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Char
import Data.Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding (group)
import Data.Tree
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
  , schemasCurrentVersion :: Version
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
        schemaInherits ++ [ name | SourceImport name <- schemaDecls ]

    resolveSchemas env [] = return env
    resolveSchemas env (AcyclicSCC one : rest) = do
      resolved <- resolveOneSchema env srcAngleVersion one
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

  return Schemas
    { schemasSchemas = HashMap.fromList
        [ (schemaVersion s, s) | s <- allTheSchemas ]
    , schemasCurrentVersion = maximum (map schemaVersion allTheSchemas)
    , schemasResolved =
        [ resolved
        | AcyclicSCC one <- sccs
        , Just resolved <- [HashMap.lookup (schemaName one) finalEnv ] ]
    }


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
  -> SourceSchema
  -> Except Text ResolvedSchema

resolveOneSchema env angleVersion SourceSchema{..} =
  flip catchError (\e -> throwError $ "In " <> schemaName <> ":\n  " <> e) $ do
  let
    SourceRef namespace maybeVer = parseRef schemaName

  checkNameSpace namespace

  -- Version of this schema
  version <- case maybeVer of
    Nothing -> throwError $ "missing version: " <> schemaName
    Just v -> return v

  -- All the schemas we're inheriting from
  inherits <- forM schemaInherits $ \name ->
    case HashMap.lookup name env of
      Nothing -> throwError $ "unknown schema: " <> name
      Just schema -> return schema

  -- All the schemas we imported
  imports <- forM [ name | SourceImport name <- schemaDecls ] $ \name ->
    case HashMap.lookup name env of
      Nothing -> throwError $ "unknown schema: " <> name
      Just schema -> return schema

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
    }


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
    "None", "operator"
  ]


instance Pretty Schemas where
  pretty Schemas{} = mempty -- TODO

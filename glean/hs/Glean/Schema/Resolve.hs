{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric #-}

module Glean.Schema.Resolve
  ( parseAndResolveSchema
  , parseAndResolveSchemaCached
  , SchemaParserCache
  , resolveType
  , runResolve
  , resolveQuery
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char
import Data.Graph
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding (group)
import TextShow

import Glean.Angle.Hash
import Glean.Angle.Parser
import Glean.Angle.Types
import Glean.Schema.Types
import Glean.Schema.Util
import Glean.Schema.Evolve (validateResolvedEvolutions)

type SchemaParserCache = HashMap Hash SourceSchemas

-- ---------------------------------------------------------------------------
-- High-level schema parsing / resolution APIs

--
-- | Useful packaging of 'parseSchema' and 'resolveSchema'. Note that
-- parsing and resolution of a schema is a pure function.
--
parseAndResolveSchema
  :: ByteString
  -> Either String (SourceSchemas, ResolvedSchemas)
parseAndResolveSchema str =
  case parseSchema str of
    Left str -> Left str
    Right ss -> case resolveSchema ss of
      Left txt -> Left (Text.unpack txt)
      Right r -> Right (ss, r)

--
-- | Like parseAndResolveSchema but uses a cache to avoid repeatedly
-- parsing the same schema fragments.
--
-- We split the input string at "schema" declarations and hash the
-- content of each one to use as the cache key. There are a couple of
-- caveats here:
--
--  * we lose the benefit of the FILE annotations that tell us the
--    original source file name for error messages. Therefore don't
--    use this parsing method when you want error messages; use
--    the uncached parseAndResolveSchema instead.
--
--  * "schema" should appear in column 0. But if it doesn't, the worst
--    that can happen is we miss out on some caching. Since "schema" is
--    a keyword, it can't appear inside a schema or anything silly like
--    that.
--
parseAndResolveSchemaCached
  :: SchemaParserCache
  -> ByteString
  -> Either String (SourceSchemas, ResolvedSchemas, SchemaParserCache)
parseAndResolveSchemaCached cache str = do
  let
    (ver, rest) = stripAngleVersion str

    cutSchemas b =
      (h <> "\n") : -- put the "\n" back on the end
        if ByteString.null t
          then []
          else cutSchemas (ByteString.drop 1 t) -- drop the "\n"
      where (h,t) = ByteString.breakSubstring "\nschema" b

    schemaFragments =
      [ (hashByteString frag, frag)
      | frag <- cutSchemas rest ]

  parsed <- forM schemaFragments $ \(key, str) ->
    case HashMap.lookup key cache of
      Nothing -> case parseSchema str of
        Left err -> Left err
        Right ss -> return (key, ss)
      Just cached -> return (key, cached)

  let
    merged = SourceSchemas
      { srcAngleVersion = ver
      , srcSchemas = schemas
      , srcEvolves = evolves }
      where
        schemas = concatMap (srcSchemas . snd) parsed
        evolves = concatMap (srcEvolves . snd) parsed

    newCache = HashMap.union cache (HashMap.fromList parsed)

  case resolveSchema merged of
    Left txt -> Left (Text.unpack txt)
    Right r -> Right (merged, r, newCache)

--
-- | Turn 'SourceSchemas' into a 'ResolvedSchemas' by resolving all the
-- references and checking for validity.
--
resolveSchema :: SourceSchemas -> Either Text ResolvedSchemas
resolveSchema SourceSchemas{..} = runExcept $ do
  checkAngleVersion srcAngleVersion
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

  when (srcAngleVersion >= AngleVersion 6) $
    liftEither $ validateResolvedEvolutions resolved

  return ResolvedSchemas
    { schemasHighestVersion =
        if null allSchemas
           then Nothing
           else Just (maximum $ map resolvedSchemaVersion allSchemas)
    , schemasResolved = resolved
    }

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

    qualifyNameEnv :: NameEnv t -> NameEnv t
    qualifyNameEnv env = HashMap.fromList
      [ (SourceRef (qualify name) ver, target)
      | (SourceRef name ver, target) <- HashMap.toList env ]

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
    qualInheritedScope =
      unionNameEnvs $ map resolvedSchemaQualScope inherits

    unqualInheritedScope :: NameEnv RefResolved
    unqualInheritedScope =
      unionNameEnvs $ map resolvedSchemaUnqualScope inherits

    -- imported names are in scope qualified only
    importedScope :: NameEnv RefResolved
    importedScope = unionNameEnvs $ map resolvedSchemaQualScope imports

    unionNameEnvs :: [NameEnv RefResolved] -> NameEnv RefResolved
    unionNameEnvs = foldl' (HashMap.unionWith Set.union) HashMap.empty

    nameEntries name ver target =
      [ (SourceRef name Nothing, set),
        (SourceRef name (Just ver), set) ]
      where set = Set.singleton target

    -- local definitions are in scope unqualified and qualified
    unqualLocalScope :: NameEnv RefResolved
    unqualLocalScope = HashMap.fromListWith Set.union unqualLocalEntities

    qualLocalScope :: NameEnv RefResolved
    qualLocalScope = qualifyNameEnv unqualLocalScope

    unqualLocalEntities =
      -- P and P.1 for each local predicate
      [ entry
      | (name, r, _) <- localPreds
      , entry <- nameEntries name (predicateRef_version r) (RefPred r)
      ] ++
      -- T and T.1 for each local type
      [ entry
      | (name, r, _) <- localTypes
      , entry <- nameEntries name (typeRef_version r) (RefType r)
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
      type' <- runResolve angleVersion scope (resolveType typeDefType)
      return (name, TypeDef
        { typeDefRef = ref
        , typeDefType = type' })

  -- resolve predicate definitions
  predicates <- forM localPreds $
    \(name, ref, PredicateDef{..}) -> do
      checkName name
      runResolve angleVersion scope $ do
        key <- resolveType predicateDefKeyType
        value <- resolveType predicateDefValueType
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
        _ -> throwError $ showRef ref <> " is not a predicate"
      resolved <- runResolve angleVersion scope (resolveDeriving derive)
      return (ref, resolved)

  let
    localTypeNames = HashSet.fromList $
      map (typeRef_name . typeDefRef . snd) types

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
        , not (typeRef_name ref `HashSet.member` localTypeNames)
        ]

    localPredicateNames = HashSet.fromList $
      [ predicateRef_name (predicateDefRef def) | (_, def) <- predicates ]

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
        , not (predicateRef_name ref `HashSet.member` localPredicateNames)
        ]

    exportedUnqualScope = HashMap.union unqualLocalScope unqualInheritedScope
      -- Note: local names override inherited names

    -- Qualified exported names. The "all" schema is special: we don't
    -- re-export things qualified with "all.", only with the original
    -- schema name.
    exportedQualScope
      | namespace == "all" = qualInheritedScope
      | otherwise =
         HashMap.union
           (qualifyNameEnv exportedUnqualScope)
           qualInheritedScope -- NB. local overrides inherited

  schemaEvolves <- traverse schemaByName evolves

  return ResolvedSchema
    { resolvedSchemaName = namespace
    , resolvedSchemaVersion = version
    , resolvedSchemaAngleVersion = angleVersion
    , resolvedSchemaTypes = localTypes
    , resolvedSchemaReExportedTypes = reExportedTypes
    , resolvedSchemaPredicates = localPredicates
    , resolvedSchemaReExportedPredicates = reExportedPredicates
    , resolvedSchemaUnqualScope = exportedUnqualScope
    , resolvedSchemaQualScope = exportedQualScope
    , resolvedSchemaDeriving = HashMap.fromList localDeriving
    , resolvedSchemaEvolves = Set.fromList (schemaRef <$> schemaEvolves)
    }

resolveType :: (ShowRef t, ShowRef p) => SourceType -> Resolve p t (Type_ p t)
resolveType typ = go typ
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
    EnumeratedTy names -> lift $ do
      mapM_ checkName names
      return (EnumeratedTy names)
    BooleanTy -> return BooleanTy

  goRef ref = do
    scope <- getScope
    target <- lift $ lookupResultToExcept ref $
      resolveRef scope ref
    case target of
      RefType ref -> return (NamedTy ref)
      RefPred ref -> return (PredicateTy ref)

  checkFields fields = do
    sequence_
      [ throwError $ "duplicate field: " <> x
        | x:_:_ <- group $ sort $ map fieldDefName fields ]
    lift $ do
      mapM_ (checkName . fieldDefName) fields
      mapM_ (checkFieldName . fieldDefName) fields

  goField (FieldDef name ty) = FieldDef name <$> go ty


lookupResultToExcept
  :: (ShowRef t, ShowRef p)
  => SourceRef
  -> LookupResult (RefTarget p t)
  -> Except Text (RefTarget p t)
lookupResultToExcept ref res =
  either throwError return (lookupResultToEither ref res)

checkAngleVersion :: AngleVersion -> Except Text ()
checkAngleVersion v =
  unless (v >= latestSupportedAngleVersion) $
    let ver = Text.pack $ show $ pretty v in
    throwError $ "Angle version " <> ver <> " is not supported"

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
    "None", "operator", {- TODO: "Enum" -}

    -- Reserved for temporary predicates
    "_tmp_"
  ]


-- -----------------------------------------------------------------------------
-- Resolving queries

runResolve
  :: AngleVersion
  -> NameEnv (RefTarget p t)
  -> Resolve p t a
  -> Except Text a
runResolve ver scope act = runReaderT act (ver,scope)

type Resolve p t a =
  ReaderT (AngleVersion, NameEnv (RefTarget p t)) (Except Text) a

getScope :: Resolve p t (NameEnv (RefTarget p t))
getScope = asks snd

resolveDeriving
  :: (ShowRef t, ShowRef p)
  => SourceDerivingInfo' SrcSpan
  -> Resolve p t (DerivingInfo (Query_ p t))
resolveDeriving NoDeriving = return NoDeriving
resolveDeriving (Derive when query) = Derive when <$> resolveQuery query

resolveQuery
  :: (ShowRef t, ShowRef p)
  => SourceQuery
  -> Resolve p t (Query_ p t)
resolveQuery (SourceQuery head stmts) =
  SourceQuery
    <$> mapM resolvePat head
    <*> mapM resolveStatement stmts

resolvePat
  :: (ShowRef t, ShowRef p)
  => SourcePat
  -> Resolve p t (SourcePat_ SrcSpan p t)
resolvePat pat = case pat of
  Nat s i -> return (Nat s i)
  String s t -> return (String s t)
  StringPrefix s t -> return (StringPrefix s t)
  ByteArray s b -> return (ByteArray s b)
  Array s pats -> Array s <$> mapM resolvePat pats
  ArrayPrefix s pats -> ArrayPrefix s <$> mapM resolvePat pats
  Tuple s pats -> Tuple s <$> mapM resolvePat pats
  Wildcard s -> return (Wildcard s)
  TypeSignature s pat ty ->
    TypeSignature s
      <$> resolvePat pat
      <*> resolveType ty
  Variable s n -> return (Variable s n)
  FactId s Nothing id -> return (FactId s Nothing id)
  FactId s (Just pred) id -> do
    res <- resolveTypeOrPred s pred
    case res of
      RefPred ref ->
        return (TypeSignature s (FactId s Nothing id) (PredicateTy ref))
      _other -> prettyErrorIn pat $ "not a predicate: " <> pretty pred
  OrPattern s l r -> OrPattern s <$> resolvePat l <*> resolvePat r
  Negation s pat -> Negation s <$> resolvePat pat
  Never s -> return (Never s)
  IfPattern s cond then_ else_ ->
    IfPattern s
      <$> resolvePat cond
      <*> resolvePat then_
      <*> resolvePat else_
  ElementsOfArray s pat -> ElementsOfArray s <$> resolvePat pat
  KeyValue s k v -> KeyValue s <$> resolvePat k <*> resolvePat v
  NestedQuery s q -> NestedQuery s <$> resolveQuery q
  Struct s fields -> Struct s <$> mapM resolveField fields
    where resolveField (Field n pat) = Field n <$> resolvePat pat
  App s (Variable svar txt) args
    | Just primOp <- HashMap.lookup txt primitives -> do
      Prim s primOp <$> mapM resolvePat args
    | otherwise ->
    case args of
      [arg] -> do
        arg' <- resolvePat arg
        res <- resolveTypeOrPred svar txt
        case res of
          RefPred ref -> return (Clause s ref arg')
          RefType ref -> return (TypeSignature s arg' (NamedTy ref))
            -- The syntax "T pat" for "pat : T" is something we might
            -- consider deprecating later. For now it's just desugared
            -- here.
      _ -> prettyErrorIn pat "unexpected extra argument(s)"
  App s (StringPrefix a b) [pat] -> do
    pat' <- resolvePat pat
    return (App s (StringPrefix a b) [pat'])
  App{} -> prettyErrorIn pat "invalid pattern"
  Clause{} -> internal
  Prim{} -> internal
  where
  internal = throwError $ "internal: unexpected: " <>
    Text.pack (show (pretty pat))

resolveTypeOrPred
  :: (ShowRef p, ShowRef t)
  => SrcSpan
  -> Name
  -> Resolve p t (RefTarget p t)
resolveTypeOrPred span txt = do
  scope <- getScope
  let ref = parseRef txt
  case lookupResultToEither ref $ resolveRef scope ref of
    Left s -> prettyErrorAt span (pretty s)
    Right r -> return r

prettyError :: Doc ann -> Resolve p t a
prettyError = throwError . Text.pack . show

prettyErrorIn :: IsSrcSpan s => SourcePat' s -> Doc ann -> Resolve p t a
prettyErrorIn pat doc = prettyErrorAt (sourcePatSpan pat) doc

prettyErrorAt :: IsSrcSpan span => span -> Doc ann -> Resolve p t a
prettyErrorAt span doc = prettyError $ vcat
  [ pretty span
  , doc
  ]

resolveStatement
  :: (ShowRef t, ShowRef p)
  => SourceStatement
  -> Resolve p t (Statement_ p t)
resolveStatement (SourceStatement l r) =
  SourceStatement <$> resolvePat l <*> resolvePat r

primitives :: HashMap Text PrimOp
primitives = HashMap.fromList
  [ ("prim.toLower", PrimOpToLower)
  , ("prim.length", PrimOpLength)
  , ("prim.relToAbsByteSpans", PrimOpRelToAbsByteSpans)
  , ("prim.gtNat", PrimOpGtNat)
  , ("prim.geNat", PrimOpGeNat)
  , ("prim.ltNat", PrimOpLtNat)
  , ("prim.leNat", PrimOpLeNat)
  , ("prim.neNat", PrimOpNeNat)
  , ("prim.addNat", PrimOpAddNat)
  , ("prim.neExpr", PrimOpNeExpr)
  ]

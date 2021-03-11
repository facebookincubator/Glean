module Glean.Query.Typecheck
  ( typecheck
  , typecheckDeriving
  , NameResolutionPolicy(..)
  , TcEnv(..)
  , emptyTcEnv
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Char
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import Glean.Angle.Types as Schema hiding (Type)
import qualified Glean.Angle.Types as Schema
import Glean.Query.Codegen
import Glean.Query.Typecheck.Types
import Glean.Query.Types as Parser
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS hiding (Match(..))
import Glean.Database.Schema.Types
import Glean.Schema.Util
import Glean.Schema.Resolve

data TcEnv = TcEnv
  { tcEnvTypes :: HashMap TypeRef TypeDetails
  , tcEnvPredicates :: HashMap PredicateRef PredicateDetails
  }

emptyTcEnv :: TcEnv
emptyTcEnv = TcEnv HashMap.empty HashMap.empty

-- | Typecheck a 'SourceQuery' which is in terms of schema types and
-- turn it into a 'TypecheckedQuery' which is in terms of raw Terms,
-- ready for compiling to bytecode.
typecheck
  :: DbSchema
  -> AngleVersion
  -> NameResolutionPolicy
  -> SourceQuery
  -> Except Text TypecheckedQuery
typecheck dbSchema ver policy query = do
  let
    tcEnv = TcEnv
      { tcEnvPredicates = predicatesByRef dbSchema
      , tcEnvTypes = schemaTypesByRef dbSchema
      }
  (q@(TcQuery ty _ _ _), TypecheckState{..}) <-
    let state = initialTypecheckState tcEnv ver policy TcModeQuery in
    flip runStateT state $ do
      modify $ \s -> s { tcVisible = varsQuery query mempty }
      inferQuery ContextExpr query
        <* freeVariablesAreErrors <* unboundVariablesAreErrors
  return (QueryWithInfo q tcNextVar ty)

-- | Typecheck the query for a derived predicate
typecheckDeriving
  :: TcEnv
  -> AngleVersion
  -> NameResolutionPolicy
  -> PredicateDetails
  -> SourceDerivingInfo
  -> Except Text (DerivingInfo TypecheckedQuery)
typecheckDeriving tcEnv ver policy PredicateDetails{..} derivingInfo = do
  (d, TypecheckState{..}) <-
    let state = initialTypecheckState tcEnv ver policy TcModePredicate
    in
    flip runStateT state $ do
    flip catchError
      (\e -> throwError $ "In " <>
        Text.pack (show (pretty predicateRef)) <> ":\n  " <> e) $ do
      case derivingInfo of
        NoDeriving -> return NoDeriving
        Derive deriveWhen q -> do
          modify $ \s -> s { tcVisible = varsQuery q mempty }
          -- we typecheck the pattern first, because we know its type.
          (head, stmts) <- needsResult q
          let
            (key, maybeVal) = case head of
              KeyValue key val -> (key, Just val)
                -- Backwards compat, we had a predicate in schema v4 of the form
                --   X -> prim.toLower X
                -- but this doesn't parse if -> binds tighter than application.
              App (KeyValue key val) x -> (key, Just (App val x))
              _other -> (head, Nothing)
          key' <- typecheckPattern ContextExpr predicateKeyType key
          maybeVal' <- case maybeVal of
            Nothing
              | unit `eqType` predicateValueType -> return Nothing
              | otherwise -> prettyError $ nest 4 $ vcat
                [ "a functional predicate must return a value,"
                , "i.e. the query should have the form 'X -> Y where .." ]
            Just val -> Just <$>
              typecheckPattern ContextExpr predicateValueType val
          stmts' <- mapM typecheckStatement stmts
          freeVariablesAreErrors
          unboundVariablesAreErrors
          nextVar <- gets tcNextVar
          return $ Derive deriveWhen $
            QueryWithInfo (TcQuery predicateKeyType key' maybeVal' stmts')
              nextVar predicateKeyType
  return d

needsResult :: SourceQuery -> T (SourcePat, [SourceStatement])
needsResult (SourceQuery (Just p) stmts) = return (p,stmts)
needsResult q@(SourceQuery Nothing stmts) = case reverse stmts of
  (SourceStatement (Variable v) _ : _) ->
    return (Variable v, stmts)
  (SourceStatement Wildcard rhs : rstmts) ->
    return (rhs, reverse rstmts)
  _ -> throwError $ "the last statement should be an expression: " <>
    Text.pack (show (pretty q))

ignoreResult :: SourcePat -> SourcePat
ignoreResult (OrPattern a b) = OrPattern (ignoreAlt a) (ignoreAlt b)
ignoreResult other = other

ignoreAlt :: SourcePat -> SourcePat
ignoreAlt p = case p of
  OrPattern a b -> OrPattern (ignoreAlt a) (ignoreAlt b)
  NestedQuery (SourceQuery Nothing stmts) ->
    NestedQuery (SourceQuery (Just empty) stmts)
  other ->
    NestedQuery (SourceQuery (Just empty) [SourceStatement Wildcard other])
  where empty = TypeSignature (Parser.Tuple []) unit

inferQuery :: Context -> SourceQuery -> T TcQuery
inferQuery ctx q = do
  (head,stmts) <- needsResult q
  stmts' <- mapM typecheckStatement stmts
  (head', ty) <- inferExpr ctx head
  return (TcQuery ty head' Nothing stmts')

typecheckQuery :: Context -> Type -> SourceQuery -> T TcQuery
typecheckQuery ctx ty q = do
  (head,stmts) <- needsResult q
  head' <- typecheckPattern ctx  ty head
  stmts' <- mapM typecheckStatement stmts
  return (TcQuery ty head' Nothing stmts')

unexpectedValue :: T a
unexpectedValue = throwError $
  "a key/value pattern (X -> Y) cannot be used here"

typecheckStatement :: SourceStatement -> T TcStatement
typecheckStatement (SourceStatement lhs rhs0) = do
  let
    -- We want to allow things like (A; B) | (C; D) at the statement
    -- level, without enforcing that B and D are expressions or that
    -- they have the same type.  After the parser this would appear
    -- as
    --     _ = ((A; B) | (C; D))
    --
    -- so when a statement has a wildcard on the left, we transform
    -- NestedQuery on the right to replace a missing head with {}.
    -- This preempts 'needsResult' from doing its transformation and
    -- failing when the last statement is not an expression.
    rhs
      | Wildcard <- lhs = ignoreResult rhs0
      | otherwise = rhs0
  (rhs', ty) <- inferExpr ContextPat rhs
  lhs' <- typecheckPattern ContextPat ty lhs
  return $ TcStatement ty lhs' rhs'

resolveTypeOrPred :: Text -> T (Maybe RefTarget)
resolveTypeOrPred txt = do
  policy <- gets tcNameResolutionPolicy
  let ref = parseRef txt
  case toScope policy ref of
    ResolvesTo target -> return (Just target)
    OutOfScope -> return Nothing
    other -> do
      void $ lift $ lookupResultToExcept ref other
      return Nothing

-- | The context in which we're typechecking: either an expression or
-- a pattern.
data Context = ContextExpr | ContextPat

-- For the term built by a Query, we have no local type information,
-- so we attempt to infer the type of the expression.  Therefore we
-- don't allow structs or alts. Variables must be occurrences
-- (because this is a term, not a pattern), and Wildcards are
-- disallowed.
inferExpr :: Context -> SourcePat -> T (TcPat, Type)
inferExpr ctx pat = case pat of
  Parser.Nat w -> return (RTS.Nat w, Schema.Nat)
    -- how would we do ByteTy?
  Parser.String s ->
    return (RTS.String (Text.encodeUtf8 s), Schema.String)
  StringPrefix s ->
    return
      (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) (mkWild Schema.String)),
        Schema.String)
  Parser.ByteArray b ->
    return (RTS.ByteArray b, Schema.Array Schema.Byte)
  (App (StringPrefix s) [pat]) -> do
    rest <- typecheckPattern ctx Schema.String pat
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) rest), Schema.String)
  Parser.Tuple ts -> do
    (ts,tys) <- unzip <$> mapM (inferExpr ctx) ts
    return (RTS.Tuple ts, tupleSchema tys)
  Parser.Array [] ->
    return (RTS.Array [], Schema.Array (Schema.Record []))
  Parser.Array (t:ts) -> do
    (t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    return (RTS.Array (t':ts'), Schema.Array ty)
  Variable name
    | name == "false" -> return (falseVal, lowerBool)
    | name == "true" -> return (trueVal, lowerBool)
    | otherwise -> inferVar ctx name
  Parser.FactId (Just pred) fid -> do
    isFactIdAllowed pat
    res <- resolveTypeOrPred pred
    case res of
      Nothing -> throwError $
        "unknown type or predicate in literal fact ID: " <> pred
      Just (RefPred ref) -> do
        TcEnv{..} <- gets tcEnv
        pid <- case HashMap.lookup ref tcEnvPredicates of
          Nothing -> throwError $ "inferExpr: " <> Text.pack (show (pretty ref))
          Just details -> return (predicatePid details)
        return (
          Ref (MatchFid (Fid (fromIntegral fid))),
          Schema.Predicate (PidRef pid ref))
      _other -> throwError $ "not a predicate: " <> pred
  App (Variable txt) args@(arg:_)
    | Just (primOp, primArgTys, retTy) <- HashMap.lookup txt primitives -> do
        args' <- primInferAndCheck args primOp primArgTys
        return
            ( RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args')))
            , retTy )
    | otherwise -> do
      res <- resolveTypeOrPred txt
      case res of
        Nothing -> throwError $
          "unknown type or predicate while inferring application: " <> txt
        Just (RefPred ref) -> tcFactGenerator ref arg
        Just (RefType ref) -> do
          TcEnv{..} <- gets tcEnv
          case HashMap.lookup ref tcEnvTypes of
            Nothing -> lift $ throwError $ "unknown type: " <> txt
            Just TypeDetails{..} ->
              (,typeType) <$> typecheckPattern ctx typeType arg
  OrPattern a b -> do
    ((a', ty), b') <-
      orPattern
        a (inferExpr ctx a)
        b (\(_,ty) -> typecheckPattern ctx ty b)
    return (Ref (MatchExt (Typed ty (TcOr a' b'))), ty)
  NestedQuery q -> do
    q@(TcQuery ty _ _ _) <- inferQuery ctx q
    return (Ref (MatchExt (Typed ty (TcQueryGen q))), ty)
  ElementsOfArray e -> do
    (e', ty) <- inferExpr ContextExpr e
    case ty of
      (Schema.Array elemTy) ->
        return (Ref (MatchExt (Typed elemTy (TcElementsOfArray e'))), elemTy)
      _other -> throwError $ Text.pack $ show $
        nest 4 $ vcat
          [ "type error in array element generator:"
          , "expression: " <> pretty e
          , "does not have an array type"
          ]
  TypeSignature e ty -> do
    policy <- gets tcNameResolutionPolicy
    v <- gets tcAngleVersion
    ty' <- lift $ resolveType v (toScope policy) ty
    typ <- convertType policy ty'
    (,typ) <$> typecheckPattern ctx typ e

  KeyValue{} -> unexpectedValue

  _ -> prettyError $ nest 4 $ vcat
    [ "can't infer the type of: " <> pretty pat
    , "try adding a type annotation like (" <> pretty pat <> " : T)"
    , "or reverse the statement (Q = P instead of P = Q)"
    ]

convertType :: NameResolutionPolicy -> Schema.Type -> T Type
convertType policy ty = do
  let rtsType = case policy of
        UseScope _ rtsType -> rtsType
        Qualified dbSchema -> dbSchemaRtsType dbSchema
  case rtsType ty of
    Just typ -> return typ
    Nothing -> throwError "cannot convert type"

-- | Check that the pattern has the correct type, and generate the
-- low-level pattern with type-annotated variables.
typecheckPattern :: Context -> Type -> SourcePat -> T TcPat
typecheckPattern ctx typ pat = case (typ, pat) of
  (Schema.Byte, Parser.Nat w) -> return (RTS.Byte (fromIntegral w))
  (Schema.Nat, Parser.Nat w) -> return (RTS.Nat w)
  (Schema.String, Parser.String s) ->
    return (RTS.String (Text.encodeUtf8 s))
  (Schema.String, StringPrefix s) ->
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) (mkWild Schema.String)))
  (Schema.String, App (StringPrefix s) [pat]) -> do
    pat' <- typecheckPattern ctx Schema.String pat
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) pat'))
  (Schema.Array Schema.Byte, Parser.String s) ->
    return (RTS.ByteArray (Text.encodeUtf8 s))
  (Schema.Array Schema.Byte, Parser.ByteArray s) ->
    return (RTS.ByteArray s)
  (Schema.Array elemTy, Parser.Array pats) ->
    RTS.Array <$> mapM (typecheckPattern ctx elemTy) pats
  (Record fields, Parser.Tuple pats) | length fields == length pats ->
    RTS.Tuple <$>
      mapM (\(t,p) -> typecheckPattern ctx t p)
        (zip (map fieldDefType fields) pats)
  (Record fieldSchema, Struct fields)
    | all (`elem` map fieldDefName fieldSchema) (map fieldName fields) ->
      RTS.Tuple <$> mapM doField fieldSchema
    where
      fieldName (Parser.Field name _) = name
      doField (Schema.FieldDef name ty) =
        case [ pat | Parser.Field name' pat <- fields, name == name' ] of
          (pat:_) -> typecheckPattern ctx ty pat
          [] -> return (mkWild ty)
        -- missing field is a wildcard

  -- v1 syntax for sum type patterns was "con pat", but this could also
  -- be a type annotation:
  (Sum fields, App (Variable fieldName) [pat]) -> do
    v <- gets tcAngleVersion
    if v >= 2 then checkTypeAnn fieldName pat else do
    case lookupField fieldName fields of
      (ty, n):_ -> RTS.Alt n <$> typecheckPattern ctx ty pat
      _ -> checkTypeAnn fieldName pat

  (Sum fields, Struct [Field fieldName pat]) ->
    case lookupField fieldName fields of
      (ty, n) :_ -> RTS.Alt n <$> typecheckPattern ctx ty pat
      _ -> patTypeErrorDesc ("unknown alt: " <> fieldName) pat typ
  -- 'field' is shorthand for '{ field = _ }' when matching a sum type
  (Sum fields, Variable fieldName)
    | ((ty, n):_)  <- lookupField fieldName fields ->
      return (RTS.Alt n (mkWild ty))
  (Sum _, Struct _) ->
    patTypeErrorDesc
      "matching on a sum type should have the form { field = pattern }"
      pat typ
  (Schema.NamedType (ExpandedType _ ty), term) -> typecheckPattern ctx ty term
  (ty, App (Variable txt) args@(arg:_))
    | Just (primOp, primArgTys, retTy) <- HashMap.lookup txt primitives -> do
        unless (ty `eqType` retTy) $
          patTypeError pat ty
        args' <- primInferAndCheck args primOp primArgTys
        return (RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args'))))
    | otherwise -> do
    res <- resolveTypeOrPred txt
    case res of
      Just (RefPred ref')
        | Schema.Predicate (PidRef _ ref) <- ty
        , ref == ref' -> fst <$> tcFactGenerator ref arg
      Just (RefType ref) -> typeAnn ref arg
      _otherwise -> patTypeError pat ty

  (Maybe elemTy, pat) ->
    typecheckPattern ctx (lowerMaybe elemTy) pat
  (Schema.Enumerated names, Variable name)
    | Just n <- elemIndex name names ->
    return (RTS.Alt (fromIntegral n) (RTS.Tuple []))
  (Schema.Boolean, Variable name)
    | name == "false" -> return falseVal
    | name == "true" -> return trueVal

  (ty, ElementsOfArray pat) -> do
    pat' <- typecheckPattern ContextExpr (Schema.Array ty) pat
    return (Ref (MatchExt (Typed ty (TcElementsOfArray pat'))))

  (ty, OrPattern left right) -> do
    (left',right') <-
      orPattern
        left (typecheckPattern ctx ty left)
        right (\_ -> typecheckPattern ctx ty right)
    return (Ref (MatchExt (Typed ty (TcOr left' right'))))

  (ty, NestedQuery query) ->
    Ref . MatchExt . Typed ty . TcQueryGen <$> typecheckQuery ctx ty query

  (Schema.Predicate (PidRef _ ref), Parser.FactId mbRef fid) -> do
    isFactIdAllowed pat
    case mbRef of
      Nothing -> return ()
      Just txt -> do
        res <- resolveTypeOrPred txt
        case res of
          Just (RefPred ref') | ref == ref' -> return ()
          _otherwise -> patTypeError pat typ
    return $ Ref (MatchFid (Fid (fromIntegral fid)))

  (ty, TypeSignature e sigty) -> do
    policy <- gets tcNameResolutionPolicy
    v <- gets tcAngleVersion
    rsigty <- lift $ resolveType v (toScope policy) sigty
    sigty' <- convertType policy rsigty
    if ty `eqType` sigty'
      then typecheckPattern ctx ty e
      else
        -- Try compiling as a fact generator that matches the key type.
        -- e.g. if the field f has type pred where pred : key, then
        -- { f = X : key } can be used to force the pattern X to
        -- match the key type instead of the fact ID.
        case ty of
          Schema.Predicate (PidRef _ ref) -> do
            fst <$> tcFactGenerator ref pat
          _ -> patTypeError pat ty

  -- A match on a predicate type with a pattern that is not a wildcard
  -- or a variable does a nested match on the key of the predicate:
  (Schema.Predicate (PidRef _ ref), pat) | not (isVar pat) ->
    fst <$> tcFactGenerator ref pat

  (ty, Wildcard) -> return (mkWild ty)
  (ty, Variable name) -> varOcc ctx name ty

  (_, KeyValue{}) -> unexpectedValue

  -- type annotations are unnecessary, but we allow and check them
  (ty, q) -> patTypeError q ty
  where

  lookupField fieldName fields =
    [ (ty, n) | (Schema.FieldDef name ty, n) <- zip fields [0..]
    , name == fieldName ]

  checkTypeAnn fieldName pat = do
    res <- resolveTypeOrPred fieldName
    case res of
      Just (RefType ref) -> typeAnn ref pat
      _otherwise -> patTypeError pat typ

  typeAnn ref pat = do
    TcEnv{..} <- gets tcEnv
    case HashMap.lookup ref tcEnvTypes of
      Nothing ->
        patTypeErrorDesc ("unknown type: " <> Text.pack (show (pretty ref)))
          pat typ
      Just TypeDetails{..} -> do
        unless (typ `eqType` typeType) $ patTypeError pat typ
        typecheckPattern ctx typeType pat

tcFactGenerator
  :: PredicateRef
  -> SourcePat
  -> T (TcPat, Type)
tcFactGenerator ref pat = do
  TcEnv{..} <- gets tcEnv
  PredicateDetails{..} <- case HashMap.lookup ref tcEnvPredicates of
    Nothing -> throwError $ "tcFactGenerator: " <> Text.pack (show (pretty ref))
    Just details -> return details
  (kpat', vpat') <- case pat of
    KeyValue kpat vpat -> do
      kpat' <- typecheckPattern ContextPat predicateKeyType kpat
      vpat' <- typecheckPattern ContextPat predicateValueType vpat
      return (kpat', vpat')
    _other -> do
      kpat' <- typecheckPattern ContextPat predicateKeyType pat
      return (kpat', mkWild predicateValueType)
  let
    pidRef = (PidRef predicatePid ref)
    ty = Schema.Predicate pidRef
  return
    ( Ref (MatchExt (Typed ty (TcFactGen pidRef kpat' vpat')))
    , ty)


isVar :: SourcePat -> Bool
isVar Wildcard = True
isVar Variable{} = True
isVar _ = False

-- | Fact Id patterns (#1234 or #pred 1234) are only allowed in
-- queries, not in derived predicates, because they potentially allow
-- a type-incorrect fact to be constructed.
isFactIdAllowed :: SourcePat -> T ()
isFactIdAllowed pat = do
  mode <- gets tcMode
  when (mode /= TcModeQuery) $ throwError $
    "fact IDs are not allowed in a derived predicate: " <>
       Text.pack (show (pretty pat))


falseVal, trueVal :: TcPat
falseVal = RTS.Alt 0 (RTS.Tuple [])
trueVal = RTS.Alt 1 (RTS.Tuple [])

-- Smart constructor for wildcard patterns; replaces a wildcard that
-- matches the unit type with a concrete pattern.  This is necessary
-- when we have an enum in an expression position: we can't translate
-- @nothing@ into @{ nothing = _ }@ because the wildcard would be
-- illegal in an expression.
mkWild :: Type -> TcPat
mkWild ty
  | Record [] <- derefType ty = RTS.Tuple []
  | otherwise = RTS.Ref (MatchWild ty)

patTypeError :: (Pretty pat, Pretty ty) => pat -> ty -> T a
patTypeError = patTypeErrorDesc "type error in pattern"

patTypeErrorDesc :: (Pretty pat, Pretty ty) => Text -> pat -> ty -> T a
patTypeErrorDesc desc q ty = prettyError $
  nest 4 $ vcat
    [ pretty desc
    , "pattern: " <> pretty q
    , "expected type: " <> pretty ty
    ]

data TcMode = TcModeQuery | TcModePredicate
  deriving Eq

data TypecheckState = TypecheckState
  { tcEnv :: TcEnv
  , tcAngleVersion :: AngleVersion
  , tcNameResolutionPolicy :: NameResolutionPolicy
  , tcNextVar :: Int
  , tcScope :: HashMap Name Var
    -- ^ Variables that we have types for, and have allocated a Var
  , tcVisible :: HashSet Name
    -- ^ Variables that are currently visible
  , tcFree :: HashSet Name
    -- ^ Variables that are mentioned only once
  , tcUses :: HashSet Name
    -- ^ Accumulates variables that appear in an ContextExpr context
  , tcBindings :: HashSet Name
    -- ^ Accumulates variables that appear in an ContextPat context
  , tcMode :: TcMode
  }

-- | Name resolution policy
data NameResolutionPolicy
  = UseScope Scope (Schema.Type -> Maybe Type)
    -- ^ Use a Scope mapping names to targets, and unversioned names are
    -- ambiguous unless there is exactly one version in scope.
  | Qualified DbSchema
    -- ^ Predicates and type names must be qualified by the schema
    -- name, and unversioned references map to the latest
    -- version. This policy is used when typechecking queries, where
    -- we currently have no means to establish a Scope.


toScope :: NameResolutionPolicy -> Scope
toScope (UseScope scope _) = scope
toScope (Qualified dbSchema) = lookup
  where
  lookup ref@(SourceRef name maybeVer)
    | Just details <- lookupPredicate name maybeVer dbSchema =
      ResolvesTo (RefPred (predicateRef details))
    | Just details <- lookupType ref dbSchema =
      ResolvesTo (RefType (typeRef details))
    | otherwise =
      OutOfScope


initialTypecheckState
  :: TcEnv
  -> AngleVersion
  -> NameResolutionPolicy
  -> TcMode
  -> TypecheckState
initialTypecheckState tcEnv version policy mode = TypecheckState
  { tcEnv = tcEnv
  , tcAngleVersion = version
  , tcNameResolutionPolicy = policy
  , tcNextVar = 0
  , tcScope = HashMap.empty
  , tcVisible = HashSet.empty
  , tcFree = HashSet.empty
  , tcUses = HashSet.empty
  , tcBindings = HashSet.empty
  , tcMode = mode
  }

type T a = StateT TypecheckState (Except Text) a

bindOrUse :: Context -> Name -> TypecheckState -> TypecheckState
bindOrUse ContextExpr name state =
  state { tcUses = HashSet.insert name (tcUses state) }
bindOrUse ContextPat name state =
  state { tcBindings = HashSet.insert name (tcBindings state) }

inferVar :: Context -> Name -> T (TcPat, Type)
inferVar ctx name = do
  checkVarCase name
  state@TypecheckState{..} <- get
  case HashMap.lookup name tcScope of
    Just v@(Var ty _ _) -> do
      put $ bindOrUse ctx name $ state { tcFree = HashSet.delete name tcFree }
      return (Ref (MatchVar v), ty)
    Nothing -> prettyError $ nest 4 $ vcat
      [ "variable has unknown type: " <> pretty name
      , "Perhaps you mistyped the variable name?"
      , "If not, try adding a type annotation like: (" <> pretty name <> " : T)"
      , "or reverse the statement (Q = P instead of P = Q)"
      ]

varOcc :: Context -> Name -> Type -> T TcPat
varOcc ctx name ty = do
  checkVarCase name
  state@TypecheckState{..} <- get
  case HashMap.lookup name tcScope of
    Nothing -> do
      let
        var = Var ty tcNextVar (Just name)
        !next = tcNextVar + 1
      put $ bindOrUse ctx name $ state
        { tcNextVar = next
        , tcScope = HashMap.insert name var tcScope
        , tcFree = HashSet.insert name tcFree }
      return (RTS.Ref (MatchBind var))
    Just v@(Var ty' _ _)
      | ty' `eqType` ty -> do
        put $ bindOrUse ctx name $
          state { tcFree = HashSet.delete name tcFree }
        return (Ref (MatchVar v))
      | otherwise -> prettyError $
        nest 4 $ vcat
          [ "type mismatch for variable " <> pretty name
          , "type of variable: " <> pretty ty'
          , "expected type: " <> pretty ty
          ]

freeVariablesAreErrors :: T ()
freeVariablesAreErrors = do
  free <- gets tcFree
  when (not (HashSet.null free)) $
    prettyError $ nest 4 $ vcat
      [ "One or more variables were mentioned only once: " <>
          hsep (punctuate "," (map pretty (HashSet.toList free)))
      , "This is usually a mistake, so it is disallowed in Angle."
      ]

unboundVariablesAreErrors :: T ()
unboundVariablesAreErrors = do
  TypecheckState{..} <- get
  when (varBinding tcAngleVersion) $
    unboundVariablesAreErrors_ tcUses tcBindings

unboundVariablesAreErrors_ :: VarSet -> VarSet -> T ()
unboundVariablesAreErrors_ uses binds = do
  case HashSet.toList (uses `HashSet.difference` binds) of
    [] -> return ()
    badGuys -> prettyError $ nest 4 $ vcat
      [ "One or more variables were not bound anywhere: " <>
          hsep (punctuate "," (map pretty badGuys))
      , "All variables must occur at least once in a context that will bind"
      , "the variable to a value; i.e., *not*"
      , "  * the head of a derived predicate"
      , "  * the argument of a primitive"
      , "  * the array in an array generator Arr[..]"
      , "A variable that is bound in just one side of '|'"
      , "cannot be used outside of the '|'"
      ]

checkVarCase :: Name -> T ()
checkVarCase name
  | Just (h,_) <- Text.uncons name, not (isUpper h) = do
    v <- gets tcAngleVersion
    when (caseRestriction v) $
      prettyError $ "variable does not begin with an upper-case letter: " <>
        pretty name
  | otherwise = return ()

prettyError :: Doc ann -> T a
prettyError doc = throwError $ Text.pack $ show doc

oldOrPattern :: T a -> (a -> T b) -> T (a,b)
oldOrPattern ta tb = do
  a <- enclose ta
  b <- enclose (tb a)
  return (a,b)
  where
  -- | prevent variables defined in a scope from escaping
  enclose :: T a -> T a
  enclose rn = do
    TypecheckState{..} <- get
    r <- rn
    modify $ \state -> state { tcScope = tcScope }
    return r

-- | Typechecking A|B
--
-- 1. The set of variables that are considered to be *bound* by this
--    pattern are those that are bound in both branches.
--
-- 2. The set of variables that are considered to be *used* by this
--    pattern are those that are used in either branch.
--
orPattern :: SourcePat -> T a -> SourcePat -> (a -> T b) -> T (a,b)
orPattern pata ta patb tb = do
  v <- gets tcAngleVersion
  if not (varBinding v) then oldOrPattern ta tb else do
  state0 <- get
  (a, usesA, bindsA) <- oneBranch pata ta
  (b, usesB, bindsB) <- oneBranch patb (tb a)
  modify  $ \s -> s {
    tcBindings = tcBindings state0 `HashSet.union` -- Note (1) above
      (bindsA `HashSet.intersection` bindsB),
    tcUses = tcUses state0 `HashSet.union` -- Note (2) above
      (usesA `HashSet.union` usesB) }
  return (a,b)

-- | Typechecking either A or B in A|B
--
-- 1. Variables that are not visible in the enclosing scope are local
--    to the inner scope, we therefore remove them from tcScope after
--    typechecking the branch.
--
-- 2. Local variables must be bound locally, or we report an error.
--
-- 3. To determine what is local to any nested A|B subterms, we update
--    tcVisible by finding the visible variables of the current pattern.
--
oneBranch :: SourcePat -> T a -> T (a, VarSet, VarSet)
oneBranch pat ta = do
  state0 <- get
  let visible = tcVisible state0
  modify $ \s -> s {
    tcUses = HashSet.empty,
    tcBindings = HashSet.empty,
    tcVisible = tcVisible state0 `HashSet.union` varsPat pat mempty }
      -- See Note (3) above
  a <- ta
  stateA <- get
  let
    localUses = HashSet.difference (tcUses stateA) visible
    localBinds = HashSet.difference (tcBindings stateA) visible
  unboundVariablesAreErrors_ localUses localBinds
  modify $ \s -> s
    { tcScope = HashMap.intersection (tcScope stateA) (HashSet.toMap visible)
       -- See Note (1) above
    , tcVisible = tcVisible state0 }
  let
    extUses = HashSet.intersection (tcUses stateA) visible
    extBinds = HashSet.intersection (tcBindings stateA) visible
  return (a, extUses, extBinds)


data PrimArgType
  -- | Check a primitive operation argument matches.
  = Check Type
  -- | First infer the type using `inferExpr` and then check. The string is a
  -- comment on what the function is checking for debugging purposes.
  | InferAndCheck (Type -> Bool) String

primInferAndCheck :: [SourcePat] -> PrimOp -> [PrimArgType] -> T [TcPat]
primInferAndCheck args primOp argTys = do
  when (length args /= length argTys) $
    primInferAndCheckError primOp $ "expected " ++ show (length argTys)
      ++ " arguments, found " ++ show (length args)
  zipWithM (checkArg primOp) args argTys
  where
    checkArg :: PrimOp -> SourcePat -> PrimArgType -> T TcPat
    checkArg _ arg (Check argTy) = typecheckPattern ContextExpr argTy arg
    checkArg primOp arg (InferAndCheck argCheck debugString) = do
      (arg', argTy) <- inferExpr ContextExpr arg
      unless (argCheck argTy) $ primInferAndCheckError primOp debugString
      return arg'

primInferAndCheckError :: PrimOp -> String -> T b
primInferAndCheckError primOp debugString = prettyError $ nest 4 $ vcat
  [ "primitive operation " <> pretty primOp
    <> " does not pass associated check: "
  , pretty debugString
  ]

primitives :: HashMap Text (PrimOp, [PrimArgType], Type)
primitives = HashMap.fromList
  [ ("prim.toLower"
    , (PrimOpToLower
      , [Check Schema.String]
      , Schema.String
      )
    )
  , ("prim.length"
    , (PrimOpLength
      , [InferAndCheck polyArray "prim.length takes an array as input"]
      , Schema.Nat
      )
    )
  , ("prim.relToAbsByteSpans"
    , (PrimOpRelToAbsByteSpans
      -- prim.relToAbsByteSpans takes an array of pairs as input and
      -- returns an array of pairs as output
      , [Check (Schema.Array (tupleSchema [Schema.Nat, Schema.Nat]))]
      , Schema.Array (tupleSchema [Schema.Nat, Schema.Nat])
      )
    )
  , ("prim.gtNat", binaryNatOp PrimOpGtNat)
  , ("prim.geNat", binaryNatOp PrimOpGeNat)
  , ("prim.ltNat", binaryNatOp PrimOpLtNat)
  , ("prim.leNat", binaryNatOp PrimOpLeNat)
  , ("prim.neNat", binaryNatOp PrimOpNeNat)
  , ("prim.addNat", binaryNatArith PrimOpAddNat)
  ]
  where
    polyArray (Schema.Array _) = True
    polyArray _ = False

    binaryNatOp op = (op, [Check Schema.Nat, Check Schema.Nat], unit)
    binaryNatArith op = (op, [Check Schema.Nat, Check Schema.Nat], Schema.Nat)

type VarSet = HashSet Name

varsPat :: SourcePat -> VarSet -> VarSet
varsPat pat r = case pat of
  Variable v -> HashSet.insert v r
  Parser.Array ps -> foldr varsPat r ps
  Parser.Tuple ps -> foldr varsPat r ps
  Struct fs -> foldr (\(Field _ p) r -> varsPat p r) r fs
  App f ps -> varsPat f $! foldr varsPat r ps
  KeyValue k v -> varsPat k (varsPat v r)
  ElementsOfArray p -> varsPat p r
  OrPattern{} -> r -- ignore nested or-patterns
  NestedQuery q -> varsQuery q r
  TypeSignature p _ -> varsPat p r
  Parser.Nat{} -> r
  Parser.String{} -> r
  StringPrefix{} -> r
  Parser.ByteArray{} -> r
  Wildcard -> r
  FactId{} -> r

varsQuery :: SourceQuery -> VarSet -> VarSet
varsQuery (SourceQuery head stmts) r =
  foldr varsStmt (foldr varsPat r head) stmts
  where
  varsStmt (SourceStatement a b) r = varsPat a $! varsPat b r

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Typecheck
  ( typecheck
  , typecheckDeriving
  , ToRtsType
  , TcEnv(..)
  , emptyTcEnv
  , tcQueryDeps
  , tcQueryUsesNegation
  , UseOfNegation(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State
import Data.Bifoldable
import Data.Char
import Data.Foldable (toList)
import Data.List.Extra (firstJust)
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import Glean.Angle.Types hiding (Type)
import qualified Glean.Angle.Types as Schema
import Glean.Display
import Glean.Query.Codegen.Types
  (Match(..), Var(..), QueryWithInfo(..), Typed(..))
import Glean.Query.Typecheck.Types
import Glean.RTS.Types as RTS
import Glean.RTS.Term hiding
  (Tuple, ByteArray, String, Array, Nat, Set, Wildcard, Variable, Match(..))
import qualified Glean.RTS.Term as RTS
import Glean.Database.Schema.Types
import Glean.Schema.Util

data TcEnv = TcEnv
  { tcEnvTypes :: HashMap TypeId TypeDetails
  , tcEnvPredicates :: HashMap PredicateId PredicateDetails
  }

emptyTcEnv :: TcEnv
emptyTcEnv = TcEnv HashMap.empty HashMap.empty

type ToRtsType = Schema.Type -> Maybe Type

type Pat' s = SourcePat_ s PredicateId TypeId
type Statement' s = SourceStatement_ s PredicateId TypeId
type Query' s = SourceQuery_ s PredicateId TypeId
type DerivingInfo' s = DerivingInfo (Query' s)

-- | Typecheck a 'SourceQuery' which is in terms of schema types and
-- turn it into a 'TypecheckedQuery' which is in terms of raw Terms,
-- ready for compiling to bytecode.
typecheck
  :: IsSrcSpan s
  => DbSchema
  -> AngleVersion
  -> ToRtsType
  -> Query' s
  -> Except Text TypecheckedQuery
typecheck dbSchema ver rtsType query = do
  let
    tcEnv = TcEnv
      { tcEnvPredicates = predicatesById dbSchema
      , tcEnvTypes = typesById dbSchema
      }
  (q@(TcQuery ty _ _ _), TypecheckState{..}) <-
    let state = initialTypecheckState tcEnv ver rtsType TcModeQuery in
    flip runStateT state $ do
      modify $ \s -> s { tcVisible = varsQuery query mempty }
      inferQuery ContextExpr query
        <* freeVariablesAreErrors <* unboundVariablesAreErrors
  return (QueryWithInfo q tcNextVar ty)

-- | Typecheck the query for a derived predicate
typecheckDeriving
  :: IsSrcSpan s
  => TcEnv
  -> AngleVersion
  -> ToRtsType
  -> PredicateDetails
  -> DerivingInfo' s
  -> Except Text (DerivingInfo TypecheckedQuery)
typecheckDeriving tcEnv ver rtsType PredicateDetails{..} derivingInfo = do
  (d, _) <-
    let state = initialTypecheckState tcEnv ver rtsType TcModePredicate
    in
    flip runStateT state $ do
    flip catchError
      (\e -> throwError $ "In " <>
        Text.pack (show (pretty (predicateIdRef predicateId))) <>
          ":\n  " <> e) $ do
      case derivingInfo of
        NoDeriving -> return NoDeriving
        Derive deriveWhen q -> do
          modify $ \s -> s { tcVisible = varsQuery q mempty }
          -- we typecheck the pattern first, because we know its type.
          (head, stmts) <- needsResult q
          let
            (key, maybeVal) = case head of
              KeyValue _ key val -> (key, Just val)
                -- Backwards compat, we had a predicate in schema v4 of the form
                --   X -> prim.toLower X
                -- but this doesn't parse if -> binds tighter than application.
              App _ (KeyValue _ key val) xs ->
                let end
                      | null xs   = val
                      | otherwise = last xs
                    span = spanBetween (sourcePatSpan val) (sourcePatSpan end)
                in
                (key, Just (App span val xs))
              _other -> (head, Nothing)
          key' <- typecheckPattern ContextExpr predicateKeyType key
          maybeVal' <- case maybeVal of
            Nothing
              | eqType ver unit predicateValueType -> return Nothing
              | otherwise -> prettyErrorIn head $ nest 4 $ vcat
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

needsResult
  :: IsSrcSpan s
  => Query' s
  -> T (Pat' s, [Statement' s])
needsResult (SourceQuery (Just p) stmts) = return (p,stmts)
needsResult q@(SourceQuery Nothing stmts) = case reverse stmts of
  (SourceStatement (Variable s v) _ : _) ->
    return (Variable s v, stmts)
  (SourceStatement Wildcard{} rhs : rstmts) ->
    return (rhs, reverse rstmts)
  (SourceStatement pat _ : _) ->
    prettyErrorIn pat =<< err
  _ ->
    prettyError =<< err
  where
    err = do
      opts <- gets tcDisplayOpts
      return $ "the last statement should be an expression: " <>
        display opts q

-- add a unit result if the pattern doesn't have a result.
ignoreResult :: IsSrcSpan s => Pat' s -> Pat' s
ignoreResult p = case p of
  OrPattern s a b -> OrPattern s (ignoreResult a) (ignoreResult b)
  IfPattern s a b c -> IfPattern s a (ignoreResult b) (ignoreResult c)
  NestedQuery s (SourceQuery Nothing stmts) ->
    NestedQuery s (SourceQuery (Just empty) stmts)
  other ->
    NestedQuery fullSpan
      (SourceQuery
        (Just empty)
        [SourceStatement (Wildcard fullSpan) other])
  where
    fullSpan = sourcePatSpan p
    startPos = mkSpan (startLoc fullSpan) (startLoc fullSpan)
    empty = TypeSignature startPos (Tuple startPos []) unit

inferQuery :: IsSrcSpan s => Context -> Query' s -> T TcQuery
inferQuery ctx q = do
  (head,stmts) <- needsResult q
  stmts' <- mapM typecheckStatement stmts
  (head', ty) <- inferExpr ctx head
  return (TcQuery ty head' Nothing stmts')

typecheckQuery
  :: IsSrcSpan s
  => Context
  -> Type
  -> Query' s
  -> T TcQuery
typecheckQuery ctx ty q = do
  (head,stmts) <- needsResult q
  head' <- typecheckPattern ctx  ty head
  stmts' <- mapM typecheckStatement stmts
  return (TcQuery ty head' Nothing stmts')

unexpectedValue :: IsSrcSpan a => Pat' a -> T b
unexpectedValue pat = prettyErrorIn pat
  "a key/value pattern (X -> Y) cannot be used here"

typecheckStatement :: IsSrcSpan s => Statement' s -> T TcStatement
typecheckStatement (SourceStatement lhs rhs0) = do
  let
    ignoreTopResult (OrPattern s a b) =
      OrPattern s (ignoreResult a) (ignoreResult b)
    ignoreTopResult (IfPattern s a b c) =
      IfPattern s a (ignoreResult b) (ignoreResult c)
    ignoreTopResult other = other
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
      | Wildcard _ <- lhs = ignoreTopResult rhs0
      | otherwise = rhs0
  (rhs', ty) <- inferExpr ContextPat rhs
  lhs' <- typecheckPattern ContextPat ty lhs
  return $ TcStatement ty lhs' rhs'

-- | The context in which we're typechecking: either an expression or
-- a pattern.
data Context = ContextExpr | ContextPat

-- For the term built by a Query, we have no local type information,
-- so we attempt to infer the type of the expression.  Therefore we
-- don't allow structs or alts. Variables must be occurrences
-- (because this is a term, not a pattern), and Wildcards are
-- disallowed.
inferExpr :: IsSrcSpan s => Context -> Pat' s -> T (TcPat, Type)
inferExpr ctx pat = case pat of
  Nat _ w -> return (RTS.Nat w, NatTy)
    -- how would we do ByteTy?
  String _ s ->
    return (RTS.String (Text.encodeUtf8 s), StringTy)
  StringPrefix _ s ->
    return
      (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) (mkWild StringTy)),
        StringTy)
  ByteArray _ b ->
    return (RTS.ByteArray b, ArrayTy ByteTy)
  (App _ (StringPrefix _ s) [pat]) -> do
    rest <- typecheckPattern ctx StringTy pat
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) rest), StringTy)
  Tuple _ ts -> do
    (ts,tys) <- unzip <$> mapM (inferExpr ctx) ts
    return (RTS.Tuple ts, tupleSchema tys)
  Array _ [] ->
    return (RTS.Array [], ArrayTy (RecordTy []))
  Array _ (t:ts) -> do
    (t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    return (RTS.Array (t':ts'), ArrayTy ty)
  ArrayPrefix _ (t:|ts) -> do
    (t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    let tcPat = RTS.Ref (MatchArrayPrefix ty (t':ts'))
    return (tcPat, ArrayTy ty)
  Variable span name
    | name == "false" -> return (falseVal, BooleanTy)
    | name == "true" -> return (trueVal, BooleanTy)
    | name /= "nothing" -> inferVar ctx span name
      -- "nothing" by itself can't be inferred, we want to fall
      -- through to the type error message.
  Prim span primOp args -> do
    let (primArgTys, mkRetTy) = primOpType primOp
    (args', argTys) <- primInferAndCheck span args primOp primArgTys
    let retTy = mkRetTy argTys
    return
      ( RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args')))
      , retTy )
  Clause _ pred pat range -> tcFactGenerator pred pat range
  OrPattern _ a b -> do
    ((a', ty), b') <-
      disjunction
        (varsPat a mempty) (inferExpr ctx a)
        (varsPat b mempty) (\(_,ty) -> typecheckPattern ctx ty b)
    return (Ref (MatchExt (Typed ty (TcOr a' b'))), ty)
  NestedQuery _ q -> do
    q@(TcQuery ty _ _ _) <- inferQuery ctx q
    return (Ref (MatchExt (Typed ty (TcQueryGen q))), ty)
  Negation _ _ ->
    (,unit) <$> typecheckPattern ctx unit pat
  IfPattern _ srcCond srcThen srcElse -> do
    let tcThen = do
          (cond, condTy) <- inferExpr ctx (ignoreResult srcCond)
          (then_, thenTy) <- inferExpr ctx srcThen
          return (Typed condTy cond, then_, thenTy)

    ((cond, then_, ty), else_) <- disjunction
      (varsPat srcCond $ varsPat srcThen mempty)
      tcThen
      (varsPat srcElse mempty)
      (\(_,_, thenTy) -> typecheckPattern ctx thenTy srcElse)

    return (Ref (MatchExt (Typed ty (TcIf{..}))), ty)

  ElementsOfArray _ e -> do
    (e', ty) <- inferExpr ContextExpr e
    case ty of
      (ArrayTy elemTy) ->
        return (Ref (MatchExt (Typed elemTy (TcElementsOfArray e'))), elemTy)
      _other -> do
        opts <- gets tcDisplayOpts
        prettyErrorIn pat $
          nest 4 $ vcat
            [ "type error in array element generator:"
            , "expression: " <> display opts e
            , "does not have an array type"
            ]
  All _ e -> do
    (e', ty) <- inferExpr ContextExpr e
    case ty of
      SetTy elemTy ->
        return (Ref (MatchExt (Typed ty (TcAll e'))), elemTy)
      _other -> do
        opts <- gets tcDisplayOpts
        prettyErrorIn pat $
          nest 4 $ vcat
            [ "type error in all:"
            , "expression: " <> display opts e
            , "does not have a set type"
            ]
  Set _ [] ->
      return (RTS.Set [], SetTy (RecordTy []))
  Set _ (t:ts) -> do
    (_t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    return (RTS.Set ts', SetTy ty)
  -- we can infer { just = E } as a maybe:
  Struct _ [ Field "just" e ] -> do
    (e', ty) <- inferExpr ctx e
    return (RTS.Alt 1 e', MaybeTy ty)
  TypeSignature s e ty -> do
    rtsType <- gets tcRtsType
    typ <- convertType s rtsType ty
    (,typ) <$> typecheckPattern ctx typ e

  v@KeyValue{} -> unexpectedValue v

  _ -> do
    opts <- gets tcDisplayOpts
    prettyErrorIn pat $ nest 4 $ vcat
      [ "can't infer the type of: " <> display opts pat
      , "try adding a type annotation like (" <> display opts pat <> " : T)"
      , "or reverse the statement (Q = P instead of P = Q)"
      ]

convertType
  :: IsSrcSpan s => s -> ToRtsType -> Schema.Type -> T Type
convertType span rtsType ty = do
  case rtsType ty of
    Just typ -> return typ
    Nothing -> prettyErrorAt span "cannot convert type"

-- | Check that the pattern has the correct type, and generate the
-- low-level pattern with type-annotated variables.
typecheckPattern :: IsSrcSpan s => Context -> Type -> Pat' s -> T TcPat
typecheckPattern ctx typ pat = case (typ, pat) of
  (ByteTy, Nat _ w) -> return (RTS.Byte (fromIntegral w))
  (NatTy, Nat _ w) -> return (RTS.Nat w)
  (StringTy, String _ s) ->
    return (RTS.String (Text.encodeUtf8 s))
  (StringTy, StringPrefix _ s) ->
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) (mkWild StringTy)))
  (StringTy, App _ (StringPrefix _ s) [pat]) -> do
    pat' <- typecheckPattern ctx StringTy pat
    return (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) pat'))
  (ArrayTy ByteTy, String _ s) ->
    return (RTS.ByteArray (Text.encodeUtf8 s))
  (ArrayTy ByteTy, ByteArray _ s) ->
    return (RTS.ByteArray s)
  (ArrayTy elemTy, Array _ pats) ->
    RTS.Array <$> mapM (typecheckPattern ctx elemTy) pats
  (ArrayTy elemTy, ArrayPrefix _ pre) -> do
    pre <- mapM (typecheckPattern ctx elemTy) pre
    let match = MatchArrayPrefix elemTy (toList pre)
    return (RTS.Ref match)
  (RecordTy fields, Tuple _ pats) | length fields == length pats ->
    RTS.Tuple <$>
      mapM (\(t,p) -> typecheckPattern ctx t p)
        (zip (map fieldDefType fields) pats)
  (RecordTy fieldSchema, Struct _ fields)
    | all (`elem` map fieldDefName fieldSchema) (map fieldName fields) ->
      RTS.Tuple <$> mapM doField fieldSchema
    where
      fieldName (Field name _) = name
      doField (FieldDef name ty) =
        case [ pat | Field name' pat <- fields, name == name' ] of
          (pat:_) -> typecheckPattern ctx ty pat
          [] -> return (mkWild ty)
        -- missing field is a wildcard

  (SumTy fields, Struct _ [Field fieldName pat]) ->
    case lookupField fieldName fields of
      (ty, n) :_ -> RTS.Alt n <$> typecheckPattern ctx ty pat
      _ -> patTypeErrorDesc ("unknown alt: " <> fieldName) pat typ
  -- 'field' is shorthand for '{ field = _ }' when matching a sum type
  (SumTy fields, Variable _ fieldName)
    | ((ty, n):_)  <- lookupField fieldName fields ->
      return (RTS.Alt n (mkWild ty))
  (SumTy _, Struct _ _) ->
    patTypeErrorDesc
      "matching on a sum type should have the form { field = pattern }"
      pat typ
  (NamedTy (ExpandedType _ ty), term) -> typecheckPattern ctx ty term
  (ty, Prim span primOp args) -> do
    ver <- gets tcAngleVersion
    let (primArgTys, mkRetTy) = primOpType primOp
    (args', argTys) <- primInferAndCheck span args primOp primArgTys
    let retTy = mkRetTy argTys
    unless (eqType ver ty retTy) $
      patTypeError pat ty
    return (RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args'))))
  (PredicateTy (PidRef _ ref), Clause _ ref' arg range) ->
    if ref == ref'
      then fst <$> tcFactGenerator ref arg range
      else patTypeError pat typ
      -- Note: we don't automatically fall back to matching against
      -- the key type here, unlike in other cases where the expected
      -- type is a predicate type. Arguably it would be more correct,
      -- but doing so produces more confusing error messages because
      -- we report a type error with the key type as the expected type
      -- instead of the original type from the context.
  (MaybeTy elemTy, pat) ->
    typecheckPattern ctx (lowerMaybe elemTy) pat
  (EnumeratedTy names, Variable _ name)
    | Just n <- elemIndex name names ->
    return (RTS.Alt (fromIntegral n) (RTS.Tuple []))
  (BooleanTy, Variable _ name)
    | name == "false" -> return falseVal
    | name == "true" -> return trueVal

  (ty, ElementsOfArray _ pat) -> do
    pat' <- typecheckPattern ContextExpr (ArrayTy ty) pat
    return (Ref (MatchExt (Typed ty (TcElementsOfArray pat'))))

  (ty, OrPattern _ left right) -> do
    (left',right') <-
      disjunction
        (varsPat left mempty) (typecheckPattern ctx ty left)
        (varsPat right mempty) (\_ -> typecheckPattern ctx ty right)
    return (Ref (MatchExt (Typed ty (TcOr left' right'))))

  (ty, IfPattern _ srcCond srcThen srcElse) -> do
    let tcThen = do
          (cond,condTy) <- inferExpr ctx (ignoreResult srcCond)
          then_ <- typecheckPattern ctx ty srcThen
          return (Typed condTy cond, then_)

    ((cond, then_), else_) <- disjunction
      (varsPat srcCond $ varsPat srcThen mempty) tcThen
      (varsPat srcElse mempty) (\_ -> typecheckPattern ctx ty srcElse)

    return (Ref (MatchExt (Typed ty (TcIf cond then_ else_))))

  (ty, NestedQuery _ query) ->
    Ref . MatchExt . Typed ty . TcQueryGen <$> typecheckQuery ctx ty query

  (ty, Negation s pat) | ty == unit -> do
    let startPos = mkSpan (startLoc s) (startLoc s)
        empty = Tuple startPos []
        stmts = case pat of
          NestedQuery _ (SourceQuery Nothing stmts) -> stmts
          other -> [SourceStatement (Wildcard s) other]

        -- A negated pattern must always have type {}.
        query = SourceQuery (Just empty) stmts

        -- Variables bound within a negated query are
        -- not considered bound outside of it.
        enclose tc = do
          before <- get
          modify $ \s -> s {
            tcVisible = varsPat pat (tcVisible before)
          }
          res <- tc
          modify $ \after -> after
            { tcBindings = tcBindings before
            , tcUses = tcUses after `HashSet.intersection` tcVisible before
            , tcScope = tcScope after
                `HashMap.intersection` HashSet.toMap (tcVisible before)
            , tcVisible = tcVisible before
            }
          return res

    TcQuery _ _ _ stmts <- enclose $ typecheckQuery ctx unit query
    return $ Ref (MatchExt (Typed unit (TcNegation stmts)))

  (PredicateTy _, FactId _ Nothing fid) -> do
    isFactIdAllowed pat
    return $ Ref (MatchFid (Fid (fromIntegral fid)))

  (ty, TypeSignature s e sigty) -> do
    rtsType <- gets tcRtsType
    ver <- gets tcAngleVersion
    sigty' <- convertType s rtsType sigty
    if eqType ver ty sigty'
      then typecheckPattern ctx ty e
      else
        -- Try compiling as a fact generator that matches the key type.
        -- e.g. if the field f has type pred where pred : key, then
        -- { f = X : key } can be used to force the pattern X to
        -- match the key type instead of the fact ID.
        case ty of
          PredicateTy (PidRef _ ref) -> do
            fst <$> tcFactGenerator ref pat SeekOnAllFacts
          _ -> patTypeError pat ty

  -- A match on a predicate type with a pattern that is not a wildcard
  -- or a variable does a nested match on the key of the predicate:
  (PredicateTy (PidRef _ ref), pat) | not (isVar pat) ->
    fst <$> tcFactGenerator ref pat SeekOnAllFacts
  (SetTy elemTy, Set _ pats) -> do
    error "Set" <$> mapM (typecheckPattern ctx elemTy) pats
  (ty, All _ pat) ->
    error "Set" <$> typecheckPattern ctx (SetTy ty) pat
  (ty, Wildcard{}) -> return (mkWild ty)
  (ty, Never{}) -> return $ Ref (MatchNever ty)
  (ty, Variable span name) -> varOcc ctx span name ty

  (_, KeyValue{}) -> unexpectedValue pat

  -- type annotations are unnecessary, but we allow and check them
  (ty, q) -> patTypeError q ty
  where

  lookupField fieldName fields =
    [ (ty, n) | (FieldDef name ty, n) <- zip fields [0..]
    , name == fieldName ]

tcFactGenerator
  :: IsSrcSpan s
  => PredicateId
  -> Pat' s
  -> SeekSection
  -> T (TcPat, Type)
tcFactGenerator ref pat range = do
  TcEnv{..} <- gets tcEnv
  PredicateDetails{..} <- case HashMap.lookup ref tcEnvPredicates of
    Nothing -> prettyErrorIn pat $ "tcFactGenerator: " <> displayDefault ref
    Just details -> return details
  (kpat', vpat') <- case pat of
    KeyValue _ kpat vpat -> do
      kpat' <- typecheckPattern ContextPat predicateKeyType kpat
      vpat' <- typecheckPattern ContextPat predicateValueType vpat
      return (kpat', vpat')
    _other -> do
      kpat' <- typecheckPattern ContextPat predicateKeyType pat
      return (kpat', mkWild predicateValueType)
  let
    pidRef = (PidRef predicatePid ref)
    ty = PredicateTy pidRef
  return
    ( Ref (MatchExt (Typed ty (TcFactGen pidRef kpat' vpat' range)))
    , ty)

isVar :: IsSrcSpan s => Pat' s -> Bool
isVar Wildcard{} = True
isVar Variable{} = True
isVar _ = False

-- | Fact Id patterns (#1234 or #pred 1234) are only allowed in
-- queries, not in derived predicates, because they potentially allow
-- a type-incorrect fact to be constructed.
isFactIdAllowed :: IsSrcSpan s => Pat' s -> T ()
isFactIdAllowed pat = do
  mode <- gets tcMode
  opts <- gets tcDisplayOpts
  when (mode /= TcModeQuery) $ prettyErrorIn pat $
    "fact IDs are not allowed in a derived predicate: " <>
      display opts pat

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
  | RecordTy [] <- derefType ty = RTS.Tuple []
  | otherwise = RTS.Ref (MatchWild ty)

patTypeError :: (IsSrcSpan s) => Pat' s -> Type -> T a
patTypeError = patTypeErrorDesc "type error in pattern"

patTypeErrorDesc :: (IsSrcSpan s) => Text -> Pat' s -> Type -> T a
patTypeErrorDesc desc q ty = do
  opts <- gets tcDisplayOpts
  prettyErrorIn q $
    nest 4 $ vcat
      [ pretty desc
      , "pattern: " <> display opts q
      , "expected type: " <> display opts ty
      ]

data TcMode = TcModeQuery | TcModePredicate
  deriving Eq

data TypecheckState = TypecheckState
  { tcEnv :: TcEnv
  , tcAngleVersion :: AngleVersion
  , tcRtsType :: ToRtsType
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
  , tcDisplayOpts :: DisplayOpts
    -- ^ Options for pretty-printing
  }

initialTypecheckState
  :: TcEnv
  -> AngleVersion
  -> ToRtsType
  -> TcMode
  -> TypecheckState
initialTypecheckState tcEnv version rtsType mode = TypecheckState
  { tcEnv = tcEnv
  , tcAngleVersion = version
  , tcRtsType = rtsType
  , tcNextVar = 0
  , tcScope = HashMap.empty
  , tcVisible = HashSet.empty
  , tcFree = HashSet.empty
  , tcUses = HashSet.empty
  , tcBindings = HashSet.empty
  , tcMode = mode
  , tcDisplayOpts = defaultDisplayOpts
      -- might make this configurable with flags later
  }

type T a = StateT TypecheckState (Except Text) a

bindOrUse :: Context -> Name -> TypecheckState -> TypecheckState
bindOrUse ContextExpr name state =
  state { tcUses = HashSet.insert name (tcUses state) }
bindOrUse ContextPat name state =
  state { tcBindings = HashSet.insert name (tcBindings state) }

inferVar :: IsSrcSpan span => Context -> span -> Name -> T (TcPat, Type)
inferVar ctx span name = do
  checkVarCase span name
  state@TypecheckState{..} <- get
  case HashMap.lookup name tcScope of
    Just v@(Var ty _ _) -> do
      put $ bindOrUse ctx name $ state { tcFree = HashSet.delete name tcFree }
      return (Ref (MatchVar v), ty)
    Nothing -> prettyErrorAt span $ nest 4 $ vcat
      [ "variable has unknown type: " <> pretty name
      , "Perhaps you mistyped the variable name?"
      , "If not, try adding a type annotation like: (" <> pretty name <> " : T)"
      , "or reverse the statement (Q = P instead of P = Q)"
      ]

varOcc :: IsSrcSpan span => Context -> span -> Name -> Type -> T TcPat
varOcc ctx span name ty = do
  checkVarCase span name
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
      | eqType tcAngleVersion ty' ty -> do
        put $ bindOrUse ctx name $
          state { tcFree = HashSet.delete name tcFree }
        return (Ref (MatchVar v))
      | otherwise -> do
        prettyErrorAt span $
          nest 4 $ vcat
            [ "type mismatch for variable " <> pretty name
            , "type of variable: " <> display tcDisplayOpts ty'
            , "expected type: " <> display tcDisplayOpts ty
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
       <+> "cannot be used outside of the '|'"
      , "A variable that is bound in a negated query '!(...)'"
       <+> "cannot be used outside of it"
      ]

checkVarCase :: IsSrcSpan span => span -> Name -> T ()
checkVarCase span name
  | Just (h,_) <- Text.uncons name, not (isUpper h) = do
    prettyErrorAt span $
      "variable does not begin with an upper-case letter: " <>
        pretty name
  | otherwise = return ()

prettyError :: Doc ann -> T a
prettyError = throwError . Text.pack . show

prettyErrorIn :: IsSrcSpan s => Pat' s -> Doc ann -> T a
prettyErrorIn pat doc = prettyErrorAt (sourcePatSpan pat) doc

prettyErrorAt :: IsSrcSpan span => span -> Doc ann -> T a
prettyErrorAt span doc = prettyError $ vcat
  [ pretty span
  , doc
  ]

-- | Typechecking A|B
--
-- 1. The set of variables that are considered to be *bound* by this
--    pattern are those that are bound in both branches.
--
-- 2. The set of variables that are considered to be *used* by this
--    pattern are those that are used in either branch.
--
disjunction :: VarSet -> T a -> VarSet -> (a -> T b) -> T (a,b)
disjunction varsA ta varsB tb = do
  state0 <- get
  (a, usesA, bindsA) <- oneBranch varsA ta
  (b, usesB, bindsB) <- oneBranch varsB (tb a)
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
oneBranch :: VarSet -> T a -> T (a, VarSet, VarSet)
oneBranch branchVars ta = do
  visibleBefore  <- gets tcVisible
  modify $ \s -> s {
    tcUses = HashSet.empty,
    tcBindings = HashSet.empty,
    tcVisible = visibleBefore `HashSet.union` branchVars }
      -- See Note (3) above
  a <- ta
  after <- get
  let
    localUses = HashSet.difference (tcUses after) visibleBefore
    localBinds = HashSet.difference (tcBindings after) visibleBefore
  unboundVariablesAreErrors_ localUses localBinds
  modify $ \s -> s
    { tcScope = HashMap.intersection
        (tcScope after)
        (HashSet.toMap visibleBefore)
       -- See Note (1) above
    , tcVisible = visibleBefore }
  let
    extUses = HashSet.intersection (tcUses after) visibleBefore
    extBinds = HashSet.intersection (tcBindings after) visibleBefore
  return (a, extUses, extBinds)

data PrimArgType
  -- | Check a primitive operation argument matches.
  = Check Type
  -- | First infer the type using `inferExpr` and then check. The string is a
  -- comment on what the function is checking for debugging purposes.
  | InferAndCheck (Type -> Bool) String
  -- | Check that all arguments are of the same type.
  | CheckAllEqual

primInferAndCheck
  :: IsSrcSpan s
  => s
  -> [Pat' s]
  -> PrimOp
  -> [PrimArgType]
  -> T ([TcPat], [Type])
primInferAndCheck span args primOp argTys =
  unzip . reverse <$> checkArgs [] args argTys
  where
    checkArgs :: IsSrcSpan s =>
      [(TcPat, Type)] -> [Pat' s] -> [PrimArgType] -> T [(TcPat, Type)]
    checkArgs acc [] [] = return acc
    checkArgs acc (arg:args) (check:pats) = do
      let prevArgTy = snd <$> listToMaybe acc
      (arg', argTy) <- checkArg prevArgTy arg check
      let acc' = (arg', argTy) : acc
      checkArgs acc' args pats
    checkArgs _ _ _ =
        primInferAndCheckError span primOp
          $ "expected " ++ show (length argTys)
          ++ " arguments, found " ++ show (length args)

    checkArg :: IsSrcSpan s =>
      Maybe Type -> Pat' s -> PrimArgType -> T (TcPat, Type)
    checkArg _ arg (Check argTy) =
      (,argTy) <$> typecheckPattern ContextExpr argTy arg
    checkArg _ arg (InferAndCheck argCheck debugString) = do
      (arg', argTy) <- inferExpr ContextExpr arg
      unless (argCheck argTy) $ primInferAndCheckError span primOp debugString
      return (arg', argTy)
    checkArg prevTy arg CheckAllEqual = case prevTy of
      Nothing -> inferExpr ContextExpr arg
      Just argTy -> (,argTy) <$> typecheckPattern ContextExpr argTy arg


primInferAndCheckError :: IsSrcSpan s => s -> PrimOp -> String -> T b
primInferAndCheckError span primOp debugString = do
  opts <- gets tcDisplayOpts
  prettyErrorAt span $ nest 4 $ vcat
    [ "primitive operation " <> display opts primOp
      <> " does not pass associated check: "
    , pretty debugString
    ]

primOpType :: PrimOp -> ([PrimArgType], [Type] -> Type)
primOpType op = case op of
  PrimOpToLower -> ([Check StringTy], const StringTy)
  PrimOpLength ->
    let
      polyArray (ArrayTy _) = True
      polyArray _ = False
    in
    ( [InferAndCheck polyArray "prim.length takes an array as input"]
    , const NatTy)
  PrimOpRelToAbsByteSpans ->
    -- prim.relToAbsByteSpans takes an array of pairs as input and
    -- returns an array of pairs as output
    ( [Check (ArrayTy (tupleSchema [NatTy, NatTy]))]
    , const $ ArrayTy (tupleSchema [NatTy, NatTy]) )
  PrimOpUnpackByteSpans ->
    -- prim.unpackByteSpans takes an array of (nat, [nat]) as input and
    -- returns an array of pairs as output
    ( [Check (ArrayTy (tupleSchema [NatTy, ArrayTy NatTy]))]
    , const $ ArrayTy (tupleSchema [NatTy, NatTy]) )
  PrimOpGtNat -> binaryNatOp
  PrimOpGeNat -> binaryNatOp
  PrimOpLtNat -> binaryNatOp
  PrimOpLeNat -> binaryNatOp
  PrimOpNeNat -> binaryNatOp
  PrimOpAddNat -> ([Check NatTy, Check NatTy], const NatTy)
  PrimOpNeExpr -> ([CheckAllEqual, CheckAllEqual], const unit)
  where
    binaryNatOp = ([Check NatTy, Check NatTy], const unit)

type VarSet = HashSet Name

-- Variables visible in the pattern's enclosing scope.
--
-- 1. or-patterns, negations and if statements don't bring variables into a
--    scope. They can bind variables that are already part of the scope, but if
--    the same variable appears in all branches but not in the enclosing scope
--    then each occurrence will be treated as a separate variable.
varsPat :: IsSrcSpan s => Pat' s -> VarSet -> VarSet
varsPat pat r = case pat of
  Variable _ v -> HashSet.insert v r
  Array _ ps -> foldr varsPat r ps
  ArrayPrefix _ ps -> foldr varsPat r ps
  Tuple _ ps -> foldr varsPat r ps
  Struct _ fs -> foldr (\(Field _ p) r -> varsPat p r) r fs
  App _ f ps -> varsPat f $! foldr varsPat r ps
  KeyValue _ k v -> varsPat k (varsPat v r)
  ElementsOfArray _ p -> varsPat p r
  Set _ pats -> foldr varsPat r pats
  All _ pat -> varsPat pat r
  OrPattern{} -> r -- ignore nested or-patterns. Note (1) above
  IfPattern{} -> r -- ignore nested if-patterns
  NestedQuery _ q -> varsQuery q r
  Negation{} -> r -- Note (1) above
  TypeSignature _ p _ -> varsPat p r
  Nat{} -> r
  String{} -> r
  StringPrefix{} -> r
  ByteArray{} -> r
  Wildcard{} -> r
  FactId{} -> r
  Never{} -> r
  Clause _ _ p _ -> varsPat p r
  Prim _ _ ps -> foldr varsPat r ps

varsQuery :: IsSrcSpan s => Query' s -> VarSet -> VarSet
varsQuery (SourceQuery head stmts) r =
  foldr varsStmt (foldr varsPat r head) stmts
  where
  varsStmt (SourceStatement a b) r = varsPat a $! varsPat b r

-- | Capture all predicates that appear in a query
tcQueryDeps :: TcQuery -> Set PredicateId
tcQueryDeps q = Set.fromList $ map getRef (overQuery q)
  where
    getRef (PidRef _ ref) = ref

    overQuery :: TcQuery -> [PidRef]
    overQuery (TcQuery ty key mval stmts) =
      overType ty
        <> overPat key
        <> maybe [] overPat mval
        <> foldMap overStatement stmts

    overType :: Type -> [PidRef]
    overType ty = bifoldMap pure (\(ExpandedType _ ty) -> overType ty) ty

    overStatement :: TcStatement -> [PidRef]
    overStatement (TcStatement ty lhs rhs) =
     overType ty <> overPat lhs <> overPat rhs

    overPat :: TcPat -> [PidRef]
    overPat pat = foldMap (bifoldMap overTyped (const mempty)) pat

    overTyped :: Typed TcTerm -> [PidRef]
    overTyped (Typed ty tcTerm) = overType ty <> overTerm tcTerm

    overTerm :: TcTerm -> [PidRef]
    overTerm = \case
      TcOr x y -> overPat x <> overPat y
      TcFactGen pref x y _ -> pref : overPat x <> overPat y
      TcElementsOfArray x -> overPat x
      TcAll x -> overPat x
      TcQueryGen q -> overQuery q
      TcNegation stmts -> foldMap overStatement stmts
      TcPrimCall _ xs -> foldMap overPat xs
      TcIf (Typed _ x) y z -> foldMap overPat [x, y, z]

data UseOfNegation
  = PatternNegation
  | IfStatement

-- | Whether a query uses negation in its definition.
-- Does not check for transitive uses of negation.
tcQueryUsesNegation :: TcQuery -> Maybe UseOfNegation
tcQueryUsesNegation (TcQuery _ _ _ stmts) =
  firstJust tcStatementUsesNegation stmts

tcStatementUsesNegation :: TcStatement -> Maybe UseOfNegation
tcStatementUsesNegation (TcStatement _ lhs rhs) =
  tcPatUsesNegation lhs <|> tcPatUsesNegation rhs

tcPatUsesNegation :: TcPat -> Maybe UseOfNegation
tcPatUsesNegation = \case
  RTS.Byte _ -> Nothing
  RTS.Nat _ -> Nothing
  RTS.Array xs -> firstJust tcPatUsesNegation xs
  RTS.ByteArray _ -> Nothing
  RTS.Tuple xs -> firstJust tcPatUsesNegation xs
  RTS.Set xs -> firstJust tcPatUsesNegation xs
  RTS.Alt _ t -> tcPatUsesNegation t
  RTS.String _ -> Nothing
  RTS.Ref match -> matchUsesNegation match

matchUsesNegation  :: Match (Typed TcTerm) Var -> Maybe UseOfNegation
matchUsesNegation = \case
  MatchWild _ -> Nothing
  MatchNever _ -> Nothing
  MatchFid _ -> Nothing
  MatchBind _ -> Nothing
  MatchVar _ -> Nothing
  MatchAnd one two -> tcPatUsesNegation one <|> tcPatUsesNegation two
  MatchPrefix _ x -> tcPatUsesNegation x
  MatchArrayPrefix _ty prefix -> firstJust tcPatUsesNegation prefix
  MatchExt (Typed _ tcterm) -> tcTermUsesNegation tcterm

tcTermUsesNegation  :: TcTerm -> Maybe UseOfNegation
tcTermUsesNegation = \case
  TcOr x y -> tcPatUsesNegation x <|> tcPatUsesNegation y
  TcFactGen _ x y _ -> tcPatUsesNegation x <|> tcPatUsesNegation y
  TcElementsOfArray x -> tcPatUsesNegation x
  TcAll x -> tcPatUsesNegation x
  TcQueryGen q -> tcQueryUsesNegation q
  TcNegation _ -> Just PatternNegation
  TcPrimCall _ xs -> firstJust tcPatUsesNegation xs
  -- one can replicate negation using if statements
  TcIf{} -> Just IfStatement

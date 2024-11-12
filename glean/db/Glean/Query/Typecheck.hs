{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- TODO
  - implement mutable type variables to speed up type inference
  - cleanup:
    - merge inferExpr and typecheckPattern?
-}

module Glean.Query.Typecheck
  ( typecheck
  , typecheckDeriving
  , ToRtsType
  , TcEnv(..)
  , TcOpts(..)
  , defaultTcOpts
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
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Compat.Prettyprinter hiding ((<>), enclose)
import System.IO
import TextShow

import Glean.Angle.Types hiding (Type)
import qualified Glean.Angle.Types as Schema
import Glean.Display
import Glean.Query.Codegen.Types
  (Match(..), Var(..), QueryWithInfo(..), Typed(..))
import Glean.Query.Typecheck.Types
import Glean.Query.Typecheck.Monad
import Glean.Query.Typecheck.Unify
import Glean.RTS.Types as RTS
import Glean.RTS.Term hiding
  (Tuple, ByteArray, String, Array, Nat)
import qualified Glean.RTS.Term as RTS
import Glean.Database.Schema.Types
import Glean.Schema.Util
import Glean.Util.Some

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
  -> TcOpts
  -> ToRtsType
  -> Query' s
  -> ExceptT Text IO TypecheckedQuery
typecheck dbSchema opts rtsType query = do
  let
    tcEnv = TcEnv
      { tcEnvPredicates = predicatesById dbSchema
      , tcEnvTypes = typesById dbSchema
      }
  (q@(TcQuery ty _ _ _ _), TypecheckState{..}) <-
    let state = initialTypecheckState tcEnv opts rtsType TcModeQuery in
    withExceptT (Text.pack . show) $ flip runStateT state $ do
      modify $ \s -> s { tcVisible = varsQuery query mempty }
      q@(TcQuery retTy _ _ _ _) <- inferQuery ContextPat query
        <* freeVariablesAreErrors <* unboundVariablesAreErrors
      subst <- gets tcSubst
      whenDebug $ liftIO $ hPutStrLn stderr $ show $
        vcat [
          "subst:", indent 2 (vcat
            [ pretty n <> " := " <> displayDefault ty
            | (n,ty) <- IntMap.toList subst
            ]),
          "query: " <> displayDefault q
        ]
      resolvePromote
      zonkVars
      zonkTcQuery q
        `catchError` \_ -> do
           (head,_,_) <- needsResult query
           opts <- gets tcDisplayOpts
           retTy' <- apply retTy
           prettyErrorAt (sourcePatSpan head) $ vcat
             [ "query has ambiguous type",
               indent 4 $ "type: " <> display opts retTy'
             ]
  return (QueryWithInfo q tcNextVar Nothing ty)

-- | Typecheck the query for a derived predicate
typecheckDeriving
  :: IsSrcSpan s
  => TcEnv
  -> TcOpts
  -> ToRtsType
  -> PredicateDetails
  -> DerivingInfo' s
  -> ExceptT Text IO (DerivingInfo TypecheckedQuery)
typecheckDeriving tcEnv opts rtsType PredicateDetails{..} derivingInfo = do
  (d, _) <-
    let state = initialTypecheckState tcEnv opts rtsType TcModePredicate
    in
    withExceptT (Text.pack . show) $ flip runStateT state $ do
    flip catchError
      (\e -> throwError $ vcat
        [ "In " <> pretty (predicateIdRef predicateId) <> ":"
        , indent 2 e ]) $ do
      case derivingInfo of
        NoDeriving -> return NoDeriving
        Derive deriveWhen q -> do
          modify $ \s -> s { tcVisible = varsQuery q mempty }
          -- we typecheck the pattern first, because we know its type.
          (head, stmts, ord) <- needsResult q
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
          key' <- typecheckPattern ContextPat predicateKeyType key
          maybeVal' <- case maybeVal of
            Nothing
              | eqType (tcOptAngleVersion opts) unit predicateValueType ->
                return Nothing
              | otherwise -> prettyErrorIn head $ nest 4 $ vcat
                [ "a functional predicate must return a value,"
                , "i.e. the query should have the form 'X -> Y where .." ]
            Just val -> Just <$>
              typecheckPattern ContextPat predicateValueType val
          stmts' <- mapM typecheckStatement stmts
          freeVariablesAreErrors
          unboundVariablesAreErrors
          resolvePromote
          zonkVars
          q <- zonkTcQuery (TcQuery predicateKeyType key' maybeVal' stmts' ord)
          nextVar <- gets tcNextVar
          return $ Derive deriveWhen $
            QueryWithInfo q nextVar Nothing predicateKeyType
  return d

needsResult
  :: IsSrcSpan s
  => Query' s
  -> T (Pat' s, [Statement' s], Ordered)
needsResult (SourceQuery (Just p) stmts ord) = return (p, stmts, ord)
needsResult q@(SourceQuery Nothing stmts ord) = case reverse stmts of
  (SourceStatement (Variable s v) _ : _) ->
    return (Variable s v, stmts, ord)
  [SourceStatement Wildcard{} rhs] ->
    return (rhs, [], ord)
  (SourceStatement (Wildcard s) rhs : rstmts) -> do
    tmp <- freshVariable s
    return (tmp, reverse (SourceStatement rhs tmp : rstmts), ord)
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
  NestedQuery s (SourceQuery Nothing stmts ord) ->
    NestedQuery s (SourceQuery (Just empty) stmts ord)
  other ->
    NestedQuery fullSpan
      (SourceQuery
        (Just empty)
        [SourceStatement (Wildcard fullSpan) other] Unordered)
  where
    fullSpan = sourcePatSpan p
    startPos = mkSpan (startLoc fullSpan) (startLoc fullSpan)
    empty = TypeSignature startPos (Tuple startPos []) unit

inferQuery :: IsSrcSpan s => Context -> Query' s -> T TcQuery
inferQuery ctx q = do
  (head,stmts,ord) <- needsResult q
  stmts' <- mapM typecheckStatement stmts
  (head', ty) <- inferExpr ctx head
  return (TcQuery ty head' Nothing stmts' ord)

typecheckQuery
  :: IsSrcSpan s
  => Context
  -> Type
  -> Query' s
  -> T TcQuery
typecheckQuery ctx ty q = do
  (head,stmts,ord) <- needsResult q
  head' <- typecheckPattern ctx ty head
  stmts' <- mapM typecheckStatement stmts
  return (TcQuery ty head' Nothing stmts' ord)

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
  Nat _ w ->
    promote (sourcePatSpan pat) (RTS.Nat w) NatTy
    -- how would we do ByteTy?
  String _ s ->
    promote (sourcePatSpan pat)
      (RTS.String (Text.encodeUtf8 s)) StringTy
  StringPrefix _ s ->
    promote (sourcePatSpan pat)
      (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) (mkWild StringTy)))
      StringTy
  ByteArray _ b ->
    promote (sourcePatSpan pat) (RTS.ByteArray b) (ArrayTy ByteTy)
  (App _ (StringPrefix _ s) [pat]) -> do
    rest <- typecheckPattern ctx StringTy pat
    promote (sourcePatSpan pat)
      (RTS.Ref (MatchPrefix (Text.encodeUtf8 s) rest)) StringTy
  Tuple _ ts -> do
    (ts,tys) <- unzip <$> mapM (inferExpr ctx) ts
    promote (sourcePatSpan pat) (RTS.Tuple ts) (tupleSchema tys)
  Array _ [] -> do
    x <- freshTyVar
    promote (sourcePatSpan pat) (RTS.Array []) (ArrayTy x)
  Array _ (t:ts) -> do
    (t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    promote (sourcePatSpan pat) (RTS.Array (t':ts')) (ArrayTy ty)
  ArrayPrefix _ (t:|ts) -> do
    (t',ty) <- inferExpr ctx t
    ts' <- mapM (typecheckPattern ctx ty) ts
    let
      aty = ArrayTy ty
      tcPat = RTS.Ref (MatchArrayPrefix ty (t':ts') (RTS.Ref (MatchWild aty)))
    promote (sourcePatSpan pat) tcPat aty
  Variable span name -> inferVar ctx span name
  Prim span primOp args -> do
    (primArgTys, retTy) <- primOpType primOp
    checkPrimOpArgs span args primArgTys
    args' <- zipWithM (typecheckPattern ContextExpr) primArgTys args
    return (RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args'))),
      retTy)
  Clause _ _ pred pat range -> tcFactGenerator pred pat range
  OrPattern _ a b -> do
    ((a', ty), b') <-
      disjunction
        (varsPat a mempty) (inferExpr ctx a)
        (varsPat b mempty) (\(_,ty) -> typecheckPattern ctx ty b)
    return (Ref (MatchExt (Typed ty (TcOr a' b'))), ty)
  NestedQuery _ q -> do
    q@(TcQuery ty _ _ _ _) <- inferQuery ctx q
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
    elemTy <- case ty of
      ArrayTy elemTy -> return elemTy
      _other -> do
        elemTy <- freshTyVar
        inPat pat $ unify ty (ArrayTy elemTy)
        return elemTy
    return (Ref (MatchExt (Typed elemTy (TcElementsOfArray e'))), elemTy)

  Elements _ e -> do
    (e', ty) <- inferExpr ContextExpr e
    case ty of
      SetTy elemTy ->
        return (Ref (MatchExt (Typed elemTy (TcElements e'))), elemTy)
      _other -> do
        opts <- gets tcDisplayOpts
        prettyErrorIn pat $
          nest 4 $ vcat
            [ "type error in set element generator:"
            , "expression: " <> display opts e
            , "does not have a set type"
            ]
  All _ e -> do
    (e',elementTy) <- inferExpr ctx e
    let
      q = TcQuery elementTy e' Nothing [] Unordered
      ty = SetTy elementTy
    return (Ref (MatchExt (Typed ty (TcAll q))), ty)

  TypeSignature s e ty -> do
    rtsType <- gets tcRtsType
    typ <- convertType s rtsType ty
    (,typ) <$> typecheckPattern ctx typ e

  v@KeyValue{} -> unexpectedValue v

  FieldSelect _ p field sum -> do
    (p', ty) <- inferExpr ctx p
    fieldSelect pat ty p' field sum

  Enum _ "true" -> promote (sourcePatSpan pat) trueVal BooleanTy
  Enum _ "false" -> promote (sourcePatSpan pat) falseVal BooleanTy

  -- we can infer { just = E } as a maybe:
  -- TODO: this is wrong really, but there is schema code that relies on it
  Struct _ [ Field "just" e ] -> do
    (e', ty) <- inferExpr ctx e
    return (RTS.Alt 1 e', MaybeTy ty)

  Struct _ fields -> do
    pairs <- forM fields $ \(Field name expr) -> do
      (expr', ty) <- inferExpr ctx expr
      return ((name, expr'), (name, ty))
    let (fields', types) = unzip pairs
    x <- freshTyVarInt
    let
      must_be_rec
        | length fields > 1 = Just Record
        | otherwise = Nothing
      ty = HasTy (Map.fromList types) must_be_rec x
    promote (sourcePatSpan pat)
      (Ref (MatchExt (Typed ty (TcStructPat fields')))) ty

  Wildcard{} -> do
    x <- freshTyVar
    return (mkWild x, x)

  Enum _ name -> do
    x <- freshTyVarInt
    let ty = HasTy (Map.singleton name unit) Nothing x
    promote (sourcePatSpan pat)
      (Ref (MatchExt (Typed ty (TcStructPat [(name, RTS.Tuple [])])))) ty

  _ -> do
    opts <- gets tcDisplayOpts
    prettyErrorIn pat $ nest 4 $ vcat
      [ "can't infer the type of: " <> display opts pat
      , "try adding a type annotation like (" <> display opts pat <> " : T)"
      ]

fieldSelect
  :: IsSrcSpan s
  => Pat' s
  -> Type
  -> TcPat
  -> FieldName
  -> RecordOrSum
  -> T (TcPat, Type)
fieldSelect src ty pat fieldName recordOrSum = do
  opts <- gets tcDisplayOpts
  let err x = do
        prettyErrorIn src $ nest 4 $ vcat
          [ x, "type of expression: " <> display opts ty ]
  ty' <- apply ty
  case derefType ty' of
    PredicateTy (PidRef _ ref) -> do
      TcEnv{..} <- gets tcEnv
      PredicateDetails{..} <- case HashMap.lookup ref tcEnvPredicates of
        Nothing -> prettyErrorIn src $ "fieldSelect: " <> displayDefault ref
        Just details -> return details
      let deref = TcDeref ty predicateValueType pat
      fieldSelect src predicateKeyType
        (Ref (MatchExt (Typed predicateKeyType deref)))
        fieldName recordOrSum
    RecordTy fields
      | Record <- recordOrSum -> case lookupField fieldName fields of
          (fieldTy,_):_ -> do
            let sel = TcFieldSelect (Typed ty pat) fieldName
            return (Ref (MatchExt (Typed fieldTy sel)), fieldTy)
          _ ->
            err $ "record does not contain the field '" <>
              display opts fieldName <> "'"
      | otherwise -> err $
        "expression is a record, use '." <> pretty fieldName <>
          "' not '." <> pretty fieldName <> "?'"
    SumTy fields
      | Sum <- recordOrSum -> case lookupField fieldName fields of
          (fieldTy,_):_ -> do
            let sel = TcAltSelect (Typed ty pat) fieldName
            return (Ref (MatchExt (Typed fieldTy sel)), fieldTy)
          _ ->
            err $ "union type does not contain the alternative '" <>
              display opts fieldName <> "'"
      | otherwise -> err $
        "expression is a union type, use '." <> pretty fieldName <>
          "?' not '." <> pretty fieldName <> "'"
    MaybeTy elemTy ->
      fieldSelect src (lowerMaybe elemTy) pat fieldName recordOrSum
    TyVar{} -> do
      x <- freshTyVarInt
      fieldTy <- freshTyVar
      let recTy = HasTy (Map.singleton fieldName fieldTy) (Just recordOrSum) x
      -- allow the lhs to be a predicate:
      fn <- demoteTo (sourcePatSpan src) ty' recTy
      let sel = case recordOrSum of
            Sum -> TcAltSelect (Typed recTy (fn pat)) fieldName
            Record -> TcFieldSelect (Typed recTy (fn pat)) fieldName
      return (Ref (MatchExt (Typed fieldTy sel)), fieldTy)
    _other ->
      err $ "expression is not a " <> case recordOrSum of
        Sum -> "union type"
        Record -> "record"

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
    let match = MatchArrayPrefix elemTy (toList pre) (RTS.Ref (MatchWild typ))
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
  (SumTy fields, Enum _ fieldName)
    | ((ty, n):_)  <- lookupField fieldName fields ->
      return (RTS.Alt n (mkWild ty))

  (SumTy _, Struct _ _) ->
    patTypeErrorDesc
      "matching on a union type should have the form { field = pattern }"
      pat typ
  (NamedTy (ExpandedType _ ty), term) -> typecheckPattern ctx ty term
  (ty, Prim span primOp args) -> do
    (primArgTys, retTy) <- primOpType primOp
    checkPrimOpArgs span args primArgTys
    args' <- zipWithM (typecheckPattern ContextExpr) primArgTys args
    inPat pat $ unify ty retTy
    return (RTS.Ref (MatchExt (Typed retTy (TcPrimCall primOp args'))))
  (PredicateTy (PidRef _ ref), Clause _ _ ref' arg range) ->
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

  (EnumeratedTy names, Enum _ name)
    | Just n <- elemIndex name names ->
    return (RTS.Alt (fromIntegral n) (RTS.Tuple []))

  (BooleanTy, Enum _ name)
    | name == "false" -> return falseVal
    | name == "true" -> return trueVal

  -- TODO: we don't really want to do this, but it's here for legacy
  -- reasons. Really we should only allow lower-case identifiers as
  -- enum constants.
  (EnumeratedTy names, Variable _ name)
    | Just n <- elemIndex name names ->
    return (RTS.Alt (fromIntegral n) (RTS.Tuple []))

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
        (stmts, ord) = case pat of
          NestedQuery _ (SourceQuery Nothing stmts _) -> (stmts, ord)
          other -> ([SourceStatement (Wildcard s) other], Unordered)

        -- A negated pattern must always have type {}.
        query = SourceQuery (Just empty) stmts ord

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

    TcQuery _ _ _ stmts _ <- enclose $ typecheckQuery ctx unit query
    return $ Ref (MatchExt (Typed unit (TcNegation stmts)))

  (PredicateTy _, FactId _ Nothing fid) -> do
    isFactIdAllowed pat
    return $ Ref (MatchFid (Fid (fromIntegral fid)))

  (ty, TypeSignature s e sigty) -> do
    rtsType <- gets tcRtsType
    sigty' <- convertType s rtsType sigty
    e' <- typecheckPattern ctx sigty' e
    f <- promoteTo (sourcePatSpan pat) sigty' ty
    return (f e')

  (ty, FieldSelect _ p field sum) -> do
    (p', recTy) <- inferExpr ctx p
    (pat', fieldTy) <- fieldSelect pat recTy p' field sum
    inPat pat $ unify ty fieldTy
    return pat'

  -- A match on a predicate type with a pattern that is not a wildcard
  -- or a variable does a nested match on the key of the predicate:
  (PredicateTy (PidRef _ ref), pat) | not (isVar pat) ->
    fst <$> tcFactGenerator ref pat SeekOnAllFacts
  (ty@(SetTy elemTy), All _ query) -> do
    arg <- typecheckPattern ctx elemTy query
    let q = TcQuery elemTy arg Nothing [] Unordered
    return (Ref (MatchExt (Typed ty (TcAll q))))
  (ty, Elements _ pat) -> do
    elems <- typecheckPattern ctx (SetTy ty) pat
    return (Ref (MatchExt (Typed ty (TcElements elems))))
  (ty, Wildcard{}) -> return (mkWild ty)
  (ty, Never{}) -> return $ Ref (MatchNever ty)
  (ty, Variable span name) -> varOcc ctx span name ty

  (_, KeyValue{}) -> unexpectedValue pat

  (ty, _) -> do
    (p', patTy) <- inferExpr ctx pat
    inPat pat $ unify ty patTy
    return p'

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
    Nothing -> do
      x <- freshTyVar
      p <- varOcc ctx span name x
      return (p,x)

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
        , tcFree = HashSet.insert name tcFree
        , tcVars = IntMap.insert tcNextVar var tcVars
        }
      return (RTS.Ref (MatchBind var))
    Just v@(Var ty' _ _) -> do
      put $ bindOrUse ctx name $
        state { tcFree = HashSet.delete name tcFree }
      addErrSpan span $ unify ty ty'
      return (Ref (MatchVar v))

freshVariable :: IsSrcSpan s => s -> T (Pat' s)
freshVariable s = do
  state@TypecheckState{..} <- get
  let !next = tcNextVar + 1
      name = "Tmp__" <> showt tcNextVar
  put state {
    tcNextVar = next,
    tcVisible = HashSet.insert name tcVisible }
    -- setting tcVisible here is a bit of a hack, but it should be OK
    -- as long as the fresh variable is used in the current scope and
    -- not a nested scope.
  return (Variable s name)

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

checkPrimOpArgs :: IsSrcSpan s => s -> [Pat' s] -> [Type] -> T ()
checkPrimOpArgs span args tys
  | length args /= length tys =
    prettyErrorAt span
      $ "expected " <> pretty (length tys)
      <> " arguments, found " <> pretty (length args)
  | otherwise = return ()

-- Several primops have polymorphic types, e.g.
--
--   prim.length : forall a . [a] -> nat
--
-- There's no first-class support for polymorphic types in the type
-- checker, but for primops we can just generate a fresh instantiation
-- of the primop's type for each call.

primOpType :: PrimOp -> T ([Type], Type)
primOpType op = case op of
  PrimOpToLower -> return ([StringTy], StringTy)
  PrimOpLength -> do
    x <- freshTyVar
    return ([ArrayTy x], NatTy)
  PrimOpZip -> do
    x <- freshTyVar
    y <- freshTyVar
    return ([ArrayTy x, ArrayTy y], ArrayTy (tupleSchema [x,y]))
  PrimOpConcat -> do
    x <- freshTyVar
    return ([ArrayTy x, ArrayTy x], ArrayTy x)
  PrimOpReverse -> return  ([StringTy], StringTy)
  PrimOpRelToAbsByteSpans ->
    -- prim.relToAbsByteSpans takes an array of pairs as input and
    -- returns an array of pairs as output
    return (
      [ArrayTy (tupleSchema [NatTy, NatTy])],
      ArrayTy (tupleSchema [NatTy, NatTy]))
  PrimOpUnpackByteSpans ->
    -- prim.unpackByteSpans takes an array of (nat, [nat]) as input and
    -- returns an array of pairs as output
    return (
      [ArrayTy (tupleSchema [NatTy, ArrayTy NatTy])],
      ArrayTy (tupleSchema [NatTy, NatTy]))
  PrimOpGtNat -> binaryNatOp
  PrimOpGeNat -> binaryNatOp
  PrimOpLtNat -> binaryNatOp
  PrimOpLeNat -> binaryNatOp
  PrimOpNeNat -> binaryNatOp
  PrimOpAddNat -> return ([NatTy, NatTy], NatTy)
  PrimOpNeExpr -> do
    x <- freshTyVar
    return ([x,x], unit)
  where
    binaryNatOp = return ([NatTy, NatTy], unit)

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
  All _ qs -> varsPat qs r
  Elements _ pat -> varsPat pat r
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
  Clause _ _ _ p _ -> varsPat p r
  Prim _ _ ps -> foldr varsPat r ps
  FieldSelect _ p _ _ -> varsPat p r
  Enum{} -> r

varsQuery :: IsSrcSpan s => Query' s -> VarSet -> VarSet
varsQuery (SourceQuery head stmts _) r =
  foldr varsStmt (foldr varsPat r head) stmts
  where
  varsStmt (SourceStatement a b) r = varsPat a $! varsPat b r

-- | Capture all predicates that appear in a query
tcQueryDeps :: TcQuery -> Set PredicateId
tcQueryDeps q = Set.fromList $ map getRef (overQuery q)
  where
    getRef (PidRef _ ref) = ref

    overQuery :: TcQuery -> [PidRef]
    overQuery (TcQuery ty key mval stmts _) =
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
      TcElements x -> overPat x
      TcQueryGen q -> overQuery q
      TcAll q -> overQuery q
      TcNegation stmts -> foldMap overStatement stmts
      TcPrimCall _ xs -> foldMap overPat xs
      TcIf (Typed _ x) y z -> foldMap overPat [x, y, z]
      TcDeref _ _ p -> overPat p
      TcFieldSelect (Typed _ p) _ -> overPat p
      TcAltSelect (Typed _ p) _ -> overPat p
      TcPromote _ p -> overPat p
      TcDemote _ p -> overPat p
      TcStructPat fs -> foldMap overPat (map snd fs)

data UseOfNegation
  = PatternNegation
  | IfStatement

-- | Whether a query uses negation in its definition.
-- Does not check for transitive uses of negation.
tcQueryUsesNegation :: TcQuery -> Maybe UseOfNegation
tcQueryUsesNegation (TcQuery _ _ _ stmts _) =
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
  MatchArrayPrefix _ty prefix all ->
    firstJust tcPatUsesNegation prefix <|> tcPatUsesNegation all
  MatchExt (Typed _ tcterm) -> tcTermUsesNegation tcterm

tcTermUsesNegation  :: TcTerm -> Maybe UseOfNegation
tcTermUsesNegation = \case
  TcOr x y -> tcPatUsesNegation x <|> tcPatUsesNegation y
  TcFactGen _ x y _ -> tcPatUsesNegation x <|> tcPatUsesNegation y
  TcElementsOfArray x -> tcPatUsesNegation x
  TcElements x -> tcPatUsesNegation x
  TcQueryGen q -> tcQueryUsesNegation q
  TcAll query -> tcQueryUsesNegation query
  TcNegation _ -> Just PatternNegation
  TcPrimCall _ xs -> firstJust tcPatUsesNegation xs
  -- one can replicate negation using if statements
  TcIf{} -> Just IfStatement
  TcDeref _ _ p -> tcPatUsesNegation p
  TcFieldSelect (Typed _ p) _ -> tcPatUsesNegation p
  TcAltSelect (Typed _ p) _ -> tcPatUsesNegation p
  TcPromote _ p -> tcPatUsesNegation p
  TcDemote _ p -> tcPatUsesNegation p
  TcStructPat fs -> firstJust tcPatUsesNegation (map snd fs)

{-
Note [Promotion]
----------------

When we see an expression like 42 or "abc" we want the expression to
be able to stand for either
   1. the literal value
   2. a fact whose key is the literal value

This is very convenient when writing patterns, e.g. we want to be able
to write

   X = hack.ClassDeclaration { name = { name = "Glean" } }

and not

   X = hack.ClassDeclaration { name = hack.QName { name = hack.Name "Glean" } }

but we don't always know at the time we're typechecking the expression
whether we need it to be promoted to the predicate type or not, we may
only know this later. For example:

   X = { name = { name = "Glean" } }
   X : hack.ClassDeclaration

when checking the first statement we don't know the desired type of
the pattern.

So we have to defer the choice about whether to promote types to a
predicate type or not. This is what `promote` and `promoteTo` below
do.

Promotion can happen any time we have a concrete expression: string,
nat, record, enum constant, and so on. For example, when we're
inferring the type of a string, we invent a new type variable, and
record the potential promotion in the TypecheckState (tcPromote).

   infer "abc" = (^"abc", X)   (X is a fresh type variable)
     { promote: string -> X }

(the syntax ^expr means "promoted", represented by the TcPromote
constructor in TcTerm)

At the end of typechecking we hope that X has been instantiated to
either string or a predicate type, and we can then resolve the
TcPromote to either nothing (in the case of string) or a TcFactGen (in
the case of a predicate type).

Resolving promotions
--------------------

In general we may have many constraints in tcPromote to resolve. There
will be a set of constraints like

  From -> To

where From is never a type variable. To can be
 a. a predicate with key = From
 b. equal to From
 c. a type variable

We can deal with (a) and (b) immediately. What about (c)?

We might have a chain, e.g.
  {a : P} -> {a : X}   (1)
  {b : nat} -> X       (2)

We should resolve (1) first, because that influences (2).  So we have
to resolve all the cases where the rhs is known, and then try
again. This is what resolvePromote does.

Strictly speaking this doesn't handle all cases and it could be
inefficient. We should build a graph of the constraints and evaluate
them in dependency order. What if there's a loop? Can that happen? I
haven't seen this go wrong with any examples yet and the worst that
can happen is you get a type error and have to add a type signature to
help the type checker.
-}

promote :: IsSrcSpan s => s -> TcPat -> Type -> T (TcPat, Type)
promote s pat t = do
  y <- freshTyVar
  addPromote s t y
  return (Ref $ MatchExt $ Typed y $ TcPromote t pat, y)

promoteTo :: IsSrcSpan s => s -> Type -> Type -> T (TcPat -> TcPat)
promoteTo _ TyVar{} _ = error "promote: TyVar"
  -- we never promote an unknown type
promoteTo s t (TyVar x) = do
  -- x could be Predicate t, or it could be t
  subst <- gets tcSubst
  case IntMap.lookup x subst of
    Just u -> promoteTo s t u
    Nothing -> do
      addPromote s t (TyVar x)
      return (Ref . MatchExt . Typed (TyVar x) . TcPromote t)
promoteTo _ (PredicateTy (PidRef p _)) (PredicateTy (PidRef q _))
  | p == q = return id
    -- if not equal, q must be the key of p, so fall through
    -- (assume we don't have  predicate P : P)
promoteTo s t u@(PredicateTy (PidRef _ ref)) = do
  PredicateDetails{..} <- getPredicateDetails ref
  addErrSpan s $ unify predicateKeyType t
  return (Ref . MatchExt . Typed u . TcPromote t)
promoteTo s t u = do
  -- the target type (u) is not a predicate or a tyvar, so it must be
  -- the key type and we can unify directly.
  addErrSpan s $ unify t u
  return id

-- | dual to promoteTo. The destination type cannot be a TyVar.
demoteTo :: IsSrcSpan s => s -> Type -> Type -> T (TcPat -> TcPat)
demoteTo _ _ TyVar{} = error "demote: TyVar"
demoteTo s t@(TyVar x) u = do
  subst <- gets tcSubst
  case IntMap.lookup x subst of
    Just t -> demoteTo s t u
    Nothing -> do
      addPromote s u (TyVar x)
      return (Ref . MatchExt . Typed u . TcDemote t)
demoteTo _ (PredicateTy (PidRef p _)) (PredicateTy (PidRef q _))
  | p == q = return id
demoteTo s t@(PredicateTy (PidRef _ ref)) u = do
  PredicateDetails{..} <- getPredicateDetails ref
  addErrSpan s $ unify predicateKeyType u
  return (Ref . MatchExt . Typed u . TcDemote t)
demoteTo s t u = do
  -- the source type (t) is not a predicate or a tyvar, so it must be
  -- the key type and we can unify directly.
  addErrSpan s $ unify t u
  return id

addPromote :: IsSrcSpan s => s -> Type -> Type -> T ()
addPromote span from to =
  modify $ \s ->
    s { tcPromote = (from, to, Some span) : tcPromote s }

resolvePromote :: T ()
resolvePromote = do
  promotes <- gets tcPromote
  let
    resolve
      :: [(Type,Type,Some IsSrcSpan)]
      -> Bool
      -> T [(Type,Type,Some IsSrcSpan)]
    resolve promotes defaultTyVars = fmap catMaybes $
      forM promotes $ \(from, to, Some span) -> do
        to' <- apply to
        -- we know from is not a TyVar
        case (from,to') of
          (TyVar{}, _) -> error "resolvePromote: tyvar"
          (PredicateTy (PidRef _ ref), PredicateTy (PidRef _ ref'))
            | ref == ref' -> return Nothing
          (_other, PredicateTy (PidRef _ ref)) -> do
            PredicateDetails{..} <- getPredicateDetails ref
            addErrSpan span $ unify predicateKeyType from
            return Nothing
          (_other, TyVar{}) | not defaultTyVars ->
            return (Just (from, to', Some span))
          _other -> do
            addErrSpan span $ unify from to'
            return Nothing

    loop [] = return ()
    loop promotes = do
      whenDebug $ liftIO $ hPutStrLn stderr $ show $ vcat
        [ "promotes: "
        , vcat
          [ displayDefault from <> " -> " <> displayDefault to
          | (from,to,_) <- promotes
          ]
        ]
      resolved <- resolve promotes False
      if length resolved < length promotes
        then loop resolved
        else do
          -- only default unconstrained promotions when there's
          -- nothing else we can do.
          resolved2 <- resolve resolved True
          loop resolved2

  loop promotes

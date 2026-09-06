{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Lint (
    lintQuery
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (foldl')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Compat.Prettyprinter

import Glean.Angle.Types
  (Type_(..), latestAngleVersion, tempPredicateId)
import Glean.Database.Schema.Types
  (DbSchema, PredicateDetails(..), lookupPid, lookupPredicateId)
import Glean.Display
import Glean.Query.Codegen.Types
import Glean.RTS.Term as RTS
import Glean.RTS.Types as RTS
import Glean.Schema.Util (tupleSchema)

-- | The lint monad: tracks the set of bound variable IDs in state,
-- and can throw a pretty-printed error.
type L a = StateT IntSet (Except (Doc ())) a

runL :: L a -> Either (Doc ()) a
runL m = runExcept (evalStateT m IntSet.empty)

lintError :: Doc () -> L a
lintError = throwError

-- | Record a variable as bound.
bindVar :: Var -> L ()
bindVar v = modify (IntSet.insert (varId v))

-- | Check that a variable is currently bound.
requireBound :: Var -> L ()
requireBound v = do
  bound <- get
  unless (varId v `IntSet.member` bound) $
    lintError $ "unbound variable:" <+> displayDefault v

-- | Run an action in a nested scope. The bound set is saved and
-- restored afterwards, so bindings made inside do not escape.
scoped :: L a -> L a
scoped m = do
  saved <- get
  a <- m
  put saved
  return a

{-
  Check the query for internal consistency. lintQuery will check
  the following properties of the query:

  - type-correctness, e.g.
    - in A = B, the types of A and B are equivalent
    - in FactGenerator and DerivedFactGenerator, the types of the key
      and value pattern match those of the predicate in the schema
    - arguments of primitives have the correct type
  - bind-before-use: for all variables, MatchBind occurs before MatchVar

-}
lintQuery :: DbSchema -> CodegenQuery -> Either (Doc ()) ()
lintQuery schema QueryWithInfo{..} = runL $ do
  let CgQuery head body = qiQuery
  lintStatements schema body
  lintPat head
  case qiGenerator of
    Nothing -> checkEqType "query head" head qiReturnType
    Just gen -> do
      lintGenerator gen
      lintGeneratorTypes schema head gen

-- ---------------------------------------------------------------------------
-- Bind-before-use and type checking for statement lists

lintStatements :: DbSchema -> [CgStatement] -> L ()
lintStatements schema = mapM_ (lintStatement schema)

lintStatement :: DbSchema -> CgStatement -> L ()
lintStatement schema = \case
  CgStatement pat gen -> do
    -- The generator is evaluated first; its patterns can bind variables
    lintGenerator gen
    -- Then the LHS pattern matches the generator result, and can use
    -- variables bound by the generator as well as previously bound ones
    lintPat pat
    -- Type check: pattern type should match generator result type
    lintStmtTypes schema pat gen

  CgAllStatement var expr stmts -> do
    -- The inner statements are evaluated in a nested scope
    scoped $ do
      lintStatements schema stmts
      lintPat expr
    -- The variable bound by 'all' is available to subsequent statements
    bindVar var

  CgNegation stmts ->
    -- Negation introduces its own scope; bindings do not escape
    scoped $ lintStatements schema stmts

  CgDisjunction stmtss -> do
    -- Each branch starts with the same bound set. Variables bound
    -- in all branches are available afterwards.
    bounds <- forM stmtss $ \stmts -> scoped $ do
      lintStatements schema stmts
      get
    case bounds of
      [] -> return ()
      (b:bs) -> put (foldl' IntSet.intersection b bs)

  CgConditional{..} -> do
    outer <- get
    lintStatements schema cond
    -- then branch sees cond bindings
    lintStatements schema then_
    thenBound <- get
    -- else branch only sees pre-condition bindings
    put outer
    lintStatements schema else_
    elseBound <- get
    -- Variables bound in both branches are available after
    put (IntSet.intersection thenBound elseBound)

-- ---------------------------------------------------------------------------
-- Check uses and record bindings in a single depth-first left-to-right
-- traversal, so that a MatchBind makes the variable immediately
-- available for a later MatchVar in the same pattern.

lintPat :: Pat -> L ()
lintPat = mapM_ lintMatch

lintMatch :: Match () Var -> L ()
lintMatch = \case
  MatchBind v -> bindVar v
  MatchVar v -> requireBound v
  MatchAnd a b -> do
    lintPat a
    lintPat b
  MatchPrefix _ rest ->
    lintPat rest
  MatchArrayPrefix _ pre whole -> do
    mapM_ lintPat pre
    lintPat whole
  _ -> return ()

-- | Lint expression-position subterms of a generator, then bind
-- pattern-position variables.
lintGenerator :: Generator -> L ()
lintGenerator = \case
  FactGenerator _ kpat vpat _ -> do
    lintPat kpat
    lintPat vpat
  TermGenerator expr ->
    lintPat expr
  DerivedFactGenerator _ key value -> do
    lintPat key
    lintPat value
  ArrayElementGenerator _ arr ->
    lintPat arr
  SetElementGenerator _ set ->
    lintPat set
  PrimCall _ args _ ->
    mapM_ lintPat args

-- | Type-check the result generator (qiGenerator). The head expression
-- is the fact ID that the generator will look up.
lintGeneratorTypes :: DbSchema -> Pat -> Generator -> L ()
lintGeneratorTypes schema head gen = case gen of
  FactGenerator pidRef kpat vpat _ -> do
    mdetails <- lookupPredicate schema pidRef
    forM_ mdetails $ \details -> do
      checkEqType
        ("result generator key of" <+> displayDefault pidRef)
        kpat (predicateKeyType details)
      checkEqType
        ("result generator value of" <+> displayDefault pidRef)
        vpat (predicateValueType details)
    checkEqType "result generator fact ID" head (PredicateTy () pidRef)
  _ -> return ()

-- ---------------------------------------------------------------------------
-- Type checking

-- | Check the type consistency of a statement: pattern type vs
-- generator result type, and predicate key/value types for fact
-- generators.
lintStmtTypes :: DbSchema -> Pat -> Generator -> L ()
lintStmtTypes schema pat gen = case gen of
  FactGenerator pidRef kpat vpat _ -> do
    mdetails <- lookupPredicate schema pidRef
    forM_ mdetails $ \details -> do
      checkEqType
        ("key of" <+> displayDefault pidRef) kpat (predicateKeyType details)
      checkEqType
        ("value of" <+> displayDefault pidRef) vpat (predicateValueType details)
    -- The LHS pattern matches the fact ID, which has the predicate type
    checkEqType "fact generator result" pat (PredicateTy () pidRef)

  DerivedFactGenerator pidRef key value -> do
    mdetails <- lookupPredicate schema pidRef
    forM_ mdetails $ \details -> do
      checkEqType
        ("key of" <+> displayDefault pidRef) key (predicateKeyType details)
      checkEqType
        ("value of" <+> displayDefault pidRef) value (predicateValueType details)
    checkEqType "derived fact generator result" pat (PredicateTy () pidRef)

  TermGenerator expr ->
    case (patType pat, patType expr) of
      (Just pty, Just ety) -> typesShouldMatch "term generator" pty ety
      _ -> return ()

  ArrayElementGenerator eltTy _arr ->
    checkEqType "array element generator" pat eltTy

  SetElementGenerator eltTy _set ->
    checkEqType "set element generator" pat eltTy

  PrimCall _op _args retTy ->
    checkEqType "primcall result" pat retTy

lookupPredicate :: DbSchema -> PidRef -> L (Maybe PredicateDetails)
lookupPredicate schema (PidRef pid predId)
  -- The temporary predicate used for derived fact generators is not
  -- stored in the schema, so skip it.
  | predId == tempPredicateId = return Nothing
  | otherwise =
    case lookupPid pid schema of
      Nothing -> case lookupPredicateId predId schema of
        Nothing ->
          lintError $ "unknown predicate:" <+> displayDefault predId
        Just details -> return (Just details)
      Just details -> return (Just details)

-- | Check that the type of a pattern matches an expected type.
-- When the pattern's type cannot be determined, the check is skipped.
checkEqType :: Doc () -> Pat -> Type -> L ()
checkEqType ctx pat expected =
  case patType pat of
    Nothing -> return ()
    Just actual -> typesShouldMatch ctx actual expected

typesShouldMatch :: Doc () -> Type -> Type -> L ()
typesShouldMatch ctx actual expected
  | eqType latestAngleVersion actual expected = return ()
  | otherwise = throwError $ vcat
      [ "type mismatch in" <+> ctx <> ":"
      , "  expected:" <+> displayDefault expected
      , "  actual:" <+> displayDefault actual
      ]

-- ---------------------------------------------------------------------------
-- Inferring the type of a pattern

-- | Attempt to determine the type of a pattern. Returns Nothing when
-- the type cannot be determined from the pattern structure alone
-- (e.g. an empty array or an Alt without surrounding context).
patType :: Pat -> Maybe Type
patType (Byte _) = Just ByteTy
patType (Nat _) = Just NatTy
patType (RTS.String _) = Just StringTy
patType (ByteArray _) = Just (ArrayTy ByteTy)
patType (Array []) = Nothing
patType (Array (t:_)) = ArrayTy <$> patType t
patType (Tuple ts) = tupleSchema <$> traverse patType ts
patType (Alt _ _) = Nothing
patType (Ref m) = matchType m

matchType :: Match () Var -> Maybe Type
matchType (MatchWild ty) = Just ty
matchType (MatchNever ty) = Just ty
matchType (MatchBind v) = Just (varType v)
matchType (MatchVar v) = Just (varType v)
matchType (MatchFid _) = Nothing
matchType (MatchAnd a _) = patType a
matchType (MatchPrefix _ _) = Just StringTy
matchType (MatchArrayPrefix ty _ _) = Just (ArrayTy ty)
matchType (MatchExt _) = Nothing

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Opt
  ( optimise
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Text (Text)

import qualified Glean.Angle.Types as Type
import Glean.Angle.Types (FieldDef_(..))
import Glean.Query.Flatten.Types
import Glean.Query.Codegen.Types
  ( matchVar
  , Match(..)
  , Var(..)
  , Generator_(..)
  , Pat
  , Generator
  , QueryWithInfo(..))
import Glean.Query.Vars
import Glean.RTS.Term
import Glean.RTS.Types

{-
Note [Query Simplification]

This pass performs obvious optimisations on a query as follows:

- Perform unification on all statements of the form P = Q
- Apply the substitution resulting from unification

So for example if we have

  P = cxx1.Name S
  S = "abc"

Unification will produce the substitution [S->"abc"]; applying it will produce

  P = cxx1.Name "abc"
  "abc" = "abc"

and finally in the Reorder pass we will identify the statement "abc" =
"abc" as redundant and eliminate it.


Optimising Derived Predicates
-----------------------------

This optimisation is particularly useful when expanding derived
predicates. For example, given

   predicate RevStringPair : { a : string, b : string }
     { A, B } where StringPair { B, A }

and a query

   RevStringPair { _, "a" }

after expansion we will have

   X where StringPair { B, A }; X = { A, B }; X = { _, "a" }

the StringPair query is not efficient here; we could do much better if
we knew that B = "a".

Unification will produce the substitution

  [ X -> { A, B }, B -> "a" ]

and after applying the substitution and simplification we will have

   { A, "a" } where StringPair { "a", A }

which is exactly what we wanted.


Dealing with Choice
-------------------

Choice is the main source of complexity here.

For example, suppose we have

   Y where
     cxx1.Name X;
     Y = (1 where X = "a") | (2 where X = "b")

Clearly there must be some "local" unification, because the
substitution X = "a" only applies inside (1 where X = "a").  But just
applying the local substitution [X->"a"] inside (1 where X = "a")
can't be right, because we would get

   Y = (1 where "a" = "a") | ...

The problem is that we bound X during unification and then discarded
that part of the substitution. We must never discard a substitution
when it has not been applied to all instances of a variable - and yet
we cannot let the substitition for X escape from the branch of this
choice, because it isn't valid outside. This is a contradiction,
so the solution is not to unify X in the first place. But how do we
know we shouldn't unify X? Because it is visible *outside* the scope
of the local substitution.

So the rule for unificaiton of (A | B) is:

- Unification is local to each branch. We have a substitution S
  arising from the context and two separate substitutions Sa and Sb
  arising from unification inside A and B respectively. The
  subsititions we apply to A and B are S+Sa and S+Sb respectively.

- We must *not* unify any variables that are visible outside. In other
  words, Sa and Sb can only bind variables that are local to A and B
  respectively.


Algorithm
---------

Unification of a complete query proceeds as follows:

0. Start with the empty substitution S = {}
1. Unify everywhere except (A | B) subterms, extending S
2. Apply S everywhere except (A | B) subterms
3. For each (A | B) subterm, repeat from step 1 for A and B separately,
   starting with current substitition S

-}

optimise :: FlattenedQuery -> Except Text FlattenedQuery
optimise query@QueryWithInfo{..} = do
  let
    state = OptState
      { optNextVar = qiNumVars
      , optSubst = IntMap.empty
      , optGenerators = IntMap.empty
      , optCurrentScope = IntSet.empty
      , optOuterScope = IntSet.empty
      , optSeen = IntMap.empty
      }
  (optimised, state') <- flip runStateT state $
    optimiseQuery =<< freshWildQuery qiQuery
    -- unification relies on wildcards being replaced by variables
    --  1. so that when we substitute an expression in multiple places
    --     we can unify the wildcards across the different instantiations.
    --  2. we want to elimiante duplicate "X = generator" statements
    --     by unifying the generators and then spotting identical
    --     statements. But those will only end up identical if we
    --     replace the wildcards with variables before unifying and
    --     substituting.
    -- freshWildQuery also replaces MatchBind with MatchVar, so we don't
    -- have spurious differences between terms that are really identical.
  return query { qiQuery = optimised, qiNumVars = optNextVar state' }

optimiseQuery :: FlatQuery -> U FlatQuery
optimiseQuery query@(FlatQuery key maybeVal stmts) = do
  -- determine variables visible outside of any nested choice:
  modify $ \s -> s { optCurrentScope = queryScope query }
  stmts' <- optStmts stmts
  FlatQuery
    <$> apply key
    <*> mapM apply maybeVal
    <*> pure stmts'

-- | Apply the current substutition
class Apply t where
  apply :: t -> U t

instance Apply Generator where
  apply (FactGenerator pid key val range) =
    FactGenerator pid <$> apply key <*> apply val <*> pure range
  apply (TermGenerator x) = TermGenerator <$> apply x
  apply (DerivedFactGenerator pid key val) =
    DerivedFactGenerator pid <$> apply key <*> apply val
  apply (ArrayElementGenerator ty arr) =
    ArrayElementGenerator ty <$> apply arr
  apply (SetElementGenerator ty arr) =
    SetElementGenerator ty <$> apply arr
  apply (PrimCall op args ty) =
    PrimCall op <$> mapM apply args <*> pure ty

{- Note [unification failure]

Unification failure can easily arise as a result of choice. For
example:

(0|X = 0|"abc") | (0|X = 1|"def")

The unification of (0|X) with (1|"def") fails, and we would like to
exploit that to simplify the whole statement to

(0|X) = (0|"abc")

This works as follows:

* unify returns a Bool to indicate whether unification succeeded

* unifyStmt replaces a statement with falseStmt if unification of the
  LHS and RHS fails

* In a disjunctions, we filter out any alternatives that are
  definitely empty, where empty is defined as "any statement is
  obviously false".

Note: we have to be careful about getting rid of statements entirely,
even if a statement is provably false, because deleting a statement
can leave unbound variables behind.  See optStmts and apply below.

-}

instance Apply FlatStatementGroup where
  apply (FlatStatementGroup stmts ord) =
    FlatStatementGroup <$> mapM apply stmts <*> pure ord

instance Apply FlatStatement where
  apply (FlatStatement ty lhs rhs) = do
    -- these were already unified by 'unify'
    FlatStatement ty <$> apply lhs <*> apply rhs
  apply (FlatAllStatement v e g) = do
    FlatAllStatement v <$> apply e <*> apply g
  apply (FlatNegation stmts) = do
    -- assumptions arising inside the negation are not true outside of it.
    stmts' <- optStmtsEnclosed stmts
    return (FlatNegation  stmts')
  apply (FlatDisjunction [stmts]) = do
    stmts' <- apply stmts
    return (grouping stmts')
  apply (FlatDisjunction stmtss) = do
    stmtss' <- mapM optStmtsEnclosed stmtss
    -- we can remove alternatives that are known to be false, but if
    -- ALL alternatives are false, then we must leave behind just one
    -- alternative so that we don't leave unbound variables.
    case filter (not . isFalseGroups) stmtss' of
      [] -> case stmtss' of
        [] -> return (FlatDisjunction [])
        (ss : _) -> return (grouping ss)
      some -> return (FlatDisjunction some)
  apply (FlatConditional cond then_ else_) = do
    -- like disjunctions, assumptions arising from the conditional statements
    -- are not true outside of it. However, those arising from the condition
    -- are true in the 'then' case.
    (cond', then') <-
      enclose cond $ do
        cond' <- optStmts cond
        then' <- optStmtsEnclosed then_
        return (cond', then')
    else' <- optStmtsEnclosed else_

    return $ if isFalseGroups cond'
      then grouping else'
      else FlatConditional cond' then' else'

optStmtsEnclosed :: FlatStatementGroup -> U FlatStatementGroup
optStmtsEnclosed stmts = enclose stmts $ optStmts stmts

-- If a sequence of statements is found to be false, then we place
-- a falseStmt sentinel at the beginning. We don't actually remove
-- any statements at this stage, because we have to be careful not to
-- leave any variables unbound.
optStmts :: FlatStatementGroup -> U FlatStatementGroup
optStmts (FlatStatementGroup stmts ord) = do
  notFalse <- and <$> mapM unifyStmt stmts
  stmts' <- concatMap expandStmt <$> mapM apply stmts
  stmts'' <- filterStmts stmts'
  -- unify may fail, but apply may also leave behind a false statement:
  if notFalse && not (any isFalseStmt stmts'')
    then return (FlatStatementGroup stmts'' ord)
    else return (FlatStatementGroup (falseStmt : stmts'' ) ord)

-- Look for the sentinel left by optStmts
isFalseGroups :: FlatStatementGroup -> Bool
isFalseGroups (FlatStatementGroup (s : _) _) = isFalseStmt s
isFalseGroups (FlatStatementGroup [] _) = False

isFalseStmt :: FlatStatement -> Bool
isFalseStmt (FlatNegation (FlatStatementGroup [] _)) = True
isFalseStmt (FlatDisjunction []) = True
isFalseStmt (FlatDisjunction [FlatStatementGroup stmts _]) =
  any isFalseStmt stmts
isFalseStmt _ = False

instance Apply Pat where
  apply (Tuple xs) = Tuple <$> mapM apply xs
  apply (Array xs) = Array <$> mapM apply xs
  apply (Alt n x) = Alt n <$> apply x
  apply (Set xs) = Set <$> mapM apply xs
  apply (Ref (MatchVar v)) = applyVar v
  apply (Ref (MatchBind v)) = applyVar v
  apply (Ref (MatchAnd x y)) = do
    x' <- apply x
    y' <- apply y
    return (Ref (MatchAnd x' y'))
  apply (Ref (MatchPrefix str x)) = Ref . MatchPrefix str <$> apply x
  apply (Ref (MatchArrayPrefix ty pre all)) =
    Ref <$> (MatchArrayPrefix ty <$> mapM apply pre <*> apply all)
  apply t@Byte{} = return t
  apply t@ByteArray{} = return t
  apply t@Nat{} = return t
  apply t@String{} = return t
  apply t@(Ref MatchExt{}) = return t
  apply t@(Ref MatchFid{}) = return t
  apply t@(Ref MatchNever{}) = return t
  apply t@(Ref MatchWild{}) = return t

applyVar :: Var -> U Pat
applyVar var@(Var _ v _) = do
  subst <- gets optSubst
  case IntMap.lookup v subst of
    Just pat -> apply pat
      -- also apply the substitition to the result, because we don't
      -- keep the substitution already in ground form. This is
      -- potentially inefficient and there are better ways to do it
      -- (typically with mutable variables), but it's probably not
      -- important enough to fix yet.
    _otherwise -> return (Ref (MatchVar var))

-- | For running unification on one alternative of nested choice
-- or a negated query.
--
-- Does not add substitutions or new variables to the parent scope.
enclose :: FlatStatementGroup -> U a -> U a
enclose (FlatStatementGroup stmts _) u = do
  state0 <- get
  -- set the outer scope to be the current scope
  let scope = foldr stmtScope (optCurrentScope state0) stmts
  modify $ \s ->
    s { optCurrentScope = scope, optOuterScope = optCurrentScope state0 }
  a <- u
  modify $ \s ->
    s { optSubst = optSubst state0
      , optGenerators = optGenerators state0
      , optCurrentScope = optCurrentScope state0
      , optOuterScope = optOuterScope state0
      , optSeen = optSeen state0 }
  return a

unifyStmt :: FlatStatement -> U Bool
unifyStmt (FlatStatement _ lhs rhs)
  | neverMatches lhs = return False
  | TermGenerator rhs' <- rhs = unify lhs rhs'
  | otherwise = do
    r <- case isVar lhs of
      Just v -> extend v rhs
      Nothing -> return True
    case rhs of
      FactGenerator _ key val _ ->
        return $ r && not (neverMatches key || neverMatches val)
      _ -> return r
unifyStmt FlatAllStatement{} = return True
unifyStmt FlatNegation{} = return True
  -- ignore negations for now. We will recurse into it later
unifyStmt (FlatDisjunction [FlatStatementGroup stmts _]) =
  and <$> mapM unifyStmt stmts
  -- singleton FlatDisjunction is used for grouping, we must retain
  -- it, but not treat it as a disjunction.
unifyStmt FlatDisjunction{} = return True
  -- ignore a disjunction for now. We will recurse into it in 'apply'
unifyStmt FlatConditional{} = return True
  -- ignore conditions for now. We will recurse into it in 'apply'

neverMatches :: Pat -> Bool
neverMatches = \case
  Byte _ -> False
  Nat _ -> False
  Array terms -> any neverMatches terms
  ByteArray _ -> False
  String _ -> False
  Tuple terms -> any neverMatches terms
  Set _ -> False
  Alt _ term -> neverMatches term
  Ref match -> case match of
    MatchWild _ -> False
    MatchNever _ -> True
    MatchFid _ -> False
    MatchBind _ -> False
    MatchVar _ -> False
    MatchAnd left right -> neverMatches left || neverMatches right
    MatchPrefix _ term -> neverMatches term
    MatchArrayPrefix _ty pre all -> any neverMatches pre || neverMatches all
    MatchExt () -> False

unifyGen :: Generator -> Generator -> U Bool
unifyGen (TermGenerator a) (TermGenerator b) = a `unify` b
unifyGen (FactGenerator _ k1 v1 _) (FactGenerator _ k2 v2 _) =
  (&&) <$> unify k1 k2 <*> unify v1 v2
unifyGen (DerivedFactGenerator _ k1 v1) (DerivedFactGenerator _ k2 v2) =
  (&&) <$> unify k1 k2 <*> unify v1 v2
unifyGen _ _ = return True

-- | Unify two patterns, extending the substitution. Note that this is
-- not complete unification: we aren't guaranteeing to produce a
-- unifier. We might refrain from unifying in some cases (see the
-- comment about or-patterns at the top of the module).
unify :: Pat -> Pat -> U Bool
unify (Byte x) (Byte y) = return (x == y)
unify (Nat x) (Nat y) = return (x == y)
unify (ByteArray x) (ByteArray y) = return (x == y)
unify (String x) (String y) = return (x == y)
unify (Ref (MatchFid x)) (Ref (MatchFid y)) = return (x == y)
unify (Ref MatchWild{}) _ = return True
unify _ (Ref MatchWild{}) = return True
unify (Ref MatchNever{}) _ = return False
unify _ (Ref MatchNever{}) = return False
unify (Ref (MatchVar v)) pat = extend v (TermGenerator pat)
unify pat (Ref (MatchVar v)) = extend v (TermGenerator pat)
unify (Ref (MatchBind v)) pat = extend v (TermGenerator pat)
unify pat (Ref (MatchBind v)) = extend v (TermGenerator pat)
unify (Tuple xs) (Tuple ys) = and <$> zipWithM unify xs ys
unify (Array xs) (Array ys)
  | length xs == length ys = and <$> zipWithM unify xs ys
  | otherwise = return False
unify (Alt n x) (Alt m y)
  | n == m = unify x y
  | otherwise = return False
unify (Ref (MatchPrefix s x)) (Ref (MatchPrefix t y))
  | s == t = unify x y
  | otherwise = return True -- might still match
unify (Ref (MatchPrefix s x)) (String y)
  | Just r <- B.stripPrefix s y = unify x (String r)
unify (String x) (Ref (MatchPrefix s y))
  | Just r <- B.stripPrefix s x = unify y (String r)
unify (Ref (MatchAnd x y)) z = (&&) <$> unify x z <*> unify y z
unify z (Ref (MatchAnd x y)) = (&&) <$> unify z x <*> unify z y
unify (Ref (MatchArrayPrefix _ pre all)) (Ref (MatchArrayPrefix _ pre' all')) = do
  b <- unify all all'
  bs <- zipWithM unify pre pre'
  return (and (b:bs))
unify (Ref (MatchArrayPrefix _ pre all)) (Array xs)
  | length pre <= length xs = do
    b <- unify all (Array xs)
    bs <- zipWithM unify pre xs
    return (and (b:bs))
unify a@Array{} b@(Ref (MatchArrayPrefix{})) = unify b a
unify _ _ = return False

extend :: Var -> Generator -> U Bool
-- These two generators don't tell us anything:
extend _ ArrayElementGenerator{} = return True
extend _ PrimCall{} = return True
-- TermGenerators get added to the substitution:
extend var (TermGenerator t) = do
  state <- get
  t <- apply t  -- ensure we don't create a recursive substitution
  let
    bind v gen = do
      modify $ \s -> s { optSubst = IntMap.insert (varId v) gen (optSubst s) }
      return True

    hasSubstitution v = IntMap.lookup (varId v) (optSubst state)
    comesFromOuterScope v = IntSet.member (varId v) (optOuterScope state)

    -- refuse to bind a variable if it is visible in the outer scope
    -- (see comment at the top of this module). If this happens, we
    -- try the unification the other way around instead.
    check v t _ | Just v' <- isVar t, v == v' = return True
    check v t orElse
      | Just t' <- hasSubstitution v = unify t t'
      | comesFromOuterScope v = orElse
      | otherwise = bind v t

  check var t $
    case isVar t of
      Just y -> check y (Ref (MatchVar var)) (return True)
      _ -> extendGen var (TermGenerator t)
-- Other kinds of generators: remember them in optGenerators
extend var gen = extendGen var gen

extendGen :: Var -> Generator -> U Bool
extendGen var gen = do
  state <- get
  gen <- apply gen
  pat <- applyVar var
  case isVar pat of
    Nothing ->
      -- it can be a fact ID. What should we do in that case? (TODO)
      return True
    Just var' ->
      case IntMap.lookup (varId var') (optGenerators state) of
        Just gen' -> unifyGen gen gen'
        Nothing -> do
          modify $ \s -> s { optGenerators =
            IntMap.insert (varId var') gen (optGenerators s) }
          return True

isVar :: Pat -> Maybe Var
isVar (Ref (MatchVar v)) = Just v
isVar (Ref (MatchBind v)) = Just v
isVar _ = Nothing

data OptState = OptState
  { optNextVar :: !Int
    -- ^ for making fresh variables
  , optSubst :: Subst
    -- ^ current substitution
  , optGenerators :: IntMap Generator
    -- ^ knowledge about which variables are bound by generators. Only
    -- contains FactGenerators and DerivedFactGenerators currently.
  , optCurrentScope :: VarSet
    -- ^ all variables visible in the current scope, excluding nested
    -- (A | B) subterms
  , optOuterScope :: VarSet
    -- ^ all variables visible in the enclosing scope, when we are
    -- unifying inside a nested (A | B) subterm.  These are the
    -- variables we must not unify (see comment at the top of the
    -- module).
  , optSeen :: IntMap Generator
    -- ^ statements we have seen before. See Note [identical statements]
  }

instance Monad m => Fresh (StateT OptState m) where
  peek = gets optNextVar
  alloc = do
    state@OptState{..} <- get
    put state{ optNextVar = optNextVar + 1 }
    return optNextVar

type U a = StateT OptState (Except Text) a

type Subst = IntMap Pat

-- | Traverse the query, excluding nested (A | B), !A, and "if" subterms,
-- collecting the visible variables.  These are the variables that we must
-- not unify *inside* an (A | B), !A, or "if" subterm, because they are
-- visible outside it. Variables that are local to one of these subterms
-- may be safely unified.
queryScope :: FlatQuery -> VarSet
queryScope (FlatQuery key maybeVal (FlatStatementGroup stmts _)) =
  foldr termScope (foldr stmtScope s stmts) maybeVal
  where
    s = termScope key IntSet.empty

stmtScope :: FlatStatement -> VarSet -> VarSet
stmtScope (FlatStatement _ lhs rhs) r = termScope lhs (genScope rhs r)
stmtScope (FlatAllStatement v e (FlatStatementGroup stmts _)) r =
  addToCurrentScope v $! termScope e $! foldr stmtScope r stmts
stmtScope (FlatNegation _) r = r
stmtScope (FlatDisjunction [FlatStatementGroup stmts _]) r =
  foldr stmtScope r stmts
stmtScope FlatDisjunction{} r = r
  -- contents of or-patterns are not part of the "current scope"
stmtScope FlatConditional{} r = r
  -- contents of if-patterns are not part of the "current scope"

genScope :: Generator -> VarSet -> VarSet
genScope (FactGenerator _ key val _) r = termScope key $! termScope val r
genScope (TermGenerator pat) r = termScope pat r
genScope (DerivedFactGenerator _ key val) r = termScope key $! termScope val r
genScope (ArrayElementGenerator _ exp) r = termScope exp r
genScope (SetElementGenerator _ exp) r = termScope exp r
genScope (PrimCall _ args _) r = foldr termScope r args

addToCurrentScope :: Var -> VarSet -> VarSet
addToCurrentScope (Var _ v _) r = IntSet.insert v r

termScope :: Pat -> VarSet -> VarSet
termScope pat r = foldr onMatch r pat
  where
  onMatch :: Match () Var -> VarSet -> VarSet
  onMatch m r = case m of
    MatchPrefix _ rest -> foldr onMatch r rest
    MatchBind v -> addToCurrentScope v r
    MatchVar v -> addToCurrentScope v r
    MatchAnd x y -> foldr onMatch (foldr onMatch r y) x
    MatchArrayPrefix _ty pre all ->
      foldr onMatch (foldr onMatch r all) (foldMap toList pre)
    MatchNever{} -> r
    MatchWild{} -> r
    MatchExt{} -> r
    MatchFid{} -> r


-- | For T = U statements, decompose the statement as far as possible.
-- For example
--
-- >    {A,B} = {C,D}
--
-- decomposes into
--
-- >     A = C
-- >     B = D
--
-- and eliminate any statements that are obviously redundant. That is,
-- when the lhs obviously matches the rhs, and the statement binds no
-- variables.
--
expandGroup :: FlatStatementGroup -> FlatStatementGroup
expandGroup (FlatStatementGroup stmts ord) =
  FlatStatementGroup (concatMap expandStmt stmts) ord

expandStmt :: FlatStatement -> [FlatStatement]
expandStmt (FlatStatement stmtTy lhs (TermGenerator rhs)) =
  [ FlatStatement ty a (TermGenerator b)
  | (ty,a,b) <- expand stmtTy lhs rhs ]
  where
  expand :: Type -> Pat -> Pat -> [(Type,Pat,Pat)]
  expand ty a b = case (a,b) of
    (_, Ref MatchWild{}) -> []
    (Ref MatchWild{}, _) -> []
    (_, Ref MatchNever{}) -> []
    (Ref MatchNever{}, _) -> []
    (Byte a, Byte b) | a == b -> []
    (Nat a, Nat b) | a == b -> []
    (String a, String b) | a == b -> []
    (Array ts, Array us)
      | Type.ArrayTy eltTy <- derefType ty
      , length ts == length us -> concat (zipWith (expand eltTy) ts us)
    (ByteArray b, ByteArray c) | b == c -> []
    (Tuple ts, Tuple us)
      | Type.RecordTy fields <- derefType ty ->
        concat (zipWith3 expand (map fieldDefType fields) ts us)
    (Alt n x, Alt m y)
      | n == m, Just fields <- sumLike (derefType ty) ->
        expand (fieldDefType (fields !! fromIntegral n)) x y
    (Ref x, Ref y)
      | Just x' <- matchVar x, Just y' <- matchVar y, x' == y' -> []
    (Ref (MatchAnd a b), x) -> expand ty a x ++ expand ty b x
    (x, Ref (MatchAnd a b)) -> expand ty x a ++ expand ty x b
    (Ref (MatchPrefix s a), String b)
      | Just r <- B.stripPrefix s b -> expand ty a (String r)
    (String a, Ref (MatchPrefix s b))
      | Just r <- B.stripPrefix s a -> expand ty (String r) b
    (Ref (MatchPrefix s a), Ref (MatchPrefix t b))
      | t == s -> expand ty a b
    (Ref (MatchFid x), Ref (MatchFid y)) | x == y -> []
    (Ref (MatchArrayPrefix eltTy0 ts all), Array us)
      | length ts <= length us
      , eltTy <- derefType eltTy0
      -> concat (expand ty all (Array us) : zipWith (expand eltTy) ts us)
    (Ref (MatchArrayPrefix eltTy0 ts all), Ref (MatchArrayPrefix _ty us all'))
      | eltTy <- derefType eltTy0
      , length ts == length us
      -> concat (expand ty all all' : zipWith (expand eltTy) ts us)
    _ -> [(ty,a,b)]
expandStmt s@(FlatNegation _) = [s]
  -- the expansion is handled by @apply@ with @optStmts@ because we need to
  -- constrain the scope of the substitutions arising from inside the negation.
expandStmt (FlatDisjunction [stmts]) =
  case expandGroup stmts of
    FlatStatementGroup [] _ -> []
    xs -> [grouping xs]
  -- non-singleton disjunctions are handled by apply, which will
  -- expand the stmts via optStmts.
expandStmt s = [s]

-- -----------------------------------------------------------------------------
-- Eliminating duplicate statements

{-
Note [identical statements]

We want to combine multiple generators like

  X = pred { _, A }
  X = pred { B, "x" }

this works as follows: first we replace all the wildcards with fresh variables

  X = pred { _1, A }
  X = pred { B, "x" }

Next, when unifying we insert the first statement into the substitution

  { X := pred { _1, A } }

when we see the second statement, we unify its generator with the
first one (see 'extend'), giving

  { _1 := B, A := "x", X := pred { _1, A } }

in 'apply', we apply the substitution to each statement:

  X = pred { B, "x" }
  X = pred { B, "x" }

next, we just elimiante literally identical statements, being careful
that we don't propagate assumptions from disjunctions/negation to the
outside scope, as with unification.

Note that even after unification and substitution we might still have
different generators for a given variable, when some of them occur
inside disjunctions or negation.
-}

notDuplicateStmt :: FlatStatement -> U Bool
notDuplicateStmt (FlatStatement _ lhs rhs)
  | Ref (MatchVar v) <- lhs = dup v rhs
  | Ref (MatchBind v) <- lhs = dup v rhs
  where
  dup (Var _ v _ ) rhs = do
    history <- gets optSeen
    case IntMap.lookup v history of
      Just gen -> return (gen /= rhs)
      Nothing -> do
        modify $ \s -> s { optSeen = IntMap.insert v rhs history }
        return True
notDuplicateStmt _other = return True

encloseSeen :: U a -> U a
encloseSeen inner = do
  state <- get
  a <- inner
  modify $ \s -> s { optSeen = optSeen state }
  return a

filterStmt :: FlatStatement -> U FlatStatement
filterStmt stmt = case stmt of
  FlatStatement{} -> return stmt
  FlatAllStatement{} -> return stmt
  FlatNegation stmts -> FlatNegation <$> filterGroupEnclosed stmts
  FlatDisjunction [stmts] -> grouping <$> filterGroup stmts
  FlatDisjunction stmtss ->
    FlatDisjunction <$> mapM filterGroupEnclosed stmtss
  FlatConditional cond then_ else_ -> do
    (cond', then') <- encloseSeen $ do
      cond' <- filterGroup cond
      then' <- filterGroupEnclosed then_
      return (cond', then')
    else' <- filterGroupEnclosed else_
    return (FlatConditional cond' then' else')

filterGroup :: FlatStatementGroup -> U FlatStatementGroup
filterGroup (FlatStatementGroup stmts ord) = do
  stmts' <- filterStmts stmts
  return (FlatStatementGroup stmts' ord)

filterStmts :: [FlatStatement] -> U [FlatStatement]
filterStmts stmts = do
  filtered <- filterM notDuplicateStmt stmts
  mapM filterStmt filtered

filterGroupEnclosed :: FlatStatementGroup -> U FlatStatementGroup
filterGroupEnclosed = encloseSeen . filterGroup

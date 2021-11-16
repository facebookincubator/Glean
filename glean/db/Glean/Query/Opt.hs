{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Opt
  ( optimise
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import qualified Glean.Angle.Types as Type
import Glean.Angle.Types (FieldDef_(..))
import Glean.Query.Flatten.Types hiding (fresh)
import Glean.Query.Codegen
import Glean.RTS.Term hiding (Match(..))
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


Dealing with Or-Patterns
------------------------

Or-patterns are the main source of complexity here.

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
or-pattern, because it isn't valid outside. This is a contradiction,
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
      , optCurrentScope = IntSet.empty
      , optOuterScope = IntSet.empty
      }
  (optimised, state') <- flip runStateT state $ optimiseQuery qiQuery
  return query { qiQuery = optimised, qiNumVars = optNextVar state' }

optimiseQuery :: FlatQuery -> U FlatQuery
optimiseQuery query@(FlatQuery key maybeVal stmts) = do
  -- determine variables visible outside of any nested or-patterns:
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
  apply (FactGenerator pid key val) =
    FactGenerator pid <$> apply key <*> apply val
  apply (TermGenerator x) = TermGenerator <$> apply x
  apply (DerivedFactGenerator pid key val) =
    DerivedFactGenerator pid <$> apply key <*> apply val
  apply (ArrayElementGenerator ty arr) =
    ArrayElementGenerator ty <$> apply arr
  apply (PrimCall op args) =
    PrimCall op <$> mapM apply args

{- Note [unification failure]

Unification failure can easily arise as a result of or-patterns. For
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

instance Apply FlatStatement where
  apply (FlatStatement ty lhs rhs) = do
    -- these were already unified by 'unify'
    FlatStatement ty <$> apply lhs <*> apply rhs
  apply (FlatNegation stmts) = do
    -- assumptions arising inside the negation are not true outside of it.
    stmts' <- optStmtsEnclosed stmts
    return (FlatNegation  stmts')
  apply (FlatDisjunction [stmts]) = do
    stmts' <- mapM (mapM apply) stmts
    return (FlatDisjunction [stmts'])
  apply (FlatDisjunction stmtss) = do
    stmtss' <- mapM optStmtsEnclosed stmtss
    -- we can remove alternatives that are known to be false, but if
    -- ALL alternatives are false, then we must leave behind just one
    -- alternative so that we don't leave unbound variables.
    case filter notFalseGroups stmtss' of
      [] -> case stmtss' of
        [] -> return (FlatDisjunction [])
        (ss : _) -> return (FlatDisjunction [ss])
      some -> return (FlatDisjunction some)

optStmtsEnclosed :: [FlatStatementGroup] -> U [FlatStatementGroup]
optStmtsEnclosed stmts = enclose stmts $ optStmts stmts

-- If a sequence of statements is found to be false, then we place
-- a falseStmt sentinel at the beginning. We don't actually remove
-- any statements at this stage, because we have to be careful not to
-- leave any variables unbound.
optStmts :: [FlatStatementGroup] -> U [FlatStatementGroup]
optStmts stmts = do
  notFalse <- all and <$> mapM (mapM unifyStmt) stmts
  stmts' <- expandGroups <$> mapM (mapM apply) stmts
  -- unify may fail, but apply may also leave behind a false statement:
  if notFalse && all (all notFalseStmt) stmts'
    then return stmts'
    else return ( (falseStmt :| []) : stmts' )

-- Look for the sentinel left by optStmts
notFalseGroups :: [FlatStatementGroup] -> Bool
notFalseGroups ((s :| _) : _) = notFalseStmt s
notFalseGroups [] = True

notFalseStmt :: FlatStatement -> Bool
notFalseStmt (FlatNegation []) = False
notFalseStmt (FlatDisjunction []) = False
notFalseStmt (FlatDisjunction [stmts]) = all (all notFalseStmt) stmts
notFalseStmt _ = True


instance Apply Pat where
  apply (Tuple xs) = Tuple <$> mapM apply xs
  apply (Array xs) = Array <$> mapM apply xs
  apply (Alt n x) = Alt n <$> apply x
  apply (Ref (MatchVar v)) = applyVar v
  apply (Ref (MatchBind v)) = applyVar v
  apply (Ref (MatchAnd x y)) = do
    x' <- apply x
    y' <- apply y
    return (Ref (MatchAnd x' y'))
  apply (Ref (MatchPrefix str x)) = Ref . MatchPrefix str <$> apply x
  apply (Ref (MatchSum alts)) =
    Ref . MatchSum <$> mapM (mapM apply) alts
  apply t = return t

applyVar :: Var -> U Pat
applyVar var@(Var _ v _) = do
  subst <- gets optSubst
  case IntMap.lookup v subst of
    Nothing -> return (Ref (MatchVar var))
    Just pat -> apply pat
      -- also apply the substitition to the result, because we don't
      -- keep the substitution already in ground form. This is
      -- potentially inefficient and there are better ways to do it
      -- (typically with mutable variables), but it's probably not
      -- important enough to fix yet.

-- | For running unification on one alternative of nested or-pattern
-- or a negated query.
--
-- Does not add substitutions or new variables to the parent scope.
enclose :: [FlatStatementGroup] -> U a -> U a
enclose stmts u = do
  state0 <- get
  -- set the outer scope to be the current scope
  let scope = foldr f (optCurrentScope state0) stmts
        where f group r = foldr stmtScope r group
  modify $ \s ->
    s { optCurrentScope = scope, optOuterScope = optCurrentScope state0 }
  a <- u
  modify $ \s ->
    s { optSubst = optSubst state0
      , optCurrentScope = optCurrentScope state0
      , optOuterScope = optOuterScope state0 }
  return a

unifyStmt :: FlatStatement -> U Bool
unifyStmt (FlatStatement _ lhs (TermGenerator rhs)) = unify lhs rhs
unifyStmt FlatStatement{} = return True
unifyStmt FlatNegation{} = return True
  -- ignore negations for now. We will recurse into it later
unifyStmt (FlatDisjunction [stmts]) =
  all and <$> mapM (mapM unifyStmt) stmts
  -- singleton FlatDisjunction is used for grouping, we must retain
  -- it, but not treat it as a disjunction.
unifyStmt (FlatDisjunction _) = return True
  -- ignore a disjunction for now. We will recurse into it later

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
unify (Ref (MatchVar v)) pat = extend v pat
unify pat (Ref (MatchVar v)) = extend v pat
unify (Ref (MatchBind v)) pat = extend v pat
unify pat (Ref (MatchBind v)) = extend v pat
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
unify (Ref MatchSum{}) (Ref MatchSum{}) = return True -- TODO: better
unify _ _ = return False

extend :: Var -> Pat -> U Bool
extend (Var ty v nm) pat = do
  state <- get
  pat <- apply pat  -- ensure we don't create a recursive substitution
  let
    bind v pat = do
      (pat', fresh) <- freshWild pat
      -- don't forget that any fresh vars are now part of the current scope:
      modify $ \s ->
         s { optSubst = IntMap.insert v pat' (optSubst s)
           , optCurrentScope = IntSet.union (optCurrentScope s) fresh
           }
      return True

    -- refuse to bind a variable if it is visible in the outer scope
    -- (see comment at the top of this module). If this happens, we
    -- try the unification the other way around instead.
    check v (Ref (MatchVar (Var _ v' _))) _ | v == v' = return True
    check v (Ref (MatchBind (Var _ v' _))) _ | v == v' = return True
    check v pat orElse
      | Just pat' <- IntMap.lookup v (optSubst state) = unify pat pat'
      | IntSet.member v (optOuterScope state) = orElse
      | otherwise = bind v pat

  check v pat $
    let try (Var _ v' _) =
          check v' (Ref (MatchVar (Var ty v nm))) (return True)
    in
    case pat of
      Ref (MatchVar v') -> try v'
      Ref (MatchBind v') -> try v'
      _ -> return True


-- | Instantiate all the wildcards in a pattern with fresh
-- variables. This makes the pattern usable when we substitute it, for
-- two reasons: (1) it might occur in multiple places, and we must
-- ensure that it matches the same term in all places, and (2) it
-- might occur in an expression (E where ...) where wildcards don't
-- make sense.
freshWild :: Pat -> U (Pat, VarSet)
freshWild pat = do
  v0 <- gets optNextVar
  pat' <- mapM freshWildMatch pat
  v1 <- gets optNextVar
  return (pat', IntSet.fromList [v0..v1])
  where
  freshWildMatch :: Match a Var -> U (Match a Var)
  freshWildMatch m = case m of
    MatchWild ty -> MatchBind <$> fresh ty
    MatchPrefix str rest -> MatchPrefix str <$> mapM freshWildMatch rest
    MatchSum alts -> MatchSum <$> mapM (mapM (mapM freshWildMatch)) alts
    MatchNever ty -> return (MatchNever ty)
    MatchFid f -> return (MatchFid f)
    MatchBind v -> return (MatchBind v)
    MatchVar v -> return (MatchVar v)
    MatchAnd x y -> MatchAnd <$> mapM freshWildMatch x <*> mapM freshWildMatch y
    MatchExt _ -> throwError "freshWildMatch"

data OptState = OptState
  { optNextVar :: !Int
    -- ^ for making fresh variables
  , optSubst :: Subst
    -- ^ current substitution
  , optCurrentScope :: VarSet
    -- ^ all variables visible in the current scope, excluding nested
    -- (A | B) subterms
  , optOuterScope :: VarSet
    -- ^ all variables visible in the enclosing scope, when we are
    -- unifying inside a nested (A | B) subterm.  These are the
    -- variables we must not unify (see comment at the top of the
    -- module).
  }

fresh :: Type -> U Var
fresh ty = do
  v <- gets optNextVar
  modify $ \state -> state { optNextVar = v+1 }
  return (Var ty v Nothing)

type U a = StateT OptState (Except Text) a

type VarSet = IntSet
type Subst = IntMap Pat

-- | Traverse the query, excluding nested (A | B) subterms,
-- collecting the visible variables.
queryScope :: FlatQuery -> VarSet
queryScope (FlatQuery key maybeVal groups) =
  foldr termScope (foldr f s groups) maybeVal
  where
    f group s = foldr stmtScope s group
    s = termScope key IntSet.empty

stmtScope :: FlatStatement -> VarSet -> VarSet
stmtScope (FlatStatement _ lhs rhs) r = termScope lhs (genScope rhs r)
stmtScope (FlatNegation stmts) r =
  foldr (flip (foldr stmtScope)) r stmts
stmtScope (FlatDisjunction [stmts]) r =
  foldr (flip (foldr stmtScope)) r stmts
stmtScope (FlatDisjunction _) r = r
  -- contents of or-patterns are not part of the "current scope"

genScope :: Generator -> VarSet -> VarSet
genScope (FactGenerator _ key val) r = termScope key $! termScope val r
genScope (TermGenerator pat) r = termScope pat r
genScope (DerivedFactGenerator _ key val) r = termScope key $! termScope val r
genScope (ArrayElementGenerator _ exp) r = termScope exp r
genScope (PrimCall _ args) r = foldr termScope r args

addToCurrentScope :: Var -> VarSet -> VarSet
addToCurrentScope (Var _ v _) r = IntSet.insert v r

termScope :: Pat -> VarSet -> VarSet
termScope pat r = foldr onMatch r pat
  where
  onMatch :: Match () Var -> VarSet -> VarSet
  onMatch m r = case m of
    MatchPrefix _ rest -> foldr onMatch r rest
    MatchSum alts -> foldr onAlt r alts
      where onAlt alt r = foldr termScope r alt
    MatchBind v -> addToCurrentScope v r
    MatchVar v -> addToCurrentScope v r
    MatchAnd x y -> foldr onMatch (foldr onMatch r y) x
    _ -> r


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
expandGroups :: [FlatStatementGroup] -> [FlatStatementGroup]
expandGroups = mapMaybe optGroup
  where
  optGroup g = case concatMap expandStmt (NonEmpty.toList g) of
    [] -> Nothing
    (x:xs) -> Just (x :| xs)

expandStmt :: FlatStatement -> [FlatStatement]
expandStmt (FlatStatement stmtTy lhs (TermGenerator rhs)) =
  [ FlatStatement ty a (TermGenerator b)
  | (ty,a,b) <- expand stmtTy lhs rhs ]
  where
  expand :: Type -> Pat -> Pat -> [(Type,Pat,Pat)]
  expand ty a b = case (a,b) of
    (_, Ref MatchWild{}) -> []
    (Ref MatchWild{}, _) -> []
    (Byte a, Byte b) | a == b -> []
    (Nat a, Nat b) | a == b -> []
    (String a, String b) | a == b -> []
    (Array ts, Array us)
      | Type.Array eltTy <- derefType ty
      , length ts == length us -> concat (zipWith (expand eltTy) ts us)
    (ByteArray b, ByteArray c) | b == c -> []
    (Tuple ts, Tuple us)
      | Type.Record fields <- derefType ty ->
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
    _ -> [(ty,a,b)]
expandStmt s@(FlatNegation _) = [s]
  -- the expansion is handled by @apply@ with @optStmts@ because we need to
  -- constrain the scope of the substitutions arising from inside the negation.
expandStmt (FlatDisjunction [stmts]) =
  case expandGroups stmts of
    [] -> []
    xs -> [FlatDisjunction [xs]]
  -- non-singleton disjunctions are handled by apply, which will
  -- expand the stmts via optStmts.
expandStmt s = [s]

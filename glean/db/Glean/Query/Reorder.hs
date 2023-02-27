{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DerivingStrategies #-}

module Glean.Query.Reorder
  ( reorder
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (find, fold)
import Data.Functor.Identity (Identity(..))
import qualified Data.ByteString as ByteString
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (uncons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding ((<>))

import Glean.Query.BindOrder
import Glean.Query.Codegen.Types
  ( matchVar
  , Var(..)
  , Pat
  , Match(..)
  , CgStatement_(..)
  , CgStatement
  , Generator_(..)
  , CgQuery(..)
  , QueryWithInfo(..)
  , SeekSection(..)
  , CodegenQuery)
import Glean.Query.Flatten.Types
import Glean.Query.Vars
import Glean.RTS.Term as RTS hiding (Match(..))
import Glean.RTS.Types as RTS
import qualified Glean.Database.Schema.Types as Schema

{-
Reordering pass
---------------

INPUT

A FlattenedQuery, in which the statements are grouped:

  P where group; ...; group

a group is a set of statements that may be reordered:

  group ::= [ stmt, ..., stmt ]

and a stmt is

  stmt ::= P = generator
        |  !( group; ..; group )
        |  ( ( group; ..; group ) | ... | (group; ..; group) )

generator has the form
  - a term
  - a fact generator
  - an array generator
  - a call to a primitive

Patterns contain no nested generators at this point, everything has
been lifted to statements by the flattening pass.


OUTPUT

The same, except that the groups have been flattened and we just have
sequences of statements.

In addition, the output has valid binding. Namely:

  - MatchBind will occur before MatchVar for a given variable, where "before"
    means "left-to-right top-to-bottom".

  - There are no MatchBind or MatchWild in an expression context. These are:
    - patterns on the rhs of a statement
    - the head of the query
    - the array of an array generator
    - arguments to a primitive call


HOW?

First (reorderStmtGroup): we order the stmts within each group. This
ordering phase is concerned with *efficiency*: we pick the best
ordering based on heuristics about which ordering will run the
fastest. Then, the groups are concatenated into a list.

Second (reorderStmts): attempt to order the statements in this list so
that variables are bound before they are used. Here we're potentially
changing the order of the statements that the user wrote (as opposed
to reorderStmtGroup above, which is picking an order where the user
didn't specify one, e.g. for nested generators). So we only reorder
statements when (a) we can be sure that performance will be better or
(b) there are unbound variables that force a reordering.

This phase might fail if we can't find a way to sequence the
statements. For example, there's no way to make

   _ = _

valid (although we should have eliminated this via unification
earlier) .  Similarly, there's no way to make

   X = Y

valid if neither X nor Y is bound by anything. However, if X or Y can
be bound by a later statement, then it might be possible to reorder
statements to make this valid.

In general, finding an ordering for the statements could involve
trying all the possibilities, including trying all the possibilities
for nested statement sequences, and so on. Since this is exponential
in complexity, we try to do something more efficient:

1. Try to find a statement that is a filter (an O(1) statement) with
all its variables bound. We'll do this next, because it's cheap.

2. Try to find the first statement in the list that is definitely
resolved using a cheap test.

3. Otherwise, just try to resolve the next statement. If this fails,
put the statement to the back of the queue and try the next one. If we
get all the way through the list, give up.

-}


reorder :: Schema.DbSchema -> FlattenedQuery -> Except Text CodegenQuery
reorder dbSchema QueryWithInfo{..} =
  withExcept (\(e, _) -> Text.pack $ show $
    vcat [pretty e, nest 2 $ vcat [ "in:", pretty qiQuery]]) qi
  where
    qi = do
      (q, ReorderState{..}) <-
        flip runStateT (initialReorderState qiNumVars dbSchema) $ do
          go qiQuery
      return (QueryWithInfo q roNextVar qiReturnType)

    -- 1. replace all wildcards with fresh variables
    -- 2. reorder the statements
    -- 3. replace any unused variables with wildcards
    -- 4. recover unused variable Ids
    -- (steps 3 and 4 lead to more efficient/smaller code)
    go query0 = do
      query <- reorderQuery =<< freshWildQuery query0
      let used = varsUsed query
          recover v
            | (v-1) `IntSet.member` used = v
            | otherwise = recover (v-1)
          -- TODO: this is only best-effort recovery of unused variables,
          -- really we should renumber all the variables before codegen.
      modify $ \s -> s { roNextVar = recover (roNextVar s) }
      return (reWildQuery used query)

reorderQuery :: FlatQuery -> R CgQuery
reorderQuery (FlatQuery pat _ stmts) =
  withScopeFor stmts $ do
    stmts' <- reorderGroups stmts
    pat' <- fixVars IsExpr pat
    return (CgQuery pat' stmts')

reorderGroups :: [FlatStatementGroup] -> R [CgStatement]
reorderGroups groups = do
  scope <- gets roScope
  let stmts = go scope groups
  reorderStmts stmts
  where
    go _ [] = []
    go scope (group : groups) = stmts <> go scope' groups
      where
        stmts = reorderStmtGroup scope group
        stmtsVars = IntSet.toList (vars stmts)
        scope' = foldr bind scope stmtsVars
        -- mark all variables from stmts as bound.

-- | Define a new scope.
-- Adds all variables local to the statements to the scope at the start and
-- remove them from the scope in the end.
withScopeFor :: [FlatStatementGroup] -> R a -> R a
withScopeFor stmts act = do
  outerScope <- gets roScope
  let stmtsVars = scopeVars stmts
      locals = IntSet.filter (not . isInScope outerScope) stmtsVars
      localScope = Scope $ IntMap.fromSet (const False) locals
      -- ^ add locals as unbound vars to the scope

  modify $ \s -> s { roScope = outerScope <> localScope }
  res <- act
  modify $ \s -> s { roScope = roScope s `without` localScope }
  return res
  where
    without (Scope x) (Scope y) = Scope $ IntMap.difference x y
    -- | All variables that appear in the scope these statements are in.
    -- Does not include variables local to sub-scopes such as those that only
    -- appear:
    --  - inside a negated subquery
    --  - in some but not all branches of a disjunction
    --  - in only one of 'else' or (condition + 'then') clauses of an if stmt
    scopeVars :: [FlatStatementGroup] -> VarSet
    scopeVars stmtss = foldMap (foldMap stmtScope) stmtss
      where
        stmtScope = \case
          FlatNegation{} -> mempty
          s@FlatStatement{} -> vars s
          -- only count variables that appear in all branches of the disjunction
          FlatDisjunction [] -> mempty
          FlatDisjunction (s:ss) ->
            foldr (IntSet.intersection . scopeVars) (scopeVars s) ss
          FlatConditional cond then_ else_ ->
            IntSet.intersection
              (scopeVars $ cond <> then_)
              (scopeVars else_)

{-
Note [Optimising statement groups]

A nested fact match in Angle compiles to a group of statements. For
example

   P { _, Q "abc" }

after flattening yields the group of statements

   P { _, X }
   X = Q "abc"

The purpose of reorderStmtGroup is to find a good ordering for the
statements in the group.

Things that we take into acccount are:
- A match with at most one result (point query) should probably be done early
- A match that never fails should probably be done late
- If a statement binds something that makes another statement more
  efficient, choose the ordering to exploit that

The algorithm is:

- construct a graph where the nodes are statements and
  - for each lookup statement (X = pred pat) (A)
  - for each statement that mentions X (B)
  - if pat is irrefutable
    - edge A -> B
  - if pat is a point match
    - edge B -> A
  - else, if X occurs in a prefix position in B
    - edge B -> A
  - else
    - edge A -> B
- render the postorder traversal of this graph

-}

reorderStmtGroup :: Scope -> FlatStatementGroup -> [FlatStatement]
reorderStmtGroup scope stmts =
  let
    bound = allBound scope

    nodes = zip [(0::Int)..] (NonEmpty.toList stmts)

    -- statements, numbered from zero, and with the set of variables
    -- mentioned anywhere in the statement.
    withVars :: [(Int, VarSet, FlatStatement)]
    withVars =
      [ (n, vars stmt `IntSet.difference` bound, stmt)
      | (n, stmt) <- nodes
      ]

    -- statements in this group that are candidates for reordering
    -- (X = pred pat)
    --
    -- The goal of this pass is to establish, for each candidate C of
    -- the form "X = pred pat" and each statement S that mentions X,
    -- whether C should go before or after S.
    candidates =
      [ (n, x, xs, matchType, stmt)
      | (n, xs, stmt) <- withVars
      , Just (x, matchType) <- [isCandidate stmt]
      ]

    -- Just X if the statement will compile to a fact lookup if X is bound.
    -- This is conservative and only matches simple cases, but it's enough
    -- to spot statements generated when we flatten a nested generator.
    -- At this point we also classify the match according to whether it
    -- matches at most one thing, matches everything, or something else
    -- (PatternMatch).
    isCandidate
      (FlatStatement _ (Ref v) (FactGenerator _ key _ _))
      | Just (Var _ x _) <- matchVar v =
        Just (x, classifyPattern bound key) -- TODO lookupScope here is wrong
      where bound = (`IntSet.member` lhsScope)
    isCandidate _ = Nothing

    lhsVars = IntSet.unions $
       IntSet.fromList [ x | (_, x, _, _, _) <- candidates ] :
       [ xs | (_, xs, FlatDisjunction{}) <- withVars ]
       -- we'll consider variables mentioned by disjunctions as
       -- bound. There's a liberal amount of guesswork going on here
       -- because we don't know how the disjunction should be ordered
       -- with respect to the other statements and it might depend on
       -- the ordering of the statements within the disjunction
       -- itself.

    -- for classifyPattern we want to consider the variables on the
    -- lhs of the candidates as bound. e.g.
    --
    --   X = pred { Y, _ }
    --   Y = ...
    --
    -- we want to consider { Y, _ } as a prefix match and put the
    -- binding of Y first.
    lhsScope = bound `IntSet.union` lhsVars

    -- for each statements in this group, find the variables that
    -- the statement mentions in a prefix position.
    uses =
      [ (n, xs, prefixVars lhsVars bound stmt, stmt)
      | (n, xs, stmt) <- withVars
      ]

    -- classify each candidate (X = pred P) according to whether it
    -- will be a lookup (X is bound) or a search (X is unbound).
    --
    -- If X = pred P is a lookup, then we want all the vars bound by P
    -- to also be lookups, because a lookup is O(1). So this property
    -- is propagated transitively.
    --
    -- * start from the set of bound vars,
    -- * add vars that are in non-prefix positions, or where P is a full scan
    -- * add vars from the rhs of all these candidates
    -- * keep going until we're done
    initialLookupVars = IntSet.unions
      [ bound
      , IntSet.fromList slowSearches
      , IntSet.fromList nonPrefixVars
      ]
      where
        slowSearches = [ x | (_, x, _, PatternSearchesAll, _) <- candidates ]
        nonPrefixVars =
          [ x
          | (_, _, prefix, stmt) <- uses
          , let lhs = case isCandidate stmt of
                  Just (x,_) -> Just x
                  Nothing -> Nothing
          , x <- IntSet.toList (boundVars stmt)
            -- NB. we want variables that this statement can *bind*,
            -- not all the variables it mentions.
          , Just x /= lhs && not (x `IntSet.member` prefix)
            -- only consider variables that actually have lookup
            -- statements, otherwise the assumption made by
            -- new_slow_searches below is incorrect.
          , x `IntMap.member` candidateMap
          ]

    candidateMap = IntMap.fromListWith (++)
      [ (n, [stmt]) | stmt@(_,n,_,_,_) <- candidates ]

    -- iteratively discover more variables that should be lookups,
    -- starting from initialLookupVars
    lookupVars = go (IntSet.toList initialLookupVars) IntSet.empty
      where
      go [] vars = vars
      go (x:xs) vars
        | x `IntSet.member` vars = go xs vars
        | otherwise = go (new ++ xs) (IntSet.insert x vars)
        where
        new = new_unpacks ++ new_slow_searches

        -- If X is a lookup and X = pred pat, then all vars in pat are
        -- lookups too (nested unpacking).
        new_unpacks = concat [ IntSet.toList ys | (_, _, ys, _, _) <- stmts ]
          where stmts = IntMap.findWithDefault [] x candidateMap

        -- If X is a lookup and Y = pred { X, ... }, then Y is a slow
        -- search and therefore a lookup too.
        new_slow_searches =
          [ y
          | Just stmts <- [usesOf x]
          , (_,_,FlatStatement _ (Ref v) (FactGenerator _ key _ _)) <- stmts
          , Just (Var _ y _) <- [matchVar v]
          , let bound = (`IntSet.member` IntSet.delete x lhsScope)
          , PatternSearchesAll <- [classifyPattern bound key]
          ]

    -- find the statements that mention X
    usesOf x = IntMap.lookup x m
      where
      m = IntMap.fromListWith (++)
        [ (x, [(n,ys,stmt)])
        | (n, xs, ys, stmt) <- uses
        , x <- IntSet.toList xs
        ]

    edges :: IntMap [(Int,FlatStatement)]
    edges = IntMap.fromListWith (++)
      [ if
          | PatternMatchesOne <- matchType -> (use, [(lookup,lookupStmt)])
            -- a point match: always do these first
          | isUnresolved scope useStmt -> (use, [(lookup,lookupStmt)])
            -- if the use is undefined at this point, put the lookup first
          | x `IntSet.member` lookupVars -> (lookup, [(use, useStmt)])
            -- we want X to be a lookup: do it after the use of X
          | otherwise -> (use, [(lookup,lookupStmt)])
            -- otherwise put the candidate before the use
      | (lookup, x, _, matchType, lookupStmt) <- candidates
      , Just uses <- [usesOf x]
      , (use, _, useStmt) <- uses
      , use /= lookup
      ]

    -- comment this out and import Debug.Trace for debugging
    trace _ y = y
  in
  trace ("numStmts: " <> show (length nodes)) $
  trace ("candidates: " <> show [ x | (_,x,_,_,_) <- candidates ]) $
  trace ("scope: " <> show scope) $
  trace ("initialLookupVars: " <> show initialLookupVars) $
  trace ("lookupVars: " <> show lookupVars) $

  -- order the statements and then recursively reorder nested groups
  map snd $ postorderDfs nodes edges

{- Note [Reordering negations]

A negated subquery doesn't bind values to variables in its enclosing scope.

This means that if a variable is unbound in the evaluation of a negation it
will behave as a wildcard. This has implications for variables that will be
bound later as it means that the order of statements can change the meaning
of the query.

Consider the following query:

  K where !(Q A); P A;

As it stands it will be equivalent to

  K where !(Q _); P A;

Which will fail if there is any Q fact in the database.
If we invert the order of statements it will only fail if there were
specifically a `Q A` fact in the database.

  K where P A; !(Q A);

To ensure consistent semantics regardless of the order of statements in the
source query we always move negated subqueries after the binding of all
variables from the parent scope that it uses.
-}

reorderStmts :: [FlatStatement] -> R [CgStatement]
reorderStmts stmts = iterate stmts []
  where
  iterate [] bad = mconcat <$> mapM reorderStmt (reverse bad)
    -- we already tried the bad list, so the first one should throw
  iterate stmts bad = do
    scope <- gets roScope
    let (chosen, rest) = choose scope stmts
    r <- tryError $ reorderStmt chosen
    case r of
      Left{} -> iterate rest (chosen : bad)
      Right cgChosen -> do
        -- we made some progress, so reset the bad list
        let next = if null bad then rest else rest <> reverse bad
        cgRest <- iterate next []
        return (cgChosen <> cgRest)

  tryError m = (Right <$> m) `catchError` (return . Left)

  -- Attempt to cheaply pick a good statement from the list. We try
  -- not to mess with the original order if we can avoid it, but we
  -- will pick a different statement if there's an O(1) statement we
  -- can do next, or if the current statement is definitely
  -- unresolved. If we don't know whether it's resolved, such as in
  -- the case of a disjunction, we'll fall back to just trying it.
  choose
    :: Scope
    -> [FlatStatement]
    -> (FlatStatement,[FlatStatement])
  choose _ [one] = (one, [])
  choose scope stmts = fromMaybe (error "choose") $
    find (isResolvedFilter scope . fst) stmts' <|>
    find (not . isUnresolved scope . fst) stmts' <|>
    uncons stmts
    where
      stmts' = go [] stmts

      go :: [a] -> [a] -> [(a,[a])]
      go _ [] = []
      go before (x:after) = (x, reverse before <> after) : go (x:before) after


-- | True if the statement is O(1) and resolved
isResolvedFilter :: Scope -> FlatStatement -> Bool
isResolvedFilter scope stmt = case stmt of
  FlatStatement _ _ ArrayElementGenerator{} -> False
    -- an ArrayElementGenerator is not O(1)
  _otherwise -> isReadyFilter scope stmt False

-- | True if the statement is definitely unresolved in the given
-- scope. False indicates "maybe resolved"; we'll fall back to trying
-- to resolve the stmt in reorderStmts.
isUnresolved :: Scope -> FlatStatement -> Bool
isUnresolved scope stmt = case stmt of
  FlatDisjunction{} -> False -- don't know
  FlatStatement _ _ (ArrayElementGenerator _ arr) -> not (patIsBound scope arr)
  _otherwise -> not (isReadyFilter scope stmt True)

isReadyFilter :: Scope -> FlatStatement -> Bool -> Bool
isReadyFilter scope stmt notFilter = case stmt of
  FlatDisjunction [one] -> all (all isReady) one
    where isReady stmt = isReadyFilter scope stmt notFilter
    -- Don't hoist a disjunction with multiple alts, even if they're
    -- all resolved, because that might duplicate work.
  FlatStatement _ lhs (TermGenerator rhs) ->
    patIsBound scope lhs || patIsBound scope rhs
  FlatStatement _ _ (PrimCall _ args) ->
    all (patIsBound scope) args
  FlatStatement _ _ (DerivedFactGenerator _ key val) ->
    patIsBound scope key && patIsBound scope val
  FlatNegation stmtss ->
    -- See Note [Reordering negations]
    all (all isReady) stmtss && hasAllNonLocalsBound
    where
      isReady stmt = isReadyFilter scope stmt notFilter
      appearInStmts = foldMap (foldMap vars) stmtss
      hasAllNonLocalsBound =
        IntSet.null $
        IntSet.filter (\var -> isInScope scope var && not (isBound scope var))
        appearInStmts
  _ -> notFilter

isInScope :: Scope -> Variable -> Bool
isInScope (Scope scope) var = var `IntMap.member` scope

allBound :: Scope -> VarSet
allBound (Scope scope) = IntMap.keysSet $ IntMap.filter id scope

allVars :: Scope -> VarSet
allVars (Scope scope) = IntMap.keysSet scope

patIsBound :: Scope -> Pat -> Bool
patIsBound scope pat
  | PatternMatchesOne <- classifyPattern (isBound scope) pat = True
  | otherwise = False

data PatternMatch
  = PatternSearchesAll
    -- ^ no prefix; looks at all the facts
  | PatternMatchesOne
    -- ^ point-match: matches at most one value
  | PatternMatchesSome
    -- ^ neither of the above

-- | Classify a pattern according to the cases in 'PatternMatch'
classifyPattern
  :: (Int -> Bool) -- ^ variable is bound?
  -> Term (Match () Var)
  -> PatternMatch
classifyPattern bound t = fromMaybe PatternMatchesOne (go False t end)
  where
  go
    :: Bool -- non-empty fixed prefix seen?
    -> Term (Match () Var)
    -> (Bool -> Maybe PatternMatch)  -- cont
    -> Maybe PatternMatch
       -- Nothing -> pattern was empty
       -- Just p -> non-empty pattern of kind p
  go pref t r = case t of
    Byte{} -> fixed r
    Nat{} -> fixed r
    Array xs -> termSeq pref xs r
    ByteArray{} -> fixed r
    Tuple xs -> termSeq pref xs r
    Alt _ t -> fixed (\pref -> go pref t r)
    String{} -> fixed r
    Ref m -> case m of
      MatchWild{} -> wild pref
      MatchNever{} -> Just PatternMatchesOne
      MatchFid{} -> fixed r
      MatchBind (Var _ v _) -> var v
      MatchVar (Var _ v _) -> var v
      MatchAnd a b ->
        case (go pref a end, go pref b end) of
          (Nothing, _) -> r False
          (_, Nothing) -> r False
          (Just PatternMatchesOne, _) -> r True
          (_, Just PatternMatchesOne) -> r True
          (Just PatternMatchesSome, _) -> Just PatternMatchesSome -- stop here
          (_, Just PatternMatchesSome) -> Just PatternMatchesSome -- stop here
          (Just PatternSearchesAll, Just PatternSearchesAll) ->
             Just PatternSearchesAll -- stop here
      MatchPrefix s t
        | not (ByteString.null s) -> fixed (\pref' -> go pref' t r)
        | otherwise -> go pref t r
      -- MatchArrayPrefix doesn't actually look at a prefix because
      -- arrays encode their length at the front
      MatchArrayPrefix{} -> wild pref
      MatchExt{} -> Just PatternMatchesSome
    where
    var v
      | bound v = fixed r
      | otherwise = wild pref

  -- we've seen a bit of fixed pattern
  fixed r = r True

  -- we've seen a bit of wild pattern
  wild nonEmptyPrefix
    | nonEmptyPrefix = Just PatternMatchesSome -- stop here
    | otherwise = Just PatternSearchesAll -- stop here

  -- end of the pattern
  end nonEmptyPrefix
    | nonEmptyPrefix = Just PatternMatchesOne
    | otherwise = Nothing

  termSeq pref [] r = r pref
  termSeq pref (x:xs) r = go pref x (\pref -> termSeq pref xs r)


-- | Determine the set of variables that occur in a prefix position in
-- a pattern.
prefixVars
  :: VarSet
     -- ^ bound variables that we're interested in
  -> VarSet
     -- ^ bound variables that we're not interested in
  -> FlatStatement
  -> VarSet
prefixVars lookups scope stmt = prefixVarsStmt stmt
  where
  prefixVarsStmt (FlatStatement _ _ (FactGenerator _ key _ _)) =
      prefixVarsTerm key IntSet.empty
  prefixVarsStmt FlatStatement{} = IntSet.empty
  prefixVarsStmt (FlatNegation stmtss) = prefixVarsStmts stmtss
  prefixVarsStmt (FlatDisjunction stmtsss) = foldMap prefixVarsStmts stmtsss
  prefixVarsStmt (FlatConditional cond then_ else_) =
    foldMap prefixVarsStmts [cond, then_, else_]

  prefixVarsStmts :: [FlatStatementGroup] -> VarSet
  prefixVarsStmts stmtss =
    IntSet.unions
      [ prefixVarsStmt stmt
      | stmts <- stmtss
      , stmt <- NonEmpty.toList stmts
      ]

  prefixVarsTerm :: Term (Match () Var) -> VarSet -> VarSet
  prefixVarsTerm t r = case t of
    Byte{} -> r
    Nat{} -> r
    Array xs -> foldr prefixVarsTerm r xs
    ByteArray{} -> r
    Tuple xs -> foldr prefixVarsTerm r xs
    Alt _ t -> prefixVarsTerm t r
    String{} -> r
    Ref m -> prefixVarsMatch m r

  prefixVarsMatch :: Match () Var -> VarSet -> VarSet
  prefixVarsMatch m r = case m of
    MatchWild{} -> IntSet.empty
    MatchNever{} -> IntSet.empty
    MatchFid{} -> r
    MatchBind (Var _ v _) -> var v
    MatchVar (Var _ v _) -> var v
    MatchAnd a b -> prefixVarsTerm a r `IntSet.union` prefixVarsTerm b r
    MatchPrefix _ t -> prefixVarsTerm t r
    MatchArrayPrefix _ty pre -> foldr prefixVarsTerm r pre
    MatchExt{} -> IntSet.empty
    where
    var v
      -- already bound: we're still in the prefix
      | v `IntSet.member` scope = r
      -- one of the lookups in this group: add to our list of prefix vars
      | v `IntSet.member` lookups = IntSet.insert v r
      -- unbound: this is the end of the prefix
      | otherwise = IntSet.empty

postorderDfs :: forall a. [(Int,a)] -> IntMap [(Int,a)] -> [(Int,a)]
postorderDfs nodes edges = go IntSet.empty nodes (\_ -> [])
  where
  go :: IntSet -> [(Int,a)] -> (IntSet -> [(Int,a)]) -> [(Int,a)]
  go seen [] cont = cont seen
  go seen ((n,a):xs) cont
    | n `IntSet.member` seen = go seen xs cont
    | otherwise = go (IntSet.insert n seen) children
        (\seen -> (n,a) : go seen xs cont)
    where
    children = IntMap.findWithDefault [] n edges

-- | Decide whether to flip a statement or not.
--
-- For a statement P = Q we will try both P = Q and Q = P to find a
-- form that has valid binding (no unbound variables or wildcards in
-- expressions).
--
-- There's a bit of delicacy around which one we try first. Choosing
-- the right one may lead to better code. For example:
--
--    cxx.Name "foo" = X
--
-- if X is bound, we can choose whether to flip or not. But if we
-- don't flip this, the generator on the left will be bound separately
-- by toCgStatement to give
--
--    Y = X
--    Y = cxx.Name "foo"
--
-- it would be better to flip the original statement to give
--
--    X = cxx.Name "foo"
--
-- More generally, if we have generators on the left but not the
-- right, we should probably flip.  If we have generators on both
-- sides, let's conservatively try not flipping first.
--
reorderStmt :: FlatStatement -> R [CgStatement]
reorderStmt stmt@(FlatStatement ty lhs gen)
  | Just flip <- canFlip =
    noflip `catchErrorRestore` \e ->
      flip `catchErrorRestore` \e' ->
        attemptBindFromType e noflip `catchErrorRestore` \_ ->
          attemptBindFromType e' flip `catchErrorRestore` \_ ->
            giveUp e
  -- If this statement can't be flipped, we may still need to bind
  -- unbound variables:
  | otherwise =
    noflip `catchErrorRestore` \e ->
      attemptBindFromType e noflip `catchErrorRestore` \_ ->
         giveUp e
  where
  catchErrorRestore x f = do
    state0 <- get
    let restore = put state0
    x `catchError` \e -> restore >> f e

  noflip = toCgStatement (FlatStatement ty lhs gen)
  canFlip
    | TermGenerator rhs <- gen
    = Just $ toCgStatement (FlatStatement ty rhs (TermGenerator lhs))
    | otherwise
    = Nothing

  giveUp (s, e) =
    throwError (errMsg s, e)
  errMsg s = Text.pack $ show $ vcat
    [ nest 2 $ vcat ["cannot resolve:", pretty stmt]
    , nest 2 $ vcat ["because:", pretty s]
    ]

  -- In general if we have X = Y where both X and Y are unbound (or LHS = RHS
  -- containing unbound variables on both sides) then we have no choice
  -- but to return an error message. However in the specific case that we
  -- know the type of X or Y is a predicate then we can add the statement
  -- X = p _ to bind it and retry.
  --
  -- Termination is guaranteed as we strictly decrease the number of unbound
  -- variables each time
  attemptBindFromType e f
    | (_, Just (UnboundVariable var@(Var ty _ _))) <- e
    , RTS.PredicateRep pid <- RTS.repType ty = tryBindPredicate var pid
    | otherwise =
      throwError e
    where
    tryBindPredicate var pid = do
      state <- get
      details <- case Schema.lookupPid pid $ roDbSchema state of
          Nothing ->
            lift $ throwError
              ( "internal error: bindUnboundPredicates: " <>
                  Text.pack (show pid)
              , Nothing )

          Just details@Schema.PredicateDetails{} -> do return details

      bindVar var
      stmts <- f `catchErrorRestore` \e' -> attemptBindFromType e' f
      let
        pid = Schema.predicatePid details
        ref = Schema.predicateId details
        p = PidRef pid ref
        tyKey = Schema.predicateKeyType details
        tyValue = Schema.predicateValueType details
        pat =
          FactGenerator p
            (Ref (MatchWild tyKey))
            (Ref (MatchWild tyValue))
            SeekOnAllFacts
      -- V = p {key=_, value=_}
      -- LHS = RHS
      return $ CgStatement (Ref (MatchBind var)) pat : stmts

    bindVar :: Var -> R ()
    bindVar (Var _ v _) = modify $ \s -> s { roScope = bind v $ roScope s }

-- fallback: just convert other statements to CgStatement
reorderStmt stmt = toCgStatement stmt

toCgStatement :: FlatStatement -> R [CgStatement]
toCgStatement stmt = case stmt of
  FlatStatement _ lhs gen -> do
    gen' <- fixVars IsExpr gen -- NB. do this first!
    lhs' <- fixVars IsPat lhs
    return [CgStatement lhs' gen']
  FlatNegation stmts -> do
    stmts' <-
      withinNegation $
      withScopeFor stmts $
      reorderGroups stmts
    return [CgNegation stmts']
  FlatDisjunction [stmts] ->
    withScopeFor stmts $ reorderGroups stmts
  FlatDisjunction stmtss -> do
    cg <- map runIdentity <$> intersectBindings (map Identity stmtss)
    return [CgDisjunction cg]
  FlatConditional cond then_ else_ -> do
    r <- intersectBindings [[ cond, then_ ], [ else_ ]]
    case r of
      [[cond', then'], [else']] -> return [CgConditional cond' then' else']
      _ -> error "unexpected length returned by intersectBindings"
  where

  intersectBindings :: (Traversable t, Foldable t) =>
    [t [FlatStatementGroup]] -> R [t [CgStatement]]
  intersectBindings [] = return []
  intersectBindings groupss = do
    initialScope <- gets roScope
    results <- forM groupss $ \tgroup -> do
        modify $ \state -> state { roScope = initialScope }
        tstmts <- withScopeFor (fold tgroup) $ traverse reorderGroups tgroup
        newScope <- gets roScope
        return (tstmts, allBound newScope)

    let boundInAllBranches = foldr1 IntSet.intersection (map snd results)
    tstmtss <- forM results $ \(tstmts, boundInThisBranch) -> do
      let needsRenaming = IntSet.difference boundInThisBranch boundInAllBranches
      traverse (renameAlt needsRenaming) tstmts

    let newScope = foldr bind initialScope $ IntSet.toList boundInAllBranches
    modify $ \state -> state { roScope = newScope }
    return tstmtss


  -- Rename local variables in each branch of |. See Note [local variables].
  renameAlt :: IntSet -> [CgStatement] -> R [CgStatement]
  renameAlt vars stmts = do
    let n = IntSet.size vars
    state@ReorderState{..} <- get
    put state { roNextVar = roNextVar + n }
    let env = IntMap.fromList (zip (IntSet.toList vars) [ roNextVar .. ])
    return (map (fmap (rename env)) stmts)

  rename :: IntMap Int -> Var -> Var
  rename env v@(Var ty x nm) = case IntMap.lookup x env of
    Nothing -> v
    Just y -> Var ty y nm

-- | Keep track of variables in the scope outside of the negation so that we
-- can make reordering decisions about variables local to the negation.
-- See Note [Reordering negations]
withinNegation :: R a -> R a
withinNegation act = do
  before <- get
  modify $ \s -> s { roNegationEnclosingScope = allVars $ roScope before }
  res <- act
  modify $ \s -> s { roNegationEnclosingScope = roNegationEnclosingScope before }
  return res

{- Note [local variables]

This is a legit query:

  X = "a" | (X where cxx.Name X)

This looks dodgy because X only appears in one branch on the rhs, but
in fact it's fine:
* (X where cxx.Name X) is reasonable
* "a" | (X where cxx.Name X) is reasonable, but doesn't bind X
* therefore in X = "a" | (X where cxx.Name X), the X on the left is binding.

You might think "let's reject it".  But even if the user didn't write
it like this, We might end up here after query optimisation, e.g. it
can start as

  X = "a" | (Z where cxx.Name Z)

and then unification will replace [X/Z]. Should we avoid doing that?
It seems hard to avoid while still doing all the useful optimisation
we want. e.g. in

  X = cxx.Name "foo"
  { X, Y } = { N, cxx.RecordDeclaraiton { name = N } } | ...

we'd really like N to unify with X.

So, let's just make it work.  If we do the naive thing and map X to a
variable _0, the codegen will see

  _0:string = "a" | (_0 where cxx.Name _0:string)

and it will generate bogus code, because the value we build in _0 on
the left depends on a value in _0 on the right.

To fix this we need to identify variables that are local to one side
of | and rename them so they can't clash with variables mentioned
elsewhere.  A "local" variable is one that isn't bound by both
branches of |.
-}


fixVars :: FixBindOrder a => IsPat -> a -> R a
fixVars isPat p = do
  state <- get
  let scope = roScope state
      noBind = NoBind (roNegationEnclosingScope state)
  (p', scope') <-
    lift $
      withExcept (\err -> (errMsg err, Just err)) $
      runFixBindOrder scope noBind (fixBindOrder isPat p)
  modify $ \s -> s { roScope = scope' }
  return p'
  where
    errMsg err = case err of
      UnboundVariable v@(Var ty _ _) ->
        "unbound variable: " <>
        Text.pack (show (pretty v <+> ":" <+> pretty ty))
      CannotUseWildcardInExpr -> "cannot use a wildcard in an expression"
      CannotUseNeverInExpr -> "cannot use 'never' in an expression"


data ReorderState = ReorderState
  { roNextVar :: !Int
  , roDbSchema :: Schema.DbSchema
  , roScope :: Scope
  , roNegationEnclosingScope :: VarSet
    -- ^ variables non-local to the current negated subquery
  }

type R a = StateT ReorderState (Except (Text, Maybe FixBindOrderError)) a

instance Monad m => Fresh (StateT ReorderState m) where
  peek = gets roNextVar
  alloc = do
    state@ReorderState{..} <- get
    put state{ roNextVar = roNextVar + 1 }
    return roNextVar

initialReorderState :: Int -> Schema.DbSchema -> ReorderState
initialReorderState nextVar dbSchema = ReorderState
  { roNextVar = nextVar
  , roScope = mempty
  , roDbSchema = dbSchema
  , roNegationEnclosingScope = mempty
  }

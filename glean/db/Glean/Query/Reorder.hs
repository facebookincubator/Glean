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
import Data.Foldable (find, toList)
import Data.Functor.Identity (Identity(..))
import qualified Data.ByteString as ByteString
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (uncons, partition, unzip4)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Compat.Prettyprinter hiding ((<>))

import Glean.Query.BindOrder
import Glean.Query.Codegen.Types
import Glean.Display
import Glean.Query.Flatten.Types
import Glean.Query.Vars
import Glean.RTS.Term as RTS
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
    vcat [pretty e, nest 2 $ vcat [ "in:", displayDefault qiQuery]]) qi
  where
    qi = do
      ((q,gen), ReorderState{..}) <-
        flip runStateT (initialReorderState qiNumVars dbSchema) $ do
          go qiQuery
      return (QueryWithInfo q roNextVar gen qiReturnType)

    -- 1. replace all wildcards with fresh variables
    -- 2. reorder the statements
    -- 3. replace any unused variables with wildcards, and
    --    renumber variables (this leads to more efficient/smaller code)
    go query0 = do
      query <- reorderQuery query0
      let used = varsUsed query <> foldMap vars qiGenerator
          varMap = IntMap.fromList (zip (IntSet.toList used) [0..])
      modify $ \s -> s { roNextVar = IntSet.size used }
      return (reWildQuery varMap query, reWildGenerator varMap <$> qiGenerator)

reorderQuery :: FlatQuery -> R CgQuery
reorderQuery (FlatQuery pat _ stmts) =
  withScopeFor [stmts] $ do
    stmts' <- reorderGroup stmts
    (extra, pat') <- resolved `catchError` \e ->
      maybeBindUnboundPredicate e resolved
    return (CgQuery pat' (extra <> stmts'))
  where
    resolved = do pat' <- fixVars IsExpr pat; return ([], pat')


reorderGroup :: FlatStatementGroup -> R [CgStatement]
reorderGroup g = do
  scope <- gets roScope
  let
    bound0 = IntMap.keysSet (allBound scope)

    -- we'll call reorderStmtGroup on the (unordered) group before
    -- reordering any nested groups. There's a chicken/egg problem here:
    --   - if we reorder the outer group first then we don't have an
    --     accurate idea of the cost of the inner groups
    --   - if we reorder the inner group first then we don't have an
    --     accurate idea of the bound variables
    -- but overall it seems like doing the outer group first gives
    -- better results. Perhaps a multi-pass approach would be even better.
    group bound (FlatStatementGroup ord float) = do
      let reordered = reorderStmtGroup (isScope scope) bound ord float
      (stmts,bound') <- go bound reordered
      return (FlatStatementGroup stmts [], bound')

    go bound [] = return ([], bound)
    go bound (FlatDisjunction [g] : rest) = do
      (g', bound') <- group bound g
      (stmts'',bound'') <- go bound' rest
      return (FlatDisjunction [g'] : stmts'', bound'')
    go bound (stmt : rest) = do
      (stmts', bound') <- go (IntSet.union (boundVars stmt) bound) rest
      return (stmt : stmts', bound')

    squash [] = []
    squash (stmt : rest) = case stmt of
      FlatDisjunction [FlatStatementGroup stmts _] ->
        squash (stmts <> rest)
      _other -> stmt : squash rest

  (FlatStatementGroup stmts _, _) <- group bound0 g
  reorderStmts (squash stmts)

-- | Define a new scope.
-- Adds all variables local to the statements to the scope at the start and
-- remove them from the scope in the end.
withScopeFor :: [FlatStatementGroup] -> R a -> R a
withScopeFor stmts act = do
  Scope outerScope bound <- gets roScope
  let stmtsVars = foldMap scopeVars stmts
      locals = IntSet.filter (`IntSet.notMember` outerScope) stmtsVars

  modify $ \s -> s { roScope = Scope (outerScope <> locals) bound }
  res <- act
  modify $ \s -> s { roScope = roScope s `without` locals }
  return res
  where
    without (Scope scope bound) x =
      Scope (IntSet.difference scope x)
        (IntMap.filterWithKey (\v _ -> v `IntSet.notMember` x) bound)
    -- | All variables that appear in the scope these statements are in.
    -- Does not include variables local to sub-scopes such as those that only
    -- appear:
    --  - inside a negated subquery
    --  - in some but not all branches of a disjunction
    --  - in only one of 'else' or (condition + 'then') clauses of an if stmt
    scopeVars :: FlatStatementGroup -> VarSet
    scopeVars (FlatStatementGroup ord float) =
      foldMap stmtScope ord <> foldMap stmtScope float
      where
        stmtScope = \case
          FlatNegation{} -> mempty
          s@FlatStatement{} -> vars s
          s@FlatAllStatement{} -> vars s
          -- only count variables that appear in all branches of the disjunction
          FlatDisjunction [] -> mempty
          FlatDisjunction (s:ss) ->
            foldr (IntSet.intersection . scopeVars) (scopeVars s) ss
          FlatConditional cond then_ else_ ->
            IntSet.intersection
              (scopeVars cond <> scopeVars then_)
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

The algorithm is

  1. first choose lookups (X = pred P, where X is bound)
  2. repeat step 1, binding more variables until there are no more lookups
  3. classify all the remaining statements by cost
  4. choose statements in the following order. If we choose any
     statements, then go back to 1 to order the remaining statements.
     a. filters (e.g. X = 1)
     b. point matches (e.g. X = pred "abc")
     c. prefix matches (e.g. X = pred { 1, _ })
     d. full scans (e.g. X = pred _)
     e. unresolved
-}

data StmtCost
  = StmtFilter
  | StmtLookup
  | StmtPointMatch
  | StmtPrefixFactMatch
  | StmtPrefixMatch
  | StmtScan
  | StmtUnresolved
  deriving (Bounded, Eq, Show, Ord)

reorderStmtGroup
  :: VarSet
  -> VarSet
  -> [FlatStatement]  -- these must remain in this order
  -> [FlatStatement]  -- these can float anywhere
  -> [FlatStatement]  -- final ordering
reorderStmtGroup _ _ ordered [] = ordered
reorderStmtGroup sc bound ordered floating =
  let
    (lookups, others) = partitionStmts (map summarise floating)
    ord = map summarise ordered
    r = layout (IntSet.toList bound) bound lookups others ord
  in
  trace (show (vcat [
    "reorderStmtGroup: " <> pretty (show bound),
    indent 2 (displayDefault (FlatStatementGroup ordered floating)),
    "===>",
    indent 2 (vcat (map displayDefault r)) ])) r
  where
    trace _ x = x

    summarise
      :: FlatStatement
      -> (Maybe VarId, VarSet, Maybe StmtCost, FlatStatement)
    summarise stmt = case stmt of
      FlatStatement _ lhs rhs -> (maybeVar, bound, Nothing, stmt)
        where
        maybeVar = case (lhs,rhs) of
          (Ref v, FactGenerator{}) | Just (Var _ x _) <- matchVar v -> Just x
          _otherwise -> Nothing
      FlatAllStatement (Var _ v _) _ _ -> (Just v,bound, Nothing, stmt)
      FlatDisjunction [_one] -> (Nothing, bound, Nothing, stmt)
        -- TODO: we could recursively reorder these nested groups as
        -- with disjunctions below, but it caused various regressions
        -- when I tried it.
      FlatDisjunction groups -> (Nothing, bound, Just cost, stmt)
        where
        -- to get a baseline cost for a disjunction, we recursively
        -- reorder it in the enclosing context, and then use this cost
        -- as a minimum. This is cheaper and less accurate than
        -- recursively reordering every time we want to classify it,
        -- but gives better results than not reordering recursively at
        -- all.
        cost = classify bound $ FlatDisjunction
          [ FlatStatementGroup (reorderStmtGroup sc bound ord float) []
          | FlatStatementGroup ord float <- groups ]
      FlatNegation{} ->  (Nothing, bound, Nothing, stmt)
      FlatConditional{} ->  (Nothing, bound, Nothing, stmt)
      where
      bound = boundVars stmt

    partitionStmts
       :: [(Maybe VarId, VarSet, Maybe StmtCost, FlatStatement)]
       -> (
            IntMap [(VarSet, FlatStatement)],
            [(VarSet, Maybe StmtCost, FlatStatement)]
          )
    partitionStmts summaries = (lookups, others)
      where
      lookups = IntMap.fromListWith (<>)
        [ (v, [(bound, stmt)]) | (Just v, bound, _, stmt) <- summaries ]
      others = [ (bound, cost, stmt) | (Nothing, bound, cost, stmt) <- summaries ]

    classify :: VarSet -> FlatStatement -> StmtCost
    classify bound (FlatStatement _ lhs (FactGenerator _ key _ _))
      | Just (Var _ v _) <- isVar lhs, v `IntSet.member` bound = StmtPointMatch
      | otherwise =
      case classifyPattern ((`IntSet.member` bound) . varId) key of
        PatternMatch _ Point -> StmtPointMatch
        PatternMatch PrefixFactId Scan -> StmtPrefixFactMatch
        PatternMatch PrefixFixed Scan -> StmtPrefixMatch
        PatternMatch _ Scan -> StmtScan
    classify _ (FlatDisjunction []) = StmtFilter -- False
    classify bound stmt
      | isResolvedFilter inScope stmt = StmtFilter
      | isCurrentlyUnresolved inScope stmt = StmtUnresolved
      where inScope = mkInScopeForClassify sc bound
    classify bound (FlatDisjunction alts@(_:tail)) =
      -- we classify a disjunction as a prefix match even if the
      -- individual brannches are O(1). In particular this means we
      -- won't pull a disjunction out of order just because the
      -- branches are filters, which can make things worse.
      if not (null tail)
        then max StmtPrefixFactMatch maxCost
        else maxCost
      where maxCost = maximum (map (classifyGroup bound) alts)
    classify _ (FlatStatement _ _ ArrayElementGenerator{}) = StmtPrefixMatch
    classify _ (FlatStatement _ _ SetElementGenerator{}) = StmtPrefixMatch
    classify bound (FlatNegation g) = classifyGroup bound g
    classify _ _ = StmtScan

    -- Approximate classification of groups. To be more
    -- accurate we would have to recursively reorder the
    -- statements in the group. TODO: recursively reorder
    -- but only if there are no O(1) statements in the group.
    -- We do attempt to reorder unresolved statements on the fly
    -- here; this turned out to be necessary in some cases.
    classifyGroup bound (FlatStatementGroup ord float) =
      go bound (float <> ord) minBound []
      where
      go _ [] cost bad = if null bad then cost else StmtUnresolved
      go bound (stmt : stmts) cost bad
        | stmtCost == StmtUnresolved = go bound stmts cost (stmt : bad)
        | otherwise = go bound' (stmts <> bad) (max cost stmtCost) []
          where
          stmtCost = classify bound stmt
          bound' = boundVars stmt `IntSet.union` bound

    layout
      :: [VarId]
      -> VarSet
      -> IntMap [(VarSet, FlatStatement)]
      -> [(VarSet, Maybe StmtCost, FlatStatement)] -- floating
      -> [(Maybe VarId, VarSet, Maybe StmtCost, FlatStatement)] -- ordered
      -> [FlatStatement]
    layout [] _ lookups [] [] | IntMap.null lookups = []
    layout (x:xs) bound lookups others ord =
      case IntMap.lookup x lookups of
        Nothing -> layout xs (IntSet.insert x bound) lookups others ord
        Just some ->
          stmts <> layout (new <> xs) bound' lookups' others ord
          where
          (varss, stmts) = unzip some
          allVars = IntSet.unions varss
          new = filter (`IntSet.notMember` bound) (IntSet.toList allVars)
          bound' = IntSet.union allVars bound
          lookups' = IntMap.delete x lookups
    layout [] bound lookups others ord =
      let
        classified =
          [ (classify bound stmt, (Just var, vars, Nothing, stmt))
          | (var, some) <- IntMap.toList lookups, (vars, stmt) <- some ] <>
          [ (maybe id min cost (classify bound stmt), (Nothing, vars, cost, stmt))
          | (vars, cost, stmt) <- others ]

        classifiedOrd =
          [ (maybe id min cost (classify bound stmt), (lkp, vars, cost, stmt))
          | (lkp, vars, cost, stmt) <- ord ]

        partitionByCost wanted stmts = (map snd yes, map snd no)
          where
          want (cost, _) = cost == wanted
          !(yes,no) = partition want stmts

        -- all statements with the desired cost
        chooseAll wanted orElse
          | null stmts = orElse
          | otherwise =
            chosen wanted (found <> foundOrd) rejected rejectedOrd $
              stmts <> layout newBound bound lookups others rejectedOrd
          where
          !(found, rejected) = partitionByCost wanted classified
          !(foundOrd, rejectedOrd) = partitionByCost wanted classifiedOrd
          !(_, varss, _, stmts) = unzip4 (found <> foundOrd)
          newBound = concatMap IntSet.toList varss
          !(lookups, others) = partitionStmts rejected

        -- the "best" statement with the desired cost is one that
        -- won't be made cheaper if we pick another statement
        chooseBest wanted orElse = go found []
          where
          !(found, rejected) = partitionByCost wanted classified

          this vars stmt rest =
            chosen wanted [(wanted,vars,Nothing,stmt)] (rest <> rejected) ord $
              chooseOne vars stmt (rest <> rejected)

          -- pick a statement that isn't bound by some other statement
          go [] _ = case findOrd wanted classifiedOrd of
            Just ((_, vars, _, stmt), ord') ->
              this vars stmt found ord'
            Nothing -> case found of
              [] -> orElse
              (_, vars, _, stmt) : rest -> this vars stmt rest ord
          go ((Nothing, vars, _, stmt) : rest) other =
            this vars stmt (rest <> other) ord
          go (info@(Just var, vars, _, stmt) : rest) other
            | not (boundBySomething var) = this vars stmt (rest <> other) ord
            | otherwise = go rest (info : other)

          boundBySomething v =
            any boundBy (found <> rejected) || any boundBy ord
            where
            boundBy (Just v', _, _, _) | v == v' = False
            boundBy (_, vars, _, _) = v `IntSet.member` vars

        -- just logging, for debugging
        chosen wanted found rejected ord =
          trace (show $ vcat [
            "picked: " <> pretty (show wanted),
            indent 2 (dumpStmts found),
            "rejected:",
            indent 2 (dumpStmts rejected),
            "ord:",
            indent 2 (dumpStmts ord)])
          where
          dumpStmts stmts =
            vcat [ displayDefault stmt | (_,_,_,stmt) <- stmts ]

        chooseOne vars stmt rest ord =
          stmt : layout (IntSet.toList vars) bound lookups others ord
          where
          (lookups, others) = partitionStmts rest

        chooseNext
          | (_, vars, _, stmt) : rest <- map snd classified =
            chooseOne vars stmt rest ord
          | (_, vars, _, stmt) : rest <- ord =
            chooseOne vars stmt [] rest
          | otherwise =
            error "chooseNext"

        -- If there was no good floating statement to use, check if
        -- the next ordered statement is good.
        findOrd wanted ord = go ord
          where
          -- We allow skipping over unresolved statements in the
          -- ordered list, because those will be reordered by the
          -- later pass anyway.
          go [] = Nothing
          go ((cost, s@(_, _, _, stmt)) : more)
            | cost == wanted =
              trace ("findOrd selecting: " <> show cost <> ": " <>
                show (displayDefault stmt)) $
              Just (s, map snd more)
            | cost == StmtUnresolved = case go more of
              Just (picked, rest) -> Just (picked, s : rest)
              Nothing -> Nothing
            | otherwise =
              trace ("findOrd ignoring: " <> show cost <> ": " <>
                show (displayDefault stmt))
              Nothing
      in
      chooseAll StmtFilter $
      chooseAll StmtPointMatch $
      chooseBest StmtPrefixFactMatch $
      chooseBest StmtPrefixMatch $
      chooseBest StmtScan
      chooseNext
      -- TODO: only classify Disjunction if there are no O(1) stmts

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
    let (desc, (chosen, rest)) = choose scope stmts
    trace (show (vcat [
      "choose:",
      indent 2 (vcat (map displayDefault stmts)),
      pretty desc <> ": " <> displayDefault chosen])) $ return ()
    r <- tryError $ reorderStmt chosen
    case r of
      Left{} -> trace ("bad: " <> show (displayDefault chosen)) $ iterate rest (chosen : bad)
      Right cgChosen -> do
        -- we made some progress, so reset the bad list
        let next = if null bad then rest else rest <> reverse bad
        cgRest <- iterate next []
        return (cgChosen <> cgRest)

  trace _ x = x -- comment out to debug

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
    -> (Text, (FlatStatement, [FlatStatement]))
  choose _ [one] = ("only", (one, []))
  choose scope stmts = fromMaybe (error "choose") $
    firstResolved <|>
    firstNotUnresolved <|>
    firstNotUnresolvedLenient <|>
    fallback
    where
      firstResolved = ("resolved",) <$>
        find (isResolvedFilter (ifBoundOnly scope) . fst) stmts'

      -- try to find a statement that's already resolved without
      -- adding any missing generators, and then try again allowing
      -- unbound predicates to be resolved.
      firstNotUnresolved = ("not unresolved",) <$>
        find (not . isUnresolved (ifBoundOnly scope) . fst) stmts'

      firstNotUnresolvedLenient = ("not unresolved lenient",) <$>
        find (not . isUnresolved (allowUnboundPredicates scope) . fst) stmts'

      fallback = ("take first",) <$> uncons stmts

      stmts' = go [] stmts

      go :: [a] -> [a] -> [(a,[a])]
      go _ [] = []
      go before (x:after) = (x, reverse before <> after) : go (x:before) after


-- | True if the statement is O(1) and resolved
isResolvedFilter :: InScope -> FlatStatement -> Bool
isResolvedFilter scope stmt = case stmt of
  FlatStatement _ _ ArrayElementGenerator{} -> False
  FlatStatement _ _ SetElementGenerator{} -> False
    -- an ArrayElementGenerator is not O(1)
  _otherwise -> isReadyFilter scope stmt False

-- | True if the statement is definitely unresolved in the given
-- scope. False indicates "maybe resolved"; we'll fall back to trying
-- to resolve the stmt in reorderStmts.
isUnresolved :: InScope -> FlatStatement -> Bool
isUnresolved inScope stmt = case stmt of
  FlatDisjunction{} -> False -- don't know
  FlatStatement _ _ (ArrayElementGenerator _ arr) ->
    not (patIsBound inScope arr)
  FlatStatement _ _ (SetElementGenerator _ arr) ->
    not (patIsBound inScope arr)
  _otherwise -> not (isReadyFilter inScope stmt True)

isCurrentlyUnresolved :: InScope -> FlatStatement -> Bool
isCurrentlyUnresolved scope stmt = case stmt of
  FlatDisjunction{} -> False -- don't know
  FlatStatement _ _ (ArrayElementGenerator _ arr) ->
    not (patIsBound scope arr)
  FlatStatement _ _ (SetElementGenerator _ arr) ->
    not (patIsBound scope arr)
  _otherwise -> not (isReadyFilter scope stmt True)

isReadyFilter :: InScope -> FlatStatement -> Bool -> Bool
isReadyFilter scope stmt notFilter = case stmt of
  FlatDisjunction [FlatStatementGroup ord float] ->
    all isReady ord && all isReady float
    where isReady stmt = isReadyFilter scope stmt notFilter
    -- Don't hoist a disjunction with multiple alts, even if they're
    -- all resolved, because that might duplicate work.
  FlatStatement _ lhs (TermGenerator rhs) ->
    patIsBound scope lhs || patIsBound scope rhs
  FlatStatement _ _ (PrimCall _ args _) ->
    all (patIsBound scope) args
  FlatStatement _ _ (DerivedFactGenerator _ key val) ->
    patIsBound scope key && patIsBound scope val
  FlatNegation (FlatStatementGroup ord float) ->
    -- See Note [Reordering negations]
    all isReady ord && all isReady float && hasAllNonLocalsBound
    where
      isReady stmt = isReadyFilter scope stmt notFilter
      appearInStmts = foldMap vars ord <> foldMap vars float
      hasAllNonLocalsBound =
        IntSet.null $
        IntSet.filter (\var -> isInScope scope var && not (inScopeBound scope var))
        appearInStmts
  _ -> notFilter

data InScope = InScope
  { unboundPredicates :: Bool
  , isInScope :: Variable -> Bool
  , inScopeBound :: Variable -> Bool
  }

mkInScope :: Bool -> Scope -> InScope
mkInScope allowPred (Scope scope bound) =
  InScope
    { unboundPredicates = allowPred
    , isInScope = \var -> var `IntSet.member` scope
    , inScopeBound = \var -> var `IntMap.member` bound
    }

mkInScopeForClassify :: VarSet -> VarSet -> InScope
mkInScopeForClassify scope bound =
  InScope
    { unboundPredicates = False
    , isInScope = \var -> var `IntSet.member` scope
    , inScopeBound = \var -> var `IntSet.member` bound
    }

allowUnboundPredicates :: Scope -> InScope
allowUnboundPredicates = mkInScope True

ifBoundOnly :: Scope -> InScope
ifBoundOnly = mkInScope False

isBoundInScope :: InScope -> Var -> Bool
isBoundInScope scope (Var ty v _) =
  inScopeBound scope v || (unboundPredicates scope && isPredicate ty)
  where
  isPredicate ty
    | RTS.PredicateRep{} <- RTS.repType ty = True
    | otherwise = False

allBound :: Scope -> IntMap Var
allBound (Scope _ bound) = bound

allVars :: Scope -> VarSet
allVars (Scope scope _) = scope

patIsBound :: InScope -> Pat -> Bool
patIsBound inScope pat
  | PatternMatch _ Point <- classifyPattern (isBoundInScope inScope) pat = True
  | otherwise = False

data PatternMatch = PatternMatch Prefix Point
data Point = Point | Scan
data Prefix
  = PrefixEmpty
  | PrefixFixed
  | PrefixFactId -- ^ prefix contains a fact ID, so it's probably more specific

-- | Classify a pattern according to the cases in 'PatternMatch'
classifyPattern
  :: (Var -> Bool) -- ^ variable is bound?
  -> Term (Match () Var)
  -> PatternMatch
classifyPattern bound t = go PrefixEmpty t end
  where
  go
    :: Prefix -- non-empty fixed prefix seen?
    -> Term (Match () Var)
    -> (Prefix -> PatternMatch)  -- cont
    -> PatternMatch
  go pref t r = case t of
    Byte{} -> fixed pref r
    Nat{} -> fixed pref r
    Array xs -> termSeq pref xs r
    ByteArray{} -> fixed pref r
    Tuple xs -> termSeq pref xs r
    Set xs -> termSeq pref xs r
    Alt _ t -> fixed pref (\pref -> go pref t r)
    String{} -> fixed pref r
    Ref m -> case m of
      MatchWild{} -> wild pref
      MatchNever{} -> PatternMatch pref Point
      MatchFid{} -> fact r
      MatchBind v -> var v
      MatchVar v -> var v
      MatchAnd a b ->
        case (go pref a end, go pref b end) of
          (PatternMatch prefix Point, _) -> r prefix
          (_, PatternMatch prefix Point) -> r prefix
          (match@(PatternMatch PrefixFactId Scan), _) -> match
          (_, match@(PatternMatch PrefixFactId Scan)) -> match
          (match@(PatternMatch PrefixFixed Scan), _) -> match
          (_, match@(PatternMatch PrefixFixed Scan)) -> match
          _ -> PatternMatch pref Scan
      MatchPrefix s t
        | not (ByteString.null s) -> fixed pref (\pref' -> go pref' t r)
        | otherwise -> go pref t r
      -- MatchArrayPrefix doesn't actually look at a prefix because
      -- arrays encode their length at the front
      MatchArrayPrefix{} -> wild pref
      MatchExt{} -> PatternMatch pref Scan
    where
    var v
      | known, PredicateRep{} <- repType (varType v) = fact r
      | known = fixed pref r
      | otherwise = wild pref
      where known = bound v

  -- we've seen a bit of fixed pattern
  fixed PrefixFactId r = r PrefixFactId
  fixed _ r = r PrefixFixed

  fact r = r PrefixFactId

  -- we've seen a bit of wild pattern
  wild prefix = PatternMatch prefix Scan -- stop here

  -- end of the pattern
  end prefix = PatternMatch prefix Point

  termSeq pref [] r = r pref
  termSeq pref (x:xs) r = go pref x (\pref -> termSeq pref xs r)

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
reorderStmt stmt
  | Just flip <- canFlip =
    noflip `catchError` \e ->
      flip `catchError` \e' ->
        attemptBindFromType e noflip `catchError` \_ ->
          attemptBindFromType e' flip `catchError` \_ ->
            giveUp e
  -- If this statement can't be flipped, we may still need to bind
  -- unbound variables:
  | otherwise =
    noflip `catchError` \e ->
      attemptBindFromType e noflip `catchError` \_ ->
         giveUp e
  where
  noflip = toCgStatement stmt
  canFlip
    | FlatStatement ty lhs gen <- stmt, TermGenerator rhs <- gen
    = Just $ toCgStatement (FlatStatement ty rhs (TermGenerator lhs))
    | otherwise
    = Nothing

  attemptBindFromType e rstmt = do
    (extra, stmts) <- maybeBindUnboundPredicate e (([],) <$> rstmt)
    return (extra <> stmts)

  giveUp (s, e) =
    throwError (errMsg s, e)
  errMsg s = Text.pack $ show $ vcat
    [ nest 2 $ vcat ["cannot resolve:", displayDefault stmt]
    , nest 2 $ vcat ["because:", displayDefault s]
    ]

-- In general if we have X = Y where both X and Y are unbound (or LHS = RHS
-- containing unbound variables on both sides) then we have no choice
-- but to return an error message. However in the specific case that we
-- know the type of X or Y is a predicate then we can add the statement
-- X = p _ to bind it and retry.
--
-- Termination is guaranteed as we strictly decrease the number of unbound
-- variables each time
maybeBindUnboundPredicate
  :: (Text, Maybe FixBindOrderError)
  -> R ([CgStatement], a)
  -> R ([CgStatement], a)
maybeBindUnboundPredicate e f
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
    (stmts, a) <- f `catchError` \e' -> maybeBindUnboundPredicate e' f
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
    return (CgStatement (Ref (MatchBind var)) pat : stmts, a)

bindVar :: Var -> R ()
bindVar v = modify $ \s -> s { roScope = bind v $ roScope s }

toCgStatement :: FlatStatement -> R [CgStatement]
toCgStatement stmt = case stmt of
  FlatStatement _ lhs gen -> do
    gen' <- fixVars IsExpr gen -- NB. do this first!
    lhs' <- fixVars IsPat lhs
    return [CgStatement lhs' gen']
  FlatAllStatement v e g -> do
    stmts <- reorderGroup g
    e' <- fixVars IsExpr e
    bindVar v
    return [CgAllStatement v e' stmts]
  FlatNegation stmts -> do
    stmts' <-
      withinNegation $
      withScopeFor [stmts] $
      reorderGroup stmts
    return [CgNegation stmts']
  FlatDisjunction [stmts] ->
    withScopeFor [stmts] $ reorderGroup stmts
  FlatDisjunction groups -> do
    cg <- map runIdentity <$> intersectBindings (map Identity groups)
    return [CgDisjunction cg]
  FlatConditional cond then_ else_ -> do
    r <- intersectBindings [[ cond, then_ ], [ else_ ]]
    case r of
      [[cond', then'], [else']] -> return [CgConditional cond' then' else']
      _ -> error "unexpected length returned by intersectBindings"
  where

  intersectBindings :: (Traversable t, Foldable t) =>
    [t FlatStatementGroup] -> R [t [CgStatement]]
  intersectBindings [] = return []
  intersectBindings groups = do
    initialScope <- gets roScope
    results <- forM groups $ \tgroup -> do
      modify $ \state -> state { roScope = initialScope }
      tstmts <- withScopeFor (toList tgroup) $ traverse reorderGroup tgroup
      newScope <- gets roScope
      return (tstmts, allBound newScope)

    let boundInAllBranches = foldr1 IntMap.intersection (map snd results)
    forM_ results $ \(_, boundInThisBranch) -> do
      let unbound = IntMap.difference boundInThisBranch boundInAllBranches
      forM_ (IntMap.elems unbound) $ \var -> do
        let e = UnboundVariable var
        throwError (errMsg e, Just e)

    let newScope = foldr bind initialScope $ IntMap.elems boundInAllBranches
    modify $ \state -> state { roScope = newScope }
    return (map fst results)

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

errMsg :: FixBindOrderError -> Text
errMsg err = case err of
  UnboundVariable v@(Var ty _ _) ->
    "unbound variable: " <>
    Text.pack (show (displayDefault v <+> ":" <+> displayDefault ty))
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
  -- with the StateT outside, m `catchError` h will restore the state
  -- prior to m when executing h.  This is what we want for trying
  -- alternative binding orders.

instance Monad m => Fresh (StateT ReorderState m) where
  peek = gets roNextVar
  alloc = do
    state@ReorderState{..} <- get
    put state{ roNextVar = roNextVar + 1 }
    return roNextVar

initialReorderState :: Int -> Schema.DbSchema -> ReorderState
initialReorderState nextVar dbSchema = ReorderState
  { roNextVar = nextVar
  , roScope = Scope mempty mempty
  , roDbSchema = dbSchema
  , roNegationEnclosingScope = mempty
  }

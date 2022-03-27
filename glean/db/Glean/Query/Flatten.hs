{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Flatten
  ( flatten
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.List hiding (intersect)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc hiding ((<>), enclose)

import qualified Glean.Angle.Types as Angle
import Glean.Query.Codegen
import Glean.Query.Expand
import Glean.Query.Flatten.Types
import Glean.Query.Typecheck.Types
import Glean.Query.Evolve (Evolutions, evolveTcQuery, evolveType, evolutionsFor)
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS hiding (Match(..))
import Glean.Database.Schema.Types
import qualified Glean.Angle.Types as Schema
import Glean.Schema.Util

-- | Turn 'TypecheckedQuery' into 'FlattenedQuery', by lifting out
-- nested generators into statements.
flatten
  :: DbSchema
  -> Schema.AngleVersion
  -> Bool -- ^ derive DerivedAndStored predicates
  -> TypecheckedQuery
  -> Except Text (FlattenedQuery, Evolutions)
flatten dbSchema _ver deriveStored typechecked = do
  let returnTy = derefType $ evolveType dbSchema $ qiReturnType typechecked
      deriveStoredPred =
        case returnTy of
          Schema.Predicate (PidRef _ pref) | deriveStored -> Just pref
          _ -> Nothing
      state = initialFlattenState
        dbSchema
        (qiNumVars typechecked)
        deriveStoredPred
  (qi,_) <- flip runStateT state $ do
      query <- evolve (qiQuery typechecked)
      q <- flattenQuery query
        `catchError` \e -> throwError $ e <> " in\n" <>
           Text.pack (show (pretty query))
      (q', ty) <- captureKey dbSchema q (case query of TcQuery ty _ _ _ -> ty)
      nextVar <- gets flNextVar
      evolutions <- liftEither $
        evolutionsFor dbSchema (qiReturnType typechecked)
      return (QueryWithInfo q' nextVar ty, evolutions)
  return qi

evolve :: TcQuery -> F TcQuery
evolve query = do
  dbSchema <- gets flDbSchema
  return $ evolveTcQuery dbSchema query

evolveTypecheckedQuery :: TypecheckedQuery -> F TypecheckedQuery
evolveTypecheckedQuery (QueryWithInfo q vars _) = do
  q'@(TcQuery ty _ _ _) <- evolve q
  return $ QueryWithInfo q' vars ty

flattenQuery :: TcQuery -> F FlatQuery
flattenQuery query = do
  (stmts', head', maybeVal) <- flattenQuery' query
  return (FlatQuery head' maybeVal (flattenStmtGroups stmts'))

flattenQuery' :: TcQuery -> F ([Statements], Expr, Maybe Expr)
flattenQuery' (TcQuery ty head Nothing stmts) = do
  stmts' <- mapM flattenStatement stmts
  pats <- flattenPattern head
  case pats of
    [(stmts,head')] -> return (stmts' ++ [stmts], head', Nothing)
    _many -> do
      -- If there are or-patterns on the LHS, then we have
      --    P1 | P2 | ... where stmts
      -- so we will generate
      --    X where stmts; X = P1 | P2 | ...
      v <- fresh ty
      let
        alts =
          [ flattenStmtGroups [stmts] ++
              [ singletonGroup
                 (FlatStatement ty (Ref (MatchBind v)) (TermGenerator head)) ]
          | (stmts, head) <- pats ]
      return
        ( stmts' ++ [mempty `thenStmt` FlatDisjunction alts]
        , Ref (MatchVar v)
        , Nothing
        )
flattenQuery' (TcQuery ty head (Just val) stmts) = do
  stmts' <- mapM flattenStatement stmts
  pats <- flattenPattern head
  vals <- flattenPattern val
  case [ (stmtsp <> stmtsv, head, val)
       | (stmtsp, head) <- pats
       , (stmtsv, val) <- vals ] of
    [(stmts, head', val)] -> return (stmts' ++ [stmts], head', Just val)
    many -> do
      -- As above, but we must handle the value too
      k <- fresh ty
      v <- fresh ty
      let
        alts =
          [ flattenStmtGroups [stmts] ++
              [ singletonGroup (FlatStatement ty lhsPair
                  (TermGenerator (Tuple [head,val]))) ]
          | (stmts, head, val) <- many ]
        lhsPair = Tuple [Ref (MatchBind k), Ref (MatchBind v)]
      return
        ( stmts' ++ [mempty `thenStmt` FlatDisjunction alts]
        , Ref (MatchVar k)
        , Just (Ref (MatchVar v))
        )


flattenStatement :: TcStatement -> F Statements
flattenStatement (TcStatement ty lhs rhs) = do
  rgens <- flattenSeqGenerators rhs
  lgens <- flattenSeqGenerators lhs
  mkStmt ty lgens rgens

mkStmt
  :: Type
  -> [(Statements, Generator)]
  -> [(Statements, Generator)]
  -> F Statements
mkStmt ty [(lhsstmts, TermGenerator lhs)] [(rhsstmts, gen)] = do
  return $ lhsstmts <> rhsstmts `thenStmt` FlatStatement ty lhs gen
mkStmt ty [(lhsstmts, gen)] [(rhsstmts, TermGenerator rhs)] = do
  return $ lhsstmts <> rhsstmts `thenStmt` FlatStatement ty rhs gen
mkStmt ty [(lhsstmts, lgen)] [(rhsstmts, rgen)] = do
  v <- fresh ty
  return $ lhsstmts <> rhsstmts
    `thenStmt` FlatStatement ty (Ref (MatchBind v)) lgen
    `thenStmt` FlatStatement ty (Ref (MatchBind v)) rgen
mkStmt ty [(lhsstmts, gen)] many = do
  v <- fresh ty
  return $
    lhsstmts `thenStmt`
    FlatStatement ty (Ref (MatchBind v)) gen `thenStmt`
    disjunction
      [ flattenStmtGroups [
          rhsstmts `thenStmt`
            FlatStatement ty (Ref (MatchBind v)) rhsgen ]
      | (rhsstmts, rhsgen) <- many
      ]
mkStmt ty many [one] = mkStmt ty [one] many
mkStmt ty lhsmany rhsmany = do
  v <- fresh ty
  rhs <- mkStmt ty [(mempty, TermGenerator (Ref (MatchBind v)))] rhsmany
  lhs <- mkStmt ty [(mempty, TermGenerator (Ref (MatchBind v)))] lhsmany
  return (rhs <> lhs)

flattenSeqGenerators :: TcPat -> F [(Statements, Generator)]
flattenSeqGenerators (Ref (MatchExt (Typed ty match))) = case match of
  TcOr left right -> do
    l <- flattenSeqGenerators left
    r <- flattenSeqGenerators right
    return (l ++ r)
  TcFactGen pid kpat vpat -> do
    kpats <- flattenPattern kpat
    vpats <- flattenPattern vpat
    sequence
      [ do
          (stmts, gen) <- flattenFactGen pid kpat vpat
          return
            ( kstmts <> vstmts <> floatGroups (flattenStmtGroups stmts),
              gen )
      | (kstmts, kpat) <- kpats
      , (vstmts, vpat) <- vpats ]
  TcElementsOfArray pat -> do
    r <- flattenPattern pat
    return [(stmts, ArrayElementGenerator ty pat') | (stmts,pat') <- r ]
  TcQueryGen query -> do
    (stmts, term, _) <- flattenQuery' query
    return [(floatGroups (flattenStmtGroups stmts), TermGenerator term)]
  TcNegation stmts -> do
    stmts' <- flattenStmtGroups <$> mapM flattenStatement stmts
    let neg = FlatNegation stmts'
    return [(mempty `thenStmt` neg, TermGenerator $ Tuple [])]
  TcPrimCall op args -> do
    manyTerms (PrimCall op) <$> mapM flattenPattern args
flattenSeqGenerators pat = do
  r <- flattenPattern pat
  return [(stmts, TermGenerator pat) | (stmts,pat) <- r ]

flattenFactGen :: PidRef -> Pat -> Pat -> F ([Statements], Generator)
flattenFactGen pidRef@(PidRef pid _) kpat vpat = do
  dbSchema <- gets flDbSchema
  deriveStored <- gets flDeriveStored
  case lookupPid pid dbSchema of
    Nothing -> lift $ throwError $
      "internal error: flatten: " <> Text.pack (show pid)
    Just details@PredicateDetails{..} ->
      case predicateDeriving of
        Schema.NoDeriving ->
          return (mempty, FactGenerator pidRef kpat vpat)
        Schema.Derive when query
          | Schema.DerivedAndStored <- when
          , Just predicateRef /= deriveStored ->
               return (mempty, FactGenerator pidRef kpat vpat)
          | otherwise -> do
            calling predicateRef $ do
              qevolved <- evolveTypecheckedQuery query
              query' <- expandDerivedPredicateCall details kpat vpat qevolved
              (stmts, key, maybeVal) <- flattenQuery' query'
              let val = fromMaybe (Tuple []) maybeVal
              return (stmts, DerivedFactGenerator pidRef key val)

-- | Catch recursive derived predicates and throw an error, as a
-- temporary measure until we can handle them.
calling :: Schema.PredicateRef -> F a -> F a
calling ref inner = do
  stack <- gets flStack
  when (ref `elem` stack) $
    throwError $ "recursive reference to predicate " <>
      Text.pack (show (pretty ref))
  modify $ \state -> state { flStack = ref : stack }
  a <- inner
  modify $ \state -> state { flStack = stack }
  return a

flattenPattern :: TcPat -> F [(Statements, Pat)]
flattenPattern pat = case pat of
  Byte n -> singleTerm (Byte n)
  Nat n -> singleTerm (Nat n)
  String s -> singleTerm (String s)
  ByteArray s -> singleTerm (ByteArray s)
  Array terms -> manyTerms Array <$> mapM flattenPattern terms
  Tuple terms -> manyTerms Tuple <$> mapM flattenPattern terms
  Alt s x -> fmap (fmap (Alt s)) <$> flattenPattern x
  Ref (MatchWild ty) -> singleTerm (Ref (MatchWild ty))
  Ref (MatchNever ty) -> singleTerm (Ref (MatchNever ty))
  Ref (MatchFid fid) -> singleTerm (Ref (MatchFid fid))
  Ref (MatchBind var) -> singleTerm (Ref (MatchBind var))
  Ref (MatchVar var) -> singleTerm (Ref (MatchVar var))
  Ref (MatchAnd a b) ->
    twoTerms (\a b -> Ref (MatchAnd a b))
      <$> flattenPattern a
      <*> flattenPattern b
  Ref (MatchPrefix str t) ->
    fmap (fmap (Ref . MatchPrefix str)) <$> flattenPattern t
  Ref (MatchExt (Typed _ (TcOr a b))) -> do  -- Note [flattening TcOr]
    as <- flattenPattern a
    bs <- flattenPattern b
    return (as ++ bs)
  Ref (MatchExt (Typed ty _)) -> do
    gens <- flattenSeqGenerators pat
    v <- fresh ty
    stmts <- mkStmt ty [(mempty, TermGenerator (RTS.Ref (MatchBind v)))] gens
    return [(stmts, RTS.Ref (MatchVar v))]

{- Note [flattening TcOr]

To flatten

   p | q

then we have a choice of two plans.

PLAN A

Is just to generate a fresh variable and bind the term. In an expression
context this would mean

   X where X = p | q

(side note: this might not work in a pattern context, e.g. if p/q
contain wildcards or unbound variables, in which case we could instead
do something like

   X where _ = (() where p = X) | (() where q = X)

but the statement must come *after* matching the current pattern,
which we don't currently support. Also note here that we can't easily
tell whether p/q contain unbound variables, because the binding order
at this point is wrong due to substitution and floating, and won't be
correct until we've run fixMatchBind later.)

PLAN B is to duplicate the enclosing pattern, so if we had

   { "a", p | q }

then we'll get

   { "a", p } | { "a", q }

and so on, all the way up to the nearest enclosing statement, where we
can generate SeqGenerators.

It turns out that PLAN B is what we want if we have an enclosing fact
generator, e.g.

   cxx1.Name ("foo".. | "bar"..)

should generate

   (cxx1.Name "foo"..) | (cxx1.Name "bar"..)

because that would be a lot more efficient than

   cxx1.Name X
   _ = (() where "foo".. = X) | (() where "bar".. = X)

However, PLAN A is good in some other cases because it doesn't
duplicate anything.

For now we do PLAN B all the time, but someday we might try to do PLAN
A when it's a good idea.

Note that this only applies to |. The other kinds of things we want
to lift out can always be bound and floated, because they're always
valid expressions and lifting out won't affect performance.
-}

-- | A set of statements. The statements in the set will be reordered
-- by the Reorder pass later.
data Statements = Statements [FlatStatement]

instance Semigroup Statements where
  Statements s1 <> Statements s2 = Statements (s2 <> s1)

instance Monoid Statements where
  mempty = Statements []
  mconcat = foldl' (<>) mempty  -- override default, we want left-fold

thenStmt :: Statements -> FlatStatement -> Statements
thenStmt (Statements ss) s = Statements (s : ss)

-- | Inject an ordered sequence of FlatStatementGroup into a
-- Statements.  This is used when we need to retain the ordering
-- between some statements, but allow the whole sequence to be
-- reordered with respect to other statements around it.
floatGroups :: [FlatStatementGroup] -> Statements
floatGroups [] = Statements []
floatGroups [x :| []] = Statements [x]
floatGroups g = Statements [FlatDisjunction [g]]
  -- Note: we nest groups by using FlatDisjunction with a single
  -- alternative. This is so that this set of groups may be reordered with
  -- respect to other statements/groups at the same level. For this to
  -- work, we have to retain this grouping until the Reorder phase, so
  -- the optimiser must not flatten it away.

flattenStmts :: Statements -> [FlatStatement]
flattenStmts (Statements s) = reverse s

disjunction :: [[FlatStatementGroup]] -> FlatStatement
disjunction [[x :| []]] = x
disjunction groups = FlatDisjunction groups

flattenStmtGroups :: [Statements] -> [FlatStatementGroup]
flattenStmtGroups stmtss =
  concat [ mkGroup x xs | x:xs <- map flattenStmts stmtss ]
  where
  mkGroup (FlatDisjunction [g]) [] = g  -- flatten unnecessary nesting
  mkGroup x xs = [x :| xs]

singleTerm :: a -> F [(Statements, a)]
singleTerm t = return [(mempty, t)]

manyTerms :: ([a] -> b) -> [[(Statements, a)]] -> [(Statements, b)]
manyTerms constr subterms =
  [ (mconcat stmts, constr ts)
  | xs <- sequence subterms  -- all combinations of subterms
  , let (stmts,ts) = unzip xs
  ]

twoTerms
  :: (a -> b -> c)
  -> [(Statements, a)]
  -> [(Statements, b)]
  -> [(Statements, c)]
twoTerms constr as bs =
  [ (stmtsa <> stmtsb, constr a b)
  | (stmtsa, a) <- as
  , (stmtsb, b) <- bs
  ]


-- | A bit of trickiness to handle capturing and returning the key and
-- value. This avoids having to separately look up the fact later, and
-- also means we can track the size of the output that we're returning
-- in the compiled query.
--
-- For a query like
--
-- > predicate ("x",_)
--
-- the parser expands this to
--
-- > $result where $result = predicate ("x",_)
--
-- and here we expand this to
--
-- > ($result, $key, ()) where $result = predicate $key @ ("x",_)
--
-- and the query compiler compiles this to efficient bytecode to
-- capture the key and return it.
--
-- Note that we might not be able to do this transformation if the
-- query isn't in the right form, so in that case we will add an
-- explicit fact lookup, like
--
--   $result = predicate $key
--
-- as the final statement.
--
-- In the case of a derived predicate, we have a statement like
--
--   $result = pred<- K -> V
--
-- which we transform to
--
--   $result = pred<- $key -> $val where $key = K; $val = V

captureKey
  :: DbSchema
  -> FlatQuery
  -> Type
  -> F (FlatQuery, Type)
captureKey dbSchema (FlatQuery pat Nothing stmts) ty
  | Angle.Predicate pidRef@(PidRef pid _) <- ty  = do
  let
    -- look for $result = pred pat
    -- replace it with  $result = pred ($key @ pat)
    captureStmt
      :: Int
      -> Var
      -> Maybe Var
      -> FlatStatement
      -> F (NonEmpty FlatStatement, Maybe (Pat, Pat))
    captureStmt idVar keyVar maybeValVar
      (FlatStatement ty
        lhs@(Ref (MatchBind (Var _ v' _)))
        (FactGenerator pid kpat vpat))
      | idVar == v' = do
        let kpat' = Ref (MatchAnd (Ref (MatchBind keyVar)) kpat)
            vpat' = case maybeValVar of
              Nothing -> vpat
              Just valVar -> Ref (MatchAnd (Ref (MatchBind valVar)) vpat)
        return
          ( singletonGroup
              (FlatStatement ty lhs (FactGenerator pid kpat' vpat'))
          , Just
            ( Ref (MatchVar keyVar)
            , maybe (Tuple []) (Ref . MatchVar) maybeValVar ))
    captureStmt idVar keyVar@(Var keyTy _ _) maybeValVar
      (FlatStatement ty
        lhs@(Ref (MatchBind (Var _ v' _)))
        (DerivedFactGenerator pid kexpr vexpr))
      | idVar == v' = do
        let
          -- optimise away a redundant copy when the key/value is
          -- already a variable.
          (keyExpr, bindKey)
            | Ref MatchVar{} <- kexpr = (kexpr, Nothing)
            | otherwise =
              ( Ref (MatchVar keyVar)
              , Just $
                  FlatStatement keyTy
                    (Ref (MatchBind keyVar))
                    (TermGenerator kexpr)
              )
          (valExpr, bindVal)
            | Ref MatchVar{} <- vexpr = (vexpr, Nothing)
            | otherwise = case maybeValVar of
                Nothing -> (Tuple [], Nothing)
                Just valVar@(Var valTy _ _) ->
                  ( Ref (MatchVar valVar)
                  , Just $
                      FlatStatement valTy
                        (Ref (MatchBind valVar))
                        (TermGenerator vexpr)
                  )
        return
          ( maybe id NonEmpty.cons bindKey $
            maybe id NonEmpty.cons bindVal $
              singletonGroup $
                FlatStatement ty lhs (DerivedFactGenerator pid keyExpr valExpr)
          , Just (keyExpr, valExpr))
    captureStmt _ _ _ other = return (singletonGroup other, Nothing)

  PredicateDetails{..} <- case lookupPid pid dbSchema of
    Nothing -> throwError "internal: captureKey"
    Just details -> return details
  keyVar <- fresh predicateKeyType
  maybeValVar <- if predicateValueType `eqType` unit
    then return Nothing
    else Just <$> fresh predicateValueType
  (stmts', captured) <-
    case pat of
      Ref (MatchVar (Var _ v _)) -> do
        (stmtss, captured) <- unzip . map NonEmpty.unzip <$>
          mapM (mapM (captureStmt v keyVar maybeValVar)) stmts
        -- stmtss :: [NonEmpty (NonEmpty FlatStatement)]
        let conc ((x :| xs) :| ys) =
              x :| (xs ++ concatMap NonEmpty.toList ys)
        return (map conc stmtss, captured)
      _other -> return (stmts, [])
  let
    returnTy = tupleSchema [ty, predicateKeyType, predicateValueType]

  case catMaybes (concatMap NonEmpty.toList captured) of
    [(key, val)] ->
      return (FlatQuery (RTS.Tuple [pat, key, val]) Nothing stmts', returnTy)
    _ ->
      return (query (stmts' ++ [singletonGroup lookup]), returnTy)
      where
        query = FlatQuery (RTS.Tuple [pat, RTS.Ref (MatchVar keyVar),
          maybe (Tuple []) (RTS.Ref . MatchVar) maybeValVar]) Nothing
        lookup = FlatStatement ty pat
          (FactGenerator pidRef
            (Ref (MatchBind keyVar))
            (Ref (maybe (MatchWild predicateValueType) MatchBind maybeValVar)))

  | otherwise = do
  -- We have
  --
  --    E where stmts
  --
  -- we'll transform it into
  --
  --    ($fid, $key) where
  --    stmts;
  --    $key = E
  --    $fid = Pid<- $key
  --
  -- where
  --    Pid is a new Pid for a fictitious derived predicate
  --    $fid is the fact ID
  --    $key is the key
    let
      pidRef = PidRef (tempPid dbSchema) tempPredicateRef
      pidTy = Angle.Predicate pidRef
      retTy = tupleSchema [pidTy, ty, unit]

    fidVar <- fresh pidTy
    keyVar <- fresh ty

    let
      result = RTS.Tuple
        [ RTS.Ref (MatchVar fidVar)
        , RTS.Ref (MatchVar keyVar)
        , Tuple []
        ]
      resultStmt1 =
        FlatStatement ty (Ref (MatchBind keyVar)) (TermGenerator pat)
      resultGen =
        DerivedFactGenerator pidRef (Ref (MatchVar keyVar)) (Tuple [])
      resultStmt2 =
        FlatStatement pidTy (Ref (MatchBind fidVar)) resultGen

    return
      ( FlatQuery result Nothing
          (stmts ++ [singletonGroup resultStmt1, singletonGroup resultStmt2])
      , retTy )

captureKey _ (FlatQuery _ Just{} _) _ =
  throwError "queries returning both a key and value are not supported"

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

import qualified Glean.Angle.Types as Angle
import Glean.Display
import Glean.Query.Codegen.Types
  ( Match(..)
  , Var(..)
  , Pat
  , Generator_(..)
  , Generator
  , Expr
  , Typed(..)
  , SeekSection(..)
  , QueryWithInfo(..))
import Glean.Query.Expand
import Glean.Query.Flatten.Types
import Glean.Query.Typecheck.Types
import Glean.Query.Vars
import Glean.RTS.Types as RTS
import Glean.RTS.Term as RTS
import Glean.Database.Schema.Types
import Glean.Database.Types (EnableRecursion(..))
import qualified Glean.Angle.Types as Schema
import Glean.Schema.Util

-- | Turn 'TypecheckedQuery' into 'FlattenedQuery', by lifting out
-- nested generators into statements.
flatten
  :: EnableRecursion
  -> DbSchema
  -> Schema.AngleVersion
  -> Bool -- ^ derive DerivedAndStored predicates
  -> TypecheckedQuery
  -> Except Text FlattenedQuery
flatten rec dbSchema ver deriveStored QueryWithInfo{..} =
  fmap fst $ flip runStateT state $ do
    (flattened, maybeLookup, returnType) <- do
      flat <- flattenQuery qiQuery `catchError` flattenFailure
      captureKey ver dbSchema flat (case qiQuery of TcQuery ty _ _ _ _ -> ty)
    nextVar <- gets flNextVar
    return $ QueryWithInfo flattened nextVar maybeLookup returnType
  where
      state = initialFlattenState rec dbSchema qiNumVars deriveStoredPred

      deriveStoredPred =
        case derefType qiReturnType of
          Angle.PredicateTy (PidRef _ pref) | deriveStored -> Just pref
          _ -> Nothing

      flattenFailure e = throwError $
        e <> " in\n" <> Text.pack (show (displayVerbose qiQuery))

flattenQuery :: TcQuery -> F FlatQuery
flattenQuery query = do
  (group, head, maybeVal) <- flattenQuery' query
  return (FlatQuery head maybeVal group)

flattenQuery' :: TcQuery -> F (FlatStatementGroup, Expr, Maybe Expr)
flattenQuery' (TcQuery ty head Nothing stmts ord) = do
  (ords, floats) <- mapAndUnzipM flattenStatement stmts
  pats <- flattenPattern head
  case pats of
    [(stmts,head')] -> return $ case ord of
      Angle.Ordered -> (mkGroup ords (stmts : floats), head', Nothing)
      Angle.Unordered -> (mkGroup [] (stmts : ords <> floats), head', Nothing)
    _many -> do -- TODO: ord
      -- If there are or-patterns on the LHS, then we have
      --    P1 | P2 | ... where stmts
      -- so we will generate
      --    X where stmts; X = P1 | P2 | ...
      v <- fresh ty
      let
        alts =
          [ asGroup (stmts `thenStmt`
              FlatStatement ty (Ref (MatchBind v)) (TermGenerator head))
          | (stmts, head) <- pats ]
      return
        ( case ord of
            Angle.Ordered ->
              mkGroup ords (oneStmt (FlatDisjunction alts) : floats)
            Angle.Unordered ->
              mkStatementGroup
                (Floating (FlatDisjunction alts) :
                  flattenStmts (mconcat (ords <> floats)))
        , Ref (MatchVar v)
        , Nothing
        )
flattenQuery' (TcQuery ty head (Just val) stmts _ord {- TODO -}) = do
  (ords, floats) <- mapAndUnzipM flattenStatement stmts
  pats <- flattenPattern head
  vals <- flattenPattern val
  case [ (stmtsp <> stmtsv, head, val)
       | (stmtsp, head) <- pats
       , (stmtsv, val) <- vals ] of
    [(stmts, head', val)] ->
      return (mkGroup ords (stmts : floats), head', Just val)
    many -> do
      -- As above, but we must handle the value too
      k <- fresh ty
      v <- fresh ty
      let
        alts =
          [ asGroup (stmts `thenStmt`
              FlatStatement ty lhsPair (TermGenerator (Tuple [head,val])))
          | (stmts, head, val) <- many ]
        lhsPair = Tuple [Ref (MatchBind k), Ref (MatchBind v)]
      return
        ( mkGroup ords (oneStmt (FlatDisjunction alts) : floats)
        , Ref (MatchVar k)
        , Just (Ref (MatchVar v))
        )

flattenStatement :: TcStatement -> F (Statements, Statements)
flattenStatement (TcStatement ty lhs rhs) = do
  rgens <- flattenSeqGenerators rhs
  lgens <- flattenSeqGenerators lhs
  mkStmt ty lgens rgens

-- | Here we decide whether an explicit statement is ordered or
-- unordered with respect to other statements in the group. Implicit
-- statements (e.g. those resulting from nested patterns) are always
-- unordered.
addStmt
  :: FlatStatement
  -> (Statements, Statements)
  -> (Statements, Statements)
addStmt stmt@(FlatStatement _ _ TermGenerator{}) (here, float) =
  (here, float `thenStmt` stmt)  -- filters are always unordered
addStmt stmt (here, float) =
  (here `thenStmt` stmt, float)

mkStmt
  :: Type
  -> [(Statements, Statements, Generator)]
  -> [(Statements, Statements, Generator)]
  -> F (Statements, Statements)
mkStmt ty [(lhshere, lhsfloat, TermGenerator lhs)]
    [(rhshere, rhsfloat, gen)] = do
  return $ addStmt (FlatStatement ty lhs gen)
    (lhshere <> rhshere, lhsfloat <> rhsfloat)
mkStmt ty [(lhshere, lhsfloat, gen)]
    [(rhshere, rhsfloat, TermGenerator rhs)] = do
  return $ addStmt (FlatStatement ty rhs gen)
    (lhshere <> rhshere, lhsfloat <> rhsfloat)
mkStmt ty [(lhshere, lhsfloat, lgen)] [(rhshere, rhsfloat, rgen)] = do
  v <- fresh ty
  return $
    addStmt (FlatStatement ty (Ref (MatchBind v)) lgen) $
    addStmt (FlatStatement ty (Ref (MatchBind v)) rgen)
      (lhshere <> rhshere, lhsfloat <> rhsfloat)
mkStmt ty [(lhshere, lhsfloat, gen)] many = do
  v <- fresh ty
  return $
    addStmt (FlatStatement ty (Ref (MatchBind v)) gen)
      (lhshere
       `thenStmt` disjunction
            [ mkGroup
                [rhshere]
                [rhsfloat `thenStmt`
                  FlatStatement ty (Ref (MatchBind v)) rhsgen ]
            | (rhshere, rhsfloat, rhsgen) <- many
            ]
      , lhsfloat
      )
mkStmt ty many [one] = mkStmt ty [one] many
mkStmt ty lhsmany rhsmany = do
  v <- fresh ty
  (lhsord, lhsfloat) <- bind v lhsmany
  (rhsord, rhsfloat) <- bind v rhsmany
  return (lhsord <> rhsord, lhsfloat <> rhsfloat)

flattenSeqGenerators :: TcPat -> F [(Statements, Statements, Generator)]
flattenSeqGenerators (Ref (MatchExt (Typed ty match))) = case match of
  TcOr left right -> do
    l <- flattenSeqGenerators left
    r <- flattenSeqGenerators right
    return (l ++ r)
  TcFactGen pid kpat vpat range -> do
    kpats <- flattenPattern kpat
    vpats <- flattenPattern vpat
    sequence
      [ do
          (stmts, gen) <- flattenFactGen pid range kpat vpat
          return (stmts, kstmts <> vstmts, gen)
      | (kstmts, kpat) <- kpats
      , (vstmts, vpat) <- vpats ]
  TcElementsOfArray pat -> do
    r <- flattenPattern pat
    return [(mempty, stmts, ArrayElementGenerator ty pat') | (stmts,pat') <- r ]
  TcElementsOfSet pat -> do
    r <- flattenPattern pat
    return [(mempty, stmts, SetElementGenerator ty pat') | (stmts,pat') <- r ]
  TcQueryGen query -> do
    (group, term, _) <- flattenQuery' query
    return [(floatGroup group, mempty, TermGenerator term)]
  TcAll query -> do
    (group, term, _) <- flattenQuery' query
    var <- fresh ty
    return
      [ (Statements [FlatAllStatement var term group]
      , mempty
      , TermGenerator (Ref (MatchVar var)))]
  TcNegation stmts -> do
    (ords, floats) <- mapAndUnzipM flattenStatement stmts
    let neg = FlatNegation (mkGroup ords floats)
    return [(oneStmt neg, mempty, TermGenerator $ Tuple [])]
  TcPrimCall op args -> do
    r <- manyTerms (\args -> PrimCall op args ty) <$> mapM flattenPattern args
    return [ (mempty, float, gen) | (float, gen) <- r ]
  TcIf (Typed condTy cond) then_ else_ -> do
    (condOrd, condFloat) <- bindWild condTy =<< flattenSeqGenerators cond
    var <- fresh ty
    (thenOrd, thenFloat) <- bind var =<< flattenSeqGenerators then_
    (elseOrd, elseFloat) <- bind var =<< flattenSeqGenerators else_
    let stmt = FlatConditional
          (asGroup (condOrd <> condFloat))
          (asGroup (thenOrd <> thenFloat))
          (asGroup (elseOrd <> elseFloat))
    return [(mempty `thenStmt` stmt, mempty,
      TermGenerator $ Ref $ MatchVar var)]
  _other -> do
    r <- flattenPattern (Ref (MatchExt (Typed ty match)))
    return $ [(mempty, stmts, TermGenerator pat) | (stmts, pat) <- r]
flattenSeqGenerators pat = do
  r <- flattenPattern pat
  return $ [(mempty, stmts, TermGenerator pat) | (stmts, pat) <- r]

bindWild
  :: Type
  -> [(Statements, Statements, Generator)]
  -> F (Statements, Statements)
bindWild ty stmts =
  mkStmt ty stmts [(mempty, mempty, TermGenerator $ Ref $ MatchWild ty)]

bind
  :: Var
  -> [(Statements, Statements, Generator)]
  -> F (Statements, Statements)
bind var gens = mkStmt
  (varType var)
  [(mempty, mempty, TermGenerator (RTS.Ref (MatchBind var)))]
  gens

flattenFactGen
  :: PidRef
  -> SeekSection
  -> Pat
  -> Pat
  -> F (Statements, Generator)
flattenFactGen pidRef@(PidRef pid _) rng kpat vpat = do
  dbSchema <- gets flDbSchema
  deriveStored <- gets flDeriveStored
  case lookupPid pid dbSchema of
    Nothing -> lift $ throwError $
      "internal error: flatten: " <> Text.pack (show pid)
    Just details@PredicateDetails{..} -> do
      let factGen = (mempty, FactGenerator pidRef kpat vpat rng)
      case predicateDeriving of
        Schema.NoDeriving ->
          return factGen
        Schema.Derive when query
          | Schema.DerivedAndStored <- when
          , Just predicateId /= deriveStored ->
               return factGen
          | otherwise -> do
            calling predicateId factGen $ do
              query' <- expandDerivedPredicateCall details kpat vpat query
              (group, key, maybeVal) <- flattenQuery' query'
              let val = fromMaybe (Tuple []) maybeVal
              return (floatGroup group, DerivedFactGenerator pidRef key val)

calling
  :: Schema.PredicateId
  -> a   -- ^ use this value if we already expanded a recursive call.
  -> F a
  -> F a
calling ref seek inner = do
  stack <- gets flStack
  recursion <- gets flRecursion
  if
    | ref `notElem` stack -> do
      modify $ \state -> state { flStack = ref : stack }
      a <- inner
      modify $ \state -> state { flStack = stack }
      return a
    | EnableRecursion <- recursion -> return seek
    | otherwise ->
      throwError $ "recursive reference to predicate " <>
        Text.pack (show (displayDefault ref))

-- | Returns a list of statement*pattern pairs
--   representing a disjunction @(P1 where S1 | P2 where S2 | ...)@
--   where none of @Pi@ nor @Si@ contain any nested generators.
--
--   Moreover, if @Si@ succeeds, it will bind variables in @Pi@.
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
  Ref (MatchArrayPrefix ty prefix all) -> do
    twoTerms (\pre all -> Ref (MatchArrayPrefix ty pre all))
      <$> (manyTerms id <$> mapM flattenPattern prefix)
      <*> flattenPattern all
  Ref (MatchExt (Typed _ (TcOr a b))) -> do  -- Note [flattening TcOr]
    as <- flattenPattern a
    bs <- flattenPattern b
    return (as ++ bs)

  --  *(pat : P) ==> X where pat = P X
  Ref (MatchExt (Typed keyTy (TcDeref ty pat))) -> do
    r <- flattenPattern pat
    ref@(PidRef _ pred)  <- case ty of
      Angle.PredicateTy ref -> return ref
      _other -> throwError "TcDeref: not a predicate"
    PredicateDetails{..} <- getPredicateDetails pred
    forM r $ \(stmts, p) -> do
      v <- Ref . MatchVar <$> fresh keyTy
      let valPat = Ref (MatchWild predicateValueType)
          gen = FactGenerator ref v valPat SeekOnAllFacts
      return (stmts `thenStmt` FlatStatement ty p gen, v)

  -- pat.field ==> X where { field = X } = pat
  Ref (MatchExt (Typed ty (TcFieldSelect (Typed recTy pat) name))) -> do
    r <- flattenPattern pat
    case derefType recTy of
      Angle.RecordTy fields -> do
        let sel v =
              [ if name == n then v else Ref (MatchWild ty)
              | Angle.FieldDef n ty <- fields
              ]
        forM r $ \(stmts, p) -> do
          v <- Ref . MatchVar <$> fresh ty
          let stmt = FlatStatement recTy (Tuple (sel v)) (TermGenerator p)
          return (stmts `thenStmt` stmt, v)
      _other ->
        throwError $ "internal: TcFieldSelect: " <>
          Text.pack (show (displayDefault recTy))

  -- pat.field? ==> X where { field = X } = pat
  Ref (MatchExt (Typed ty (TcAltSelect (Typed sumTy pat) name))) -> do
    r <- flattenPattern pat
    n <- case derefType sumTy of
      Angle.SumTy fields
        | (_,n):_ <- lookupField name fields ->
          return n
      _ -> error "flatten: SumTy"
    forM r $ \(stmts, p) -> do
      v <- Ref . MatchVar <$> fresh ty
      let stmt = FlatStatement sumTy (Alt n v) (TermGenerator p)
      return (stmts `thenStmt` stmt, v)

  Ref (MatchExt (Typed _ (TcPromote _ pat))) ->
    flattenPattern pat

  Ref (MatchExt (Typed ty _)) -> do
    gens <- flattenSeqGenerators pat
    v <- fresh ty
    let gen = TermGenerator (RTS.Ref (MatchBind v))
    (ord, float) <- mkStmt ty [(mempty, mempty, gen)] gens
    return [(ord <> float, RTS.Ref (MatchVar v))]

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
newtype Statements = Statements { _unStatements :: [FlatStatement] }

instance Semigroup Statements where
  Statements s1 <> Statements s2 = Statements (s2 <> s1)

instance Monoid Statements where
  mempty = Statements []
  mconcat = foldl' (<>) mempty  -- override default, we want left-fold

thenStmt :: Statements -> FlatStatement -> Statements
thenStmt ss s | irrelevant s = ss
thenStmt (Statements ss) s = Statements (s : ss)

oneStmt :: FlatStatement -> Statements
oneStmt s = mempty `thenStmt` s

-- | True for statements that cannot make any difference to the query.
-- That is, statements that can neither fail nor bind any variables.
irrelevant :: FlatStatement -> Bool
irrelevant (FlatStatement _ _ (TermGenerator (Ref (MatchWild _)))) = True
irrelevant (FlatStatement _ (Ref (MatchWild _)) rhs) =
  case rhs of
    DerivedFactGenerator{} -> True
    _ -> False
    -- it's tempting to include PrimCall and TermGenerator here, but
    -- note that these may be written by the user and may be
    -- unresolvable, so elimiating them here may hide an error.
irrelevant _ = False

-- | Inject an ordered sequence of FlatStatementGroup into a
-- Statements.  This is used when we need to retain the ordering
-- between some statements, but allow the whole sequence to be
-- reordered with respect to other statements around it.
floatGroup :: FlatStatementGroup -> Statements
floatGroup g = Statements [grouping g]
  -- Note: we nest groups by using FlatDisjunction with a single
  -- alternative. This is so that this set of groups may be reordered with
  -- respect to other statements/groups at the same level. For this to
  -- work, we have to retain this grouping until the Reorder phase, so
  -- the optimiser must not flatten it away.

flattenStmts :: Statements -> [Ordered FlatStatement]
flattenStmts (Statements s) = map Floating (reverse s)

disjunction :: [FlatStatementGroup] -> FlatStatement
disjunction [FlatStatementGroup [x]] = unOrdered x
disjunction groups = FlatDisjunction groups

mkGroup :: [Statements] -> [Statements] -> FlatStatementGroup
mkGroup ords floats =
  mkStatementGroup $
    flattenStmts (mconcat floats) <>
    map Ordered (orderedGroups ords)
  where
  orderedGroups :: [Statements] -> [FlatStatement]
  orderedGroups = mapMaybe (mkGroup . flattenStmts)

  mkGroup [] = Nothing
  mkGroup [Floating one] = Just one
  mkGroup more  = Just (FlatDisjunction [mkStatementGroup more])

asGroup :: Statements -> FlatStatementGroup
asGroup one = mkStatementGroup (flattenStmts one)

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


-- | Note [query result]
--
-- A bit of trickiness to handle capturing and returning the key and
-- value. This avoids having to separately look up the fact later, and
-- also means we can track the size of the output that we're returning
-- in the compiled query.
--
-- For a query like
--
-- > predicate ("x",_)
--
-- flattening expands this to
--
-- > X where X = predicate ("x",_)
--
-- and here we expand this to
--
-- > { X, K, () } where X = predicate K @ ("x",_)
--
-- and the query compiler compiles this to efficient bytecode to
-- capture the key and return it.
--
-- Note that we might not be able to do this transformation if the
-- query isn't in the right form, so in that case we need an
-- explicit fact lookup, like
--
--   X = predicate K
--
-- BUT we have to do this lookup as the very last thing before
-- returning the result, because the fact might be a derived fact
-- produced by a DerivedFactGen, so it won't exist until after it has
-- been produced. Therefore we can't just add this lookup statement to
-- the query, because there's nothing preventing later phases from
-- moving it too early.
--
-- So instead we defer the generation of this statement until code
-- generation. The generator is kept in the field qiGenerator in the
-- QueryWithInfo.

captureKey
  :: Schema.AngleVersion
  -> DbSchema
  -> FlatQuery
  -> Type
  -> F (FlatQuery, Maybe Generator, Type)
captureKey ver dbSchema
    (FlatQuery pat Nothing (FlatStatementGroup ord)) ty
  | Angle.PredicateTy pidRef@(PidRef pid _) <- ty  = do
  let
    captureOrdStmt fidVar keyVar maybeValVar (Ordered s) =
      (Ordered <$> stmts, pats)
      where (stmts, pats) = captureStmt fidVar keyVar maybeValVar s
    captureOrdStmt fidVar keyVar maybeValVar (Floating s) =
      (Floating <$> stmts, pats)
      where (stmts, pats) = captureStmt fidVar keyVar maybeValVar s

    -- look for $result = pred pat
    -- replace it with  $result = pred ($key @ pat)
    captureStmt
      :: Int
      -> Var
      -> Maybe Var
      -> FlatStatement
      -> (NonEmpty FlatStatement, Maybe (Pat, Pat))
    captureStmt fidVar keyVar maybeValVar
      (FlatStatement ty lhs (FactGenerator pid kpat vpat range))
      | Ref (MatchBind (Var _ v' _)) <- lhs
      , fidVar == v' =
        let kpat' = Ref (MatchAnd (Ref (MatchBind keyVar)) kpat)
            vpat' = case maybeValVar of
              Nothing -> vpat
              Just valVar -> Ref (MatchAnd (Ref (MatchBind valVar)) vpat)
            stmt' = FlatStatement ty lhs (FactGenerator pid kpat' vpat' range)
            keyExpr = Ref (MatchVar keyVar)
            valExpr = maybe (Tuple []) (Ref . MatchVar) maybeValVar
        in
        (stmt' :| [], Just (keyExpr, valExpr))

    captureStmt fidVar keyVar@(Var keyTy _ _) maybeValVar
      (FlatStatement ty lhs (DerivedFactGenerator pid kexpr vexpr))
      | Ref (MatchBind (Var _ v' _)) <- lhs
      , fidVar == v' =
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
        in
        ( NonEmpty.fromList $ catMaybes
            [ bindKey
            , bindVal
            , Just $ FlatStatement ty lhs
                (DerivedFactGenerator pid keyExpr valExpr)
            ]
        , Just (keyExpr, valExpr))
    captureStmt _ _ _ other =
        (other :| [], Nothing)

  PredicateDetails{..} <- case lookupPid pid dbSchema of
    Nothing -> throwError "internal: captureKey"
    Just details -> return details
  keyVar <- fresh predicateKeyType
  maybeValVar <- if eqType ver predicateValueType unit
    then return Nothing
    else Just <$> fresh predicateValueType
  let
    (ord', capturedOrd) =
      case pat of
        Ref (MatchVar (Var _ v _)) ->
          let k :: [Ordered FlatStatement]
                -> [(NonEmpty (Ordered FlatStatement), Maybe (Pat, Pat))]
              k = map (captureOrdStmt v keyVar maybeValVar)
              (ords, capturedOrd) = unzip (k ord)

              conc :: [NonEmpty (Ordered FlatStatement)] ->
                [Ordered FlatStatement]
              conc ((x :| xs) : ys) = x : (xs ++ concatMap NonEmpty.toList ys)
              conc [] = []
          in
          (conc ords, capturedOrd)
        _other -> (ord, [])

    returnTy = tupleSchema [ty, predicateKeyType, predicateValueType]

  case catMaybes capturedOrd of
    [(key, val)] ->
      return (FlatQuery (RTS.Tuple [pat, key, val]) Nothing
        (mkStatementGroup ord'), Nothing, returnTy)
    _ -> do
      -- If we can't find the key/value now, we'll add a statement
      --   X = pred K V
      -- at the end right before codeGen. Here we produce the generator
      -- for the statement.
      let
        gen = FactGenerator pidRef
          (Ref (MatchBind keyVar))
          (Ref (maybe (MatchWild predicateValueType) MatchBind maybeValVar))
          SeekOnAllFacts
      return (FlatQuery pat Nothing (mkStatementGroup ord),
        Just gen,
        returnTy)

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
      pidRef = PidRef (tempPid dbSchema) tempPredicateId
      pidTy = Angle.PredicateTy pidRef
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
          (mkStatementGroup (Floating resultStmt1 : Floating resultStmt2 : ord))
      , Nothing
      , retTy )

captureKey _ _ (FlatQuery _ Just{} _) _ =
  throwError "queries returning both a key and value are not supported"

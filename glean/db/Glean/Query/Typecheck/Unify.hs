{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Typecheck.Unify (
    unify,
    apply,
    zonkTcQuery,
    zonkVars,
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Maybe
import qualified Data.Text as Text
import Compat.Prettyprinter hiding ((<>), enclose)

import Glean.Angle.Types hiding (Type)
import Glean.Database.Schema.Types
import Glean.Display
import Glean.Query.Codegen.Types
import Glean.Query.Typecheck.Monad
import Glean.Query.Typecheck.Types
import Glean.RTS.Term hiding
  (Tuple, ByteArray, String, Array, Nat)
import qualified Glean.RTS.Term as RTS
import Glean.RTS.Types as RTS
import Glean.Schema.Util

unify :: Type -> Type -> T ()
unify ByteTy ByteTy = return ()
unify NatTy NatTy = return ()
unify StringTy StringTy = return ()
unify (ArrayTy t) (ArrayTy u) = unify t u
unify (SetTy t) (SetTy u) = unify t u
unify a@(RecordTy ts) b@(RecordTy us)
  | length ts == length us = forM_ (zip ts us) $
      \(FieldDef f t, FieldDef g u) ->
        if f == g || compareStructurally
          then unify t u
          else unifyError a b
  where
  isTuple = all (Text.isInfixOf "tuplefield"  . fieldDefName)
  compareStructurally = isTuple ts || isTuple us
     -- structural equality for tuples by ignoring field names.
unify a@(SumTy ts) b@(SumTy us)
  | length ts == length us = forM_ (zip ts us) $
      \(FieldDef f t, FieldDef g u) ->
        if f /= g then unifyError a b else unify t u
unify (PredicateTy (PidRef p _)) (PredicateTy (PidRef q _))
  | p == q = return ()
unify (NamedTy (ExpandedType n _)) (NamedTy (ExpandedType m _))
  | n == m = return ()
unify (NamedTy (ExpandedType _ t)) u = unify t u
unify t (NamedTy (ExpandedType _ u)) = unify t u
unify (MaybeTy t) (MaybeTy u) = unify t u
unify (MaybeTy t) u@SumTy{} = unify (lowerMaybe t) u
unify (MaybeTy t) u@HasTy{} = unify (lowerMaybe t) u
unify t@SumTy{} (MaybeTy u) = unify t (lowerMaybe u)
unify t@HasTy{} (MaybeTy u) = unify t (lowerMaybe u)
unify (EnumeratedTy ns) (EnumeratedTy ms) | ns == ms = return ()
unify (EnumeratedTy ns) u@SumTy{} = unify (lowerEnum ns) u
unify t@SumTy{} (EnumeratedTy ns) = unify t (lowerEnum ns)
unify BooleanTy BooleanTy = return ()

unify (TyVar x) (TyVar y) | x == y = return ()
unify (TyVar x) t = extend x t
unify t (TyVar x) = extend x t

{- Unifying HasTy

   When a (HasTy f r x) is created, the type variable x is unbound.
   Unification with this HasTy records the new information by binding
   x; the new information might be another HasTy with more fields (and
   an unbound type variable), or it might be a RecordTy / SumTy.

   Therefore during typechecking a HasTy turns into a chain of
   increasingly larger HasTys culminating in a RecordTy / SumTy.
-}
unify a@(HasTy fa ra x) b@(HasTy fb rb y)
  | x == y = return ()
  | otherwise = do
  rec <- case (ra,rb) of
    (Just x, Just y) | x /= y -> unifyError a b
    (Nothing, _) -> return rb
    _otherwise -> return ra
  union <- Map.mergeA
    Map.preserveMissing
    Map.preserveMissing
    (Map.zipWithAMatched $ \_ a b -> do unify a b; return a)
    fa fb
  -- if either a or b is the same as the unified type, avoid creating
  -- a new type variable.
  let size = Map.size union
  if size == Map.size fa && ra == rec
    then extend y a
    else if size == Map.size fb && rb == rec
      then extend x b
      else do
        z <- freshTyVarInt
        let all = HasTy union rec z
        extend x all
        extend y all

unify a@(HasTy _ (Just Sum) _) b@RecordTy{} =
  unifyError a b
unify a@(HasTy m _ x) b@(RecordTy fs) = do
  forM_ fs $ \(FieldDef f ty) ->
    case Map.lookup f m of
      Nothing -> return ()
      Just ty' -> unify ty ty'
  when (not (Map.null (foldr (Map.delete . fieldDefName) m fs))) $
    unifyError a b
  extend x (RecordTy fs)

unify a@(HasTy _ (Just Record) _) b@SumTy{} =
  unifyError a b
unify a@(HasTy m _ x) b@(SumTy fs) = do
  forM_ fs $ \(FieldDef f ty) ->
    case Map.lookup f m of
      Nothing -> return ()
      Just ty' -> unify ty ty'
  forM_ (Map.keys m) $ \n ->
    when (n `notElem` map fieldDefName fs) $
      unifyError a b
  extend x (SumTy fs)

unify a@RecordTy{} b@HasTy{} = unify b a
unify a@SumTy{} b@HasTy{} = unify b a

unify (HasKey keyTy x) predTy@(PredicateTy (PidRef _ ref)) = do
  PredicateDetails{..} <- getPredicateDetails ref
  unify predicateKeyType keyTy
  extend x predTy
unify (HasKey a x) (HasKey b y)
  | x == y = return ()
  | otherwise = do
  unify a b
  extend x (HasKey a y)
  extend y (HasKey a x)
unify a@PredicateTy{} b@HasKey{} = unify b a

unify a b = unifyError a b

unifyError :: Type -> Type -> T a
unifyError a b = do
  opts <- gets tcDisplayOpts
  prettyError $ vcat
    [ "type error:"
    , indent 2 (display opts a)
    , "does not match:"
    , indent 2 (display opts b)
    ]

extend :: Int -> Type -> T ()
extend x t = do
  t' <- apply t  -- avoid creating a cycle in the substitution
  if
    | TyVar y <- t, y == x -> return ()
    | otherwise -> do
      subst <- gets tcSubst
      case IntMap.lookup x subst of
        Just u -> unify t' u
        Nothing ->
          modify $ \s -> s{ tcSubst = IntMap.insert x t' (tcSubst s) }

apply :: Type -> T Type
apply t = do
  let unbound _ = return Nothing
      unboundHas _ _ _ = return Nothing
  apply_ unbound unboundHas t

zonkType :: Type -> T Type
zonkType t = do
  opts <- gets tcDisplayOpts
  let unbound x = prettyError $
        "ambiguous type: " <> display opts (TyVar x :: Type)
  apply_ unbound resolveHas t

-- resolve unbound HasTy to a record. Otherwise a query like
--   { a = 3 }
-- will be ambiguous.
resolveHas :: Int -> Map.Map Name Type -> Maybe RecordOrSum -> T (Maybe Type)
resolveHas _ fieldmap mr = case mr of
  Nothing -> rec
  Just Record -> rec
  Just Sum -> sum
  where
  rec = return $ Just $ RecordTy fields
  sum = return $ Just $ SumTy fields
  fields = [ FieldDef name ty | (name,ty) <- Map.toList fieldmap ]

apply_
  :: (Int -> T (Maybe Type))
     -- ^ unbound regular tyvar
  -> (Int -> Map.Map Name Type -> Maybe RecordOrSum -> T (Maybe Type))
     -- ^ unbound HasTy tyvar
  -> Type
  -> T Type
apply_ unbound unboundHas t = do
  subst <- gets tcSubst
  go_ subst t
  where
  go_ subst t = go t
    where
    lookup x = case IntMap.lookup x subst of
      Nothing -> unbound x
      Just ty -> return (Just ty)

    lookupHas x f r = case IntMap.lookup x subst of
      Nothing -> unboundHas x f r
      Just ty -> return (Just ty)

    go t = case t of
      ByteTy -> return t
      NatTy -> return t
      StringTy -> return t
      ArrayTy t -> ArrayTy <$> go t
      RecordTy fs ->
        fmap RecordTy $ forM fs $ \(FieldDef n t) ->
          FieldDef n <$> go t
      SumTy fs ->
        fmap SumTy $ forM fs $ \(FieldDef n t) ->
          FieldDef n <$> go t
      PredicateTy{} -> return t
      NamedTy{} -> return t
      MaybeTy t -> MaybeTy <$> go t
      EnumeratedTy{} -> return t
      BooleanTy -> return t
      TyVar x -> do
        m <- lookup x
        case m of
          Nothing -> return t
          Just u -> go u
      HasTy f r x -> do
        m <- lookupHas x f r
        case m of
          Nothing -> return t
          Just u -> go u
      HasKey _ x -> do
        m <- lookup x
        case m of
          Nothing -> return t
          Just u -> go u
      SetTy t -> SetTy <$> go t

zonkVars :: T ()
zonkVars = do
  vars <- gets tcVars
  zonked <- forM vars $ \Var{..} -> do
    let
      unbound _ = prettyError $ vcat
          [ "variable " <> pretty var <>
            " has unknown type"
          , "    try adding a type signature, like: " <> pretty var <> " : T"
          ]
          where var = fromMaybe (Text.pack ('_':show varId)) varOrigName
    t <- apply_ unbound resolveHas varType
    return (Var { varType = t, ..})
  modify $ \s -> s { tcVars = zonked }

zonkTcQuery :: TcQuery -> T TcQuery
zonkTcQuery (TcQuery ty k mv stmts ord) =
  TcQuery
    <$> zonkType ty
    <*> zonkTcPat k
    <*> mapM zonkTcPat mv
    <*> mapM zonkTcStatement stmts
    <*> pure ord

zonkTcPat :: TcPat -> T TcPat
zonkTcPat p = case p of
  RTS.Byte{} -> return p
  RTS.Nat{} -> return p
  RTS.Array ts -> RTS.Array <$> mapM zonkTcPat ts
  RTS.ByteArray{} -> return p
  RTS.Tuple ts -> RTS.Tuple <$> mapM zonkTcPat ts
  RTS.Alt n t -> RTS.Alt n <$> zonkTcPat t
  RTS.String{} -> return p
  Ref (MatchExt (Typed ty (TcPromote inner e))) -> do
    ty' <- zonkType ty
    inner' <- zonkType inner
    e' <- zonkTcPat e
    case (ty', inner') of
      (TyVar{}, _) -> error "zonkMatch: tyvar"
      (_, TyVar{}) -> error "zonkMatch: tyvar"
      (PredicateTy (PidRef _ ref), PredicateTy (PidRef _ ref'))
        | ref == ref' -> return e'
      (PredicateTy pidRef@(PidRef _ ref), _other) -> do
        PredicateDetails{..} <- getPredicateDetails ref
        let vpat = Ref (MatchWild predicateValueType)
        return (Ref (MatchExt (Typed ty'
          (TcFactGen pidRef e' vpat SeekOnAllFacts))))
      _ ->
        return e'
  Ref (MatchExt (Typed ty (TcDemote inner e))) -> do
    ty' <- zonkType ty
    inner' <- zonkType inner
    e' <- zonkTcPat e
    case (ty', inner') of
      (TyVar{}, _) -> error "zonkMatch: tyvar"
      (_, TyVar{}) -> error "zonkMatch: tyvar"
      (PredicateTy (PidRef _ ref), PredicateTy (PidRef _ ref'))
        | ref == ref' -> return e'
      (_other, PredicateTy{}) -> do
        return (Ref (MatchExt (Typed ty' (TcDeref inner' e'))))
      _ ->
        return e'
  Ref (MatchExt (Typed ty (TcStructPat fs))) -> do
    ty' <- zonkType ty
    case ty' of
      RecordTy fields ->
        fmap RTS.Tuple $ forM fields $ \(FieldDef f ty) ->
          case [ p | (g,p) <- fs, f == g ] of
            [] -> return (mkWild ty)
            (x:_) -> zonkTcPat x
      SumTy fields ->
        case fs of
          [(name,pat)]
            | (_, n) :_ <- lookupField name fields -> do
              pat' <- zonkTcPat pat
              return (RTS.Alt n pat')
          _other -> error $ "zonkTcPat: " <> show (displayDefault p)
      _other -> do
        opts <- gets tcDisplayOpts
        prettyError $
          nest 4 $ vcat
            [ "type error in pattern"
            , "pattern: " <> display opts p
            , "expected type: " <> display opts ty
            ]

  Ref m -> Ref <$> zonkMatch m

zonkMatch :: Match (Typed TcTerm) Var -> T (Match (Typed TcTerm) Var)
zonkMatch m = case m of
  MatchWild ty -> MatchWild <$> zonkType ty
  MatchNever ty -> MatchNever <$> zonkType ty
  MatchFid{} -> return m
  MatchBind v -> MatchBind <$> var v
  MatchVar v -> MatchVar <$> var v
  MatchAnd a b -> MatchAnd <$> zonkTcPat a <*> zonkTcPat b
  MatchPrefix s pat -> MatchPrefix s <$> zonkTcPat pat
  MatchArrayPrefix ty ts all ->
    MatchArrayPrefix
      <$> zonkType ty
      <*> mapM zonkTcPat ts
      <*> zonkTcPat all
  MatchExt (Typed ty e) ->
    MatchExt <$> (Typed <$> zonkType ty <*> zonkTcTerm e)
  where
  var (Var _ n _) = do
    vars <- gets tcVars
    case IntMap.lookup n vars of
      Nothing -> error "zonkMatch"
      Just v -> return v

zonkTcTerm :: TcTerm -> T TcTerm
zonkTcTerm t = case t of
  TcOr a b -> TcOr <$> zonkTcPat a <*> zonkTcPat b
  TcFactGen pid k v sec ->
    TcFactGen pid <$> zonkTcPat k <*> zonkTcPat v <*> pure sec
  TcElementsOfArray a -> TcElementsOfArray <$> zonkTcPat a
  TcQueryGen q -> TcQueryGen <$> zonkTcQuery q
  TcAll q -> TcAll <$> zonkTcQuery q
  TcNegation stmts -> TcNegation <$> mapM zonkTcStatement stmts
  TcPrimCall op args -> TcPrimCall op <$> mapM zonkTcPat args
  TcIf (Typed ty cond) th el ->
    TcIf
      <$> (Typed <$> zonkType ty <*> zonkTcPat cond)
      <*> zonkTcPat th
      <*> zonkTcPat el
  TcDeref ty p ->
    TcDeref <$> zonkType ty <*> zonkTcPat p
  TcFieldSelect (Typed ty p) f ->
    TcFieldSelect
      <$> (Typed <$> zonkType ty <*> zonkTcPat p)
      <*> pure f
  TcAltSelect (Typed ty p) f ->
    TcAltSelect
      <$> (Typed <$> zonkType ty <*> zonkTcPat p)
      <*> pure f
  TcElements p -> TcElements <$> zonkTcPat p
  TcPromote{} -> error "zonkTcTerm: TcPromote" -- handled in zonkTcPat
  TcDemote{} -> error "zonkTcTerm: TcPromote" -- handled in zonkTcPat
  TcStructPat{} -> error "zonkTcTerm: TcStructPat" -- handled in zonkTcPat

zonkTcStatement :: TcStatement -> T TcStatement
zonkTcStatement (TcStatement ty l r) =
  TcStatement
    <$> zonkType ty
    <*> zonkTcPat l
    <*> zonkTcPat r

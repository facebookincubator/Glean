{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Collecting the variables mentioned by a term
module Glean.Query.Vars (
    VarId,
    VarSet,
    WhichVars(..),
    VarsOf(..),
    vars,
    varsBound,
    varsUsed,
    Fresh(..),
    fresh,
    freshWild,
    freshWildVars,
    reWild,
    reWildQuery,
  ) where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty)

import Glean.Display
import Glean.Query.Codegen.Types
import Glean.RTS.Term hiding (All)
import qualified Glean.RTS.Term as RTS
import Glean.RTS.Types as RTS

type VarId = Int
type VarSet = IntSet
type VarMap = IntMap VarId

vars :: VarsOf a => a -> VarSet
vars x = varsOf AllVars x IntSet.empty

varsBound :: VarsOf a => a -> VarSet
varsBound x = varsOf VarsBound x IntSet.empty

varsUsed :: VarsOf a => a -> VarSet
varsUsed x = varsOf VarsUsed x IntSet.empty

data WhichVars = VarsUsed | VarsBound | AllVars
  deriving Eq

class VarsOf a where
  varsOf :: WhichVars -> a -> VarSet -> VarSet

instance VarsOf Generator where
  varsOf w (FactGenerator _ key val _) r = varsOf w key $! varsOf w val r
  varsOf w (TermGenerator exp) r = varsOf w exp r
  varsOf w (DerivedFactGenerator _ key val) r = varsOf w key $! varsOf w val r
  varsOf w (ArrayElementGenerator _ arr) r = varsOf w arr r
  varsOf w (SetElementGenerator _ set) r = varsOf w set r
  varsOf w (PrimCall _ args _) r = varsOf w args r

instance (VarsOf a) => VarsOf [a] where
  varsOf w container r = foldr (varsOf w) r container

instance (VarsOf a) => VarsOf (NonEmpty a) where
  varsOf w container r = foldr (varsOf w) r container

instance VarsOf m => VarsOf (Term m) where
  varsOf w t r = case t of
    Byte{} -> r
    Nat{} -> r
    ByteArray{} -> r
    String{} -> r
    Ref x -> varsOf w x r
    Tuple xs -> varsOf w xs r
    Array xs -> varsOf w xs r
    Alt _ x -> varsOf w x r
    RTS.All xs -> varsOf w xs r

instance VarsOf (Match () Var) where
  varsOf w m r = case m of
    MatchWild{} -> r
    MatchNever{} -> r
    MatchFid{} -> r
    MatchBind (Var _ v _) -> if w == VarsUsed then r else IntSet.insert v r
    MatchVar (Var _ v _) -> if w == VarsBound then r else IntSet.insert v r
    MatchAnd a b -> varsOf w a $! varsOf w b r
    MatchPrefix _ t -> varsOf w t r
    MatchArrayPrefix _ty pre -> varsOf w pre r
    MatchExt{} -> r

instance VarsOf CgQuery where
  varsOf w (CgQuery head stmts) r = varsOf w head $! varsOf w stmts r

instance VarsOf CgStatement where
  varsOf w (CgStatement lhs gen) r = varsOf w lhs $! varsOf w gen r
  varsOf w (CgNegation stmts) r = varsOf w stmts r
  varsOf w (CgDisjunction stmtss) r = varsOf w stmtss r
  varsOf w (CgConditional cond then_ else_) r =
    varsOf w cond $! varsOf w then_ $! varsOf w else_ r

-- -----------------------------------------------------------------------------
-- Fresh variables

class Fresh m where
  peek :: m VarId
  alloc :: m VarId

fresh :: (Monad m, Fresh m) => Type -> m Var
fresh ty = do
  n <- alloc
  return (Var ty n Nothing)

-- -----------------------------------------------------------------------------
-- Replace wildcards with fresh variables

freshWildVars :: (Monad m, Fresh m) => (a -> m a) -> a -> m (a, VarSet)
freshWildVars freshen thing = do
  v0 <- peek
  pat' <- freshen thing
  v1 <- peek
  return (pat', IntSet.fromList [v0..v1])

-- | Instantiate all the wildcards in a pattern with fresh
-- variables. This makes the pattern usable when we substitute it, for
-- two reasons: (1) it might occur in multiple places, and we must
-- ensure that it matches the same term in all places, and (2) it
-- might occur in an expression (E where ...) where wildcards don't
-- make sense.
freshWild :: forall m . (Monad m, Fresh m) => Pat -> m Pat
freshWild pat = mapM freshWildMatch pat
  where
  freshWildMatch :: Match a Var -> m (Match a Var)
  freshWildMatch m = case m of
    MatchWild ty -> MatchBind <$> fresh ty
    MatchPrefix str rest -> MatchPrefix str <$> mapM freshWildMatch rest
    MatchArrayPrefix ty pre ->
      MatchArrayPrefix ty <$> (mapM.mapM) freshWildMatch pre
    MatchNever ty -> return (MatchNever ty)
    MatchFid f -> return (MatchFid f)
    MatchBind v -> return (MatchVar v)  -- also make all variables MatchVar
    MatchVar v -> return (MatchVar v)
    MatchAnd x y -> MatchAnd <$> mapM freshWildMatch x <*> mapM freshWildMatch y
    MatchExt _ -> error "freshWildMatch"

-- | Replace unused variables with wildcards and remap variable numbers
reWild :: VarMap -> Pat -> Pat
reWild used pat = fmap reWildMatch pat
  where
  reWildMatch :: Match () Var -> Match () Var
  reWildMatch m = case m of
    MatchWild ty -> MatchWild ty
    MatchBind (Var ty n x) -> case IntMap.lookup n used of
      Nothing -> MatchWild ty
      Just new -> MatchBind (Var ty new x)
    MatchVar (Var ty n x) -> case IntMap.lookup n used of
      Nothing -> error $ "reWild: " <> show (displayVerbose m)
      Just new -> MatchVar (Var ty new x)
    MatchPrefix str rest -> MatchPrefix str (fmap reWildMatch rest)
    MatchArrayPrefix ty pre ->
      MatchArrayPrefix ty $ (fmap . fmap) reWildMatch pre
    MatchAnd x y -> MatchAnd (fmap reWildMatch x) (fmap reWildMatch y)
    MatchNever ty -> MatchNever ty
    MatchFid f -> MatchFid f
    MatchExt _ -> error "reWild"

reWildGenerator :: VarMap -> Generator -> Generator
reWildGenerator used gen = case gen of
  FactGenerator pid key val sec ->
    FactGenerator pid (reWild used key) (reWild used val) sec
  TermGenerator t -> TermGenerator (reWild used t)
  DerivedFactGenerator pid key val ->
    DerivedFactGenerator pid (reWild used key) (reWild used val)
  ArrayElementGenerator ty exp ->
    ArrayElementGenerator ty (reWild used exp)
  SetElementGenerator ty exp ->
    SetElementGenerator ty (reWild used exp)
  PrimCall op args ty ->
    PrimCall op (map (reWild used) args) ty

reWildStatement :: VarMap -> CgStatement -> CgStatement
reWildStatement used (CgStatement lhs rhs) =
  CgStatement (reWild used lhs) (reWildGenerator used rhs)
reWildStatement used (CgNegation stmts) =
  CgNegation (map (reWildStatement used) stmts)
reWildStatement used (CgDisjunction stmtss) =
  CgDisjunction (map (map (reWildStatement used)) stmtss)
reWildStatement used (CgConditional cond then_ else_) =
  CgConditional
    (map (reWildStatement used) cond)
    (map (reWildStatement used) then_)
    (map (reWildStatement used) else_)

reWildQuery :: VarMap -> CgQuery -> CgQuery
reWildQuery used (CgQuery head stmts) =
  CgQuery (reWild used head) (map (reWildStatement used) stmts)

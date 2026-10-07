{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Glean.Query.BindOrder
  ( Fix
  , Scope(..)
  , NoBind(..)
  , IsPat(..)
  , runFixBindOrder
  , FixBindOrder(..)
  , FixBindOrderError(..)
  , Variable
  , isBound
  , bind
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Glean.Query.Codegen.Types (Match(..), Var(..), Generator_(..), Generator)
import Glean.Query.Vars (VarSet)
import Glean.RTS.Term as RTS

-- -----------------------------------------------------------------------------
-- Resolving MatchBind vs. MatchVar

-- Before the Reorder pass, MatchBind and MatchVar are the same (strictly
-- speaking we should have just one variable form). The reorder pass resolves
-- statement ordering so that variables are bound before their use;
-- MatchBind indicates a variable binding and MatchVar indicates a variable use.

data Scope = Scope
  { isScope :: VarSet
  , bound :: IntMap Var
  }
  deriving (Show)

type Variable = Int

isBound :: Scope -> Variable -> Bool
isBound (Scope _ bound) var = IntMap.member var bound

bind :: Var -> Scope -> Scope
bind var (Scope scope bound) = Scope scope (IntMap.insert (varId var) var bound)


-- | Variables that should not be bound.
-- Statements in a negated subquery should not bind values that occur later
-- in a positive statement.
newtype NoBind = NoBind
  { unNoBind :: IntSet
  }

data FixBindOrderError
  = CannotUseWildcardInExpr
  | CannotUseNeverInExpr
  | UnboundVariable Var

type Fix a = StateT (Scope, NoBind) (Except FixBindOrderError) a

runFixBindOrder
  :: Scope -> NoBind -> Fix a -> Except FixBindOrderError (a, Scope)
runFixBindOrder scope nobind fx = do
  (res, (scope', _)) <- runStateT fx (scope, nobind)
  return (res, scope')

data IsPat = IsPat | IsExpr
  deriving Eq

class FixBindOrder a where
  fixBindOrder :: IsPat -> a -> Fix a

instance FixBindOrder Generator where
  fixBindOrder isPat (TermGenerator p) =
    TermGenerator <$> fixBindOrder isPat p
  fixBindOrder _ (ArrayElementGenerator ty p) =
    ArrayElementGenerator ty <$> fixBindOrder IsExpr p
  fixBindOrder _ (SetElementGenerator ty p) =
    SetElementGenerator ty <$> fixBindOrder IsExpr p
  fixBindOrder isPat (DerivedFactGenerator pid key val) =
    DerivedFactGenerator pid
      <$> fixBindOrder isPat key
      <*> fixBindOrder isPat val
  fixBindOrder _ (FactGenerator pid kpat vpat range) =
    FactGenerator pid <$>
      fixBindOrder IsPat kpat <*>
      fixBindOrder IsPat vpat <*>
      pure range
  fixBindOrder _ (PrimCall op pats ty) =
    PrimCall op <$> mapM (fixBindOrder IsExpr) pats <*> pure ty

instance FixBindOrder a => FixBindOrder (Term a) where
  fixBindOrder isPat term = mapM (fixBindOrder isPat) term

instance FixBindOrder () where
  fixBindOrder _ () = return ()

instance (FixBindOrder a) => FixBindOrder (Match a Var) where
  fixBindOrder IsExpr MatchWild{} =
    throwError CannotUseWildcardInExpr
  fixBindOrder IsExpr MatchNever{} =
    throwError CannotUseNeverInExpr
  fixBindOrder isPat (MatchBind var) = fixVar isPat var
  fixBindOrder isPat (MatchVar var) = fixVar isPat var
  fixBindOrder isPat (MatchAnd a b) =
    MatchAnd <$> mapM (fixBindOrder isPat) a <*> mapM (fixBindOrder isPat) b
  fixBindOrder isPat (MatchPrefix str rest) =
    MatchPrefix str <$> mapM (fixBindOrder isPat) rest
  fixBindOrder isPat (MatchArrayPrefix ty pre all) =
    MatchArrayPrefix ty
      <$> (mapM.mapM) (fixBindOrder isPat) pre
      <*> mapM (fixBindOrder isPat) all
  fixBindOrder isPat (MatchExt ext) = MatchExt <$> fixBindOrder isPat ext
  fixBindOrder _ other@MatchFid{} = return other
  fixBindOrder _ other@MatchNever{} = return other
  fixBindOrder _ other@MatchWild{} = return other

fixVar :: IsPat -> Var -> Fix (Match a Var)
fixVar isPat var@(Var _ v _) = do
  (scope, NoBind noBind) <- get
  if
    | isBound scope v -> return (MatchVar var)
    | (IsExpr == isPat) || (v `IntSet.member` noBind)
      -> throwError $ UnboundVariable var
    | otherwise -> do
      put (bind var scope, NoBind noBind)
      return (MatchBind var)

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Collecting the variables mentioned by a term
module Glean.Query.Vars (
    VarSet,
    varsOf,
    vars,
    boundVars,
    boundVarsOf,
  ) where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)

import Glean.Query.Codegen
import Glean.Query.Flatten.Types
import Glean.RTS.Term hiding (Match(..))

type VarSet = IntSet

vars :: VarsOf a => a -> VarSet
vars x = varsOf x IntSet.empty

class VarsOf a where
  varsOf :: a -> VarSet -> VarSet

instance VarsOf FlatStatement where
  varsOf s r = case s of
    FlatStatement _ lhs rhs -> varsOf lhs (varsOf rhs r)
    FlatNegation stmts      -> varsStmts stmts r
    FlatDisjunction stmtss  -> foldr varsStmts r stmtss
    FlatConditional cond then_ else_ ->
      foldr varsStmts r [cond, then_, else_]
    where
      varsStmts stmts r = foldr (\g r -> foldr varsOf r g) r stmts

instance VarsOf Generator where
  varsOf (FactGenerator _ key val) r = varsOf key $! varsOf val r
  varsOf (TermGenerator exp) r = varsOf exp r
  varsOf (DerivedFactGenerator _ key val) r = varsOf key $! varsOf val r
  varsOf (ArrayElementGenerator _ arr) r = varsOf arr r
  varsOf (PrimCall _ args) r = foldr varsOf r args

instance (VarsOf a) => VarsOf [a] where
  varsOf container r = foldr varsOf r container

instance (VarsOf a) => VarsOf (NonEmpty a) where
  varsOf container r = foldr varsOf r container

instance VarsOf m => VarsOf (Term m) where
  varsOf t r = case t of
    Byte{} -> r
    Nat{} -> r
    ByteArray{} -> r
    String{} -> r
    Ref x -> varsOf x r
    Tuple xs -> foldr varsOf r xs
    Array xs -> foldr varsOf r xs
    Alt _ x -> varsOf x r

instance VarsOf (Match () Var) where
  varsOf m r = case m of
    MatchWild{} -> r
    MatchNever{} -> r
    MatchFid{} -> r
    MatchBind (Var _ v _) -> IntSet.insert v r
    MatchVar (Var _ v _) -> IntSet.insert v r
    MatchAnd a b -> varsOf a $! varsOf b r
    MatchPrefix _ t -> varsOf t r
    MatchExt{} -> r

-- | Like 'varsOf', but only including variables that can be bound by
-- this statement.
boundVars :: FlatStatement -> VarSet
boundVars stmt = boundVarsOf stmt IntSet.empty

boundVarsOf :: FlatStatement -> VarSet -> VarSet
boundVarsOf (FlatStatement _ lhs rhs) r = varsOf lhs (boundVarsOfGen rhs r)
boundVarsOf (FlatNegation _) r = r -- a negated query cannot bind variables
boundVarsOf (FlatDisjunction stmtss) r = foldr varsStmts r stmtss
  where varsStmts stmts r = foldr (\g r -> foldr boundVarsOf r g) r stmts
boundVarsOf (FlatConditional cond then_ else_) r =
  varsStmts cond $ varsStmts then_ $ varsStmts else_ r
  where varsStmts stmts r = foldr (\g r -> foldr boundVarsOf r g) r stmts

boundVarsOfGen :: Generator -> VarSet -> VarSet
boundVarsOfGen DerivedFactGenerator{} r = r
boundVarsOfGen ArrayElementGenerator{} r = r
boundVarsOfGen PrimCall{} r = r
boundVarsOfGen other r = varsOf other r

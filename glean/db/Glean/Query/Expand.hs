{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Expand
  ( expandDerivedPredicateCall
  ) where

import Control.Monad.State
import Data.Bifunctor (first)

import Glean.Query.Codegen.Types
  (Match(..), Var(..), Pat, QueryWithInfo(..), Typed(..))
import Glean.Query.Flatten.Types
import Glean.Query.Typecheck.Types
import Glean.Query.Vars
import Glean.RTS.Term as RTS
import Glean.Database.Schema.Types


expandDerivedPredicateCall
  :: PredicateDetails
  -> Pat                -- ^ pattern at the call site
  -> Pat                -- ^ value pattern, if applicable
  -> TypecheckedQuery   -- ^ query from the derived predicate
  -> F TcQuery
expandDerivedPredicateCall PredicateDetails{..} key val QueryWithInfo{..} = do
  (TcQuery _ keyDef maybeValDef stmts) <-
    instantiateWithFreshVariables qiQuery qiNumVars

  let
    key' = fmap (first (\_ -> error "MatchExt")) key
    val' = fmap (first (\_ -> error "MatchExt")) val

  -- we have
  --    pred P -> Q
  -- where
  --    pred : T
  --       K -> V where S
  --
  -- let's turn it into
  --
  --   { X, Y } where S; X = K, X = P; Y = V; Y = Q;
  --
  -- If the predicate has no value, we can simplify
  --
  --   X where S; X = K, X = P

  case maybeValDef of
    Just valDef -> do
      x <- fresh predicateKeyType
      y <- fresh predicateValueType
      return $
        TcQuery predicateKeyType (Ref (MatchVar x)) (Just (Ref (MatchVar y))) $
          stmts ++ [
            TcStatement predicateKeyType (Ref (MatchBind x)) keyDef,
            TcStatement predicateKeyType (Ref (MatchVar x)) key',
            TcStatement predicateValueType (Ref (MatchBind y)) valDef,
            TcStatement predicateValueType (Ref (MatchVar y)) val' ]
    Nothing -> do
      x <- fresh predicateKeyType
      return $
        TcQuery predicateKeyType (Ref (MatchVar x)) Nothing $
          stmts ++ [
            TcStatement predicateKeyType (Ref (MatchBind x)) keyDef,
            TcStatement predicateKeyType (Ref (MatchVar x)) key' ]

-- | Make a fresh instance of a query where none of the variables
-- clash with existing variables. We know the maximum variable in the
-- query, so we just reserve this number of variables and add the
-- current value of exNextVar to each variable in the query.
instantiateWithFreshVariables :: TcQuery -> Int -> F TcQuery
instantiateWithFreshVariables query numVars = do
  state <- get
  let base = flNextVar state
  put state { flNextVar = base + numVars }
  return $ instantiateQuery base query
  where
  instantiateQuery base (TcQuery ty head maybeVal stmts) =
    TcQuery ty
      (instantiatePat base head)
      (fmap (instantiatePat base) maybeVal)
      (map (instantiateStmt base) stmts)

  instantiateStmt base (TcStatement ty lhs rhs) =
    TcStatement ty (instantiatePat base lhs) (instantiatePat base rhs)

  instantiateTcTerm base (TcOr a b) =
    TcOr (instantiatePat base a) (instantiatePat base b)
  instantiateTcTerm base (TcFactGen pid kpat vpat range) =
    TcFactGen pid (instantiatePat base kpat) (instantiatePat base vpat) range
  instantiateTcTerm base (TcElementsOfArray pat) =
    TcElementsOfArray (instantiatePat base pat)
  instantiateTcTerm base (TcElements pat) =
    TcElements (instantiatePat base pat)
  instantiateTcTerm base (TcQueryGen query) =
    TcQueryGen (instantiateQuery base query)
  instantiateTcTerm base (TcNegation stmts) =
    TcNegation (map (instantiateStmt base) stmts)
  instantiateTcTerm base (TcPrimCall op args) =
    TcPrimCall op (map (instantiatePat base) args)
  instantiateTcTerm base (TcIf (Typed ty cond) then_ else_) =
    TcIf
      (Typed ty $ instantiatePat base cond)
      (instantiatePat base then_)
      (instantiatePat base else_)
  instantiateTcTerm base (TcDeref ty valTy pat) =
    TcDeref ty valTy (instantiatePat base pat)
  instantiateTcTerm base (TcFieldSelect (Typed ty pat) field) =
    TcFieldSelect (Typed ty (instantiatePat base pat)) field
  instantiateTcTerm base (TcAltSelect (Typed ty pat) field) =
    TcAltSelect (Typed ty (instantiatePat base pat)) field
  instantiateTcTerm base (TcPromote ty pat) =
    TcPromote ty (instantiatePat base pat)
  instantiateTcTerm base (TcStructPat fs) =
    TcStructPat [(n, instantiatePat base p) | (n,p) <- fs]

  instantiatePat :: Int -> TcPat -> TcPat
  instantiatePat base = fmap (instantiateMatch base)

  instantiateMatch base m = case m of
    MatchVar (Var ty v nm) -> MatchVar (Var ty (v+base) nm)
    MatchBind (Var ty v nm) -> MatchBind (Var ty (v+base) nm)
    MatchAnd l r -> MatchAnd (instantiatePat base l) (instantiatePat base r)
    MatchPrefix s t -> MatchPrefix s (instantiatePat base t)
    MatchExt (Typed ty ext) -> MatchExt (Typed ty (instantiateTcTerm base ext))
    MatchArrayPrefix ty pre -> MatchArrayPrefix ty (instantiatePat base <$> pre)
    other -> other

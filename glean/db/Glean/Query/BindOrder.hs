-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Query.BindOrder
  ( Fix
  , Scope(..)
  , IsPat(..)
  , runFixBindOrder
  , FixBindOrder(..)
  , FixBindOrderError(..)
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Glean.Query.Codegen
import Glean.Query.Typecheck.Types
import Glean.RTS.Term as RTS hiding (Match(..))

-- -----------------------------------------------------------------------------
-- Fixing up MatchBind vs. MatchVar

-- Substitution can mess up MatchBind and MatchVar - we might end up
-- with multiple MatchBinds for a variable, or a MatchVar before a
-- MatchBind. To fix it up all we need to do is traverse the query in
-- the correct order, keeping track of which variables are in scope,
-- and change MatchBind<->MatchVar as appropriate.
--
-- This is all somewhat suboptimal, because the typechecker has
-- already figured out MatchBind vs. MatchVar and here we mess it up
-- by substitution and then fix it again. Which begs the question: why
-- do we have MatchBind and MatchVar at all, couldn't we leave it
-- until the last minute just before code generation to figure out
-- which variables are binding occurrences?  Yes, but it's nice to be
-- able to give the user out-of-scope error messages from the
-- typechecker.  Maybe we'll change this in the future.

newtype Scope = Scope { unScope :: IntSet }

data FixBindOrderError
  = CannotUseWildcardInExpr
  | UnboundVariable Var

type Fix a = StateT Scope (Except FixBindOrderError) a

runFixBindOrder :: Scope -> Fix a -> Except FixBindOrderError (a, Scope)
runFixBindOrder scope fx = runStateT fx scope

encloseFix :: Fix a -> Fix a
encloseFix f = do inScope <- get; r <- f; put inScope; return r

data IsPat = IsPat | IsExpr

class FixBindOrder a where
  fixBindOrder :: IsPat -> a -> Fix a

fixPat, fixExpr :: FixBindOrder a => a -> Fix a
fixPat = fixBindOrder IsPat
fixExpr = fixBindOrder IsExpr

instance FixBindOrder TcQuery where
  fixBindOrder _ (TcQuery ty key val stmts) = do
    stmts' <- mapM (fixBindOrder IsPat) stmts -- stmts bind variables first
    key' <- mapM (fixBindOrder IsExpr) key
    val' <- mapM (mapM (fixBindOrder IsExpr)) val
    return $ TcQuery ty key' val' stmts'

instance FixBindOrder TcStatement where
  fixBindOrder _ (TcStatement ty lhs rhs) = do
    rhs' <- fixBindOrder IsExpr rhs
    lhs' <- fixBindOrder IsPat lhs
    return (TcStatement ty lhs' rhs')

instance FixBindOrder Generator where
  fixBindOrder isPat (TermGenerator p) =
    TermGenerator <$> fixBindOrder isPat p
  fixBindOrder _ (ArrayElementGenerator ty p) =
    ArrayElementGenerator ty <$> fixBindOrder IsExpr p
  fixBindOrder isPat (DerivedFactGenerator pid key val) =
    DerivedFactGenerator pid
      <$> fixBindOrder isPat key
      <*> fixBindOrder isPat val
  fixBindOrder _ (FactGenerator pid kpat vpat) =
    FactGenerator pid <$> fixBindOrder IsPat kpat <*> fixBindOrder IsPat vpat
  fixBindOrder _ (PrimCall op pats) =
    PrimCall op <$> mapM (fixBindOrder IsExpr) pats

instance FixBindOrder a => FixBindOrder (Term a) where
  fixBindOrder isPat term = mapM (fixBindOrder isPat) term

instance FixBindOrder TcTerm where
  fixBindOrder isPat (TcQueryGen query) =
    TcQueryGen <$> fixBindOrder isPat query
  fixBindOrder _ (TcFactGen pid kpat vpat) =
    TcFactGen pid <$> fixPat kpat <*> fixPat vpat
  fixBindOrder _ (TcElementsOfArray expr) =
    TcElementsOfArray <$> fixExpr expr
  fixBindOrder isPat (TcOr a b) = do
    TcOr
      <$> encloseFix (mapM (fixBindOrder isPat) a)
      <*> encloseFix (mapM (fixBindOrder isPat) b)
  fixBindOrder _ (TcPrimCall op pats) =
    TcPrimCall op <$> mapM (fixBindOrder IsExpr) pats

instance FixBindOrder a => FixBindOrder (Typed a) where
  fixBindOrder isPat (Typed ty a) = Typed ty <$> fixBindOrder isPat a

instance FixBindOrder () where
  fixBindOrder _ () = return ()

instance (FixBindOrder a) => FixBindOrder (Match a Var) where
  fixBindOrder IsExpr MatchWild{} =
    throwError CannotUseWildcardInExpr
  fixBindOrder isPat (MatchBind var) = fixVar isPat var
  fixBindOrder isPat (MatchVar var) = fixVar isPat var
  fixBindOrder isPat (MatchAnd a b) =
    MatchAnd <$> mapM (fixBindOrder isPat) a <*> mapM (fixBindOrder isPat) b
  fixBindOrder isPat (MatchPrefix str rest) =
    MatchPrefix str <$> mapM (fixBindOrder isPat) rest
  fixBindOrder isPat (MatchSum alts) =
    MatchSum <$> mapM (mapM (mapM (fixBindOrder isPat))) alts
  fixBindOrder isPat (MatchExt ext) = MatchExt <$> fixBindOrder isPat ext
  fixBindOrder _ other = return other

fixVar :: IsPat -> Var -> Fix (Match a Var)
fixVar isPat var@(Var _ v _) = do
  Scope inScope <- get
  if
    | v `IntSet.member` inScope -> return (MatchVar var)
    | IsExpr <- isPat ->
        throwError $ UnboundVariable var
    | otherwise -> do
      put (Scope (IntSet.insert v inScope))
      return (MatchBind var)

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Glean.Query.Flatten.Types
  ( F
  , initialFlattenState
  , getPredicateDetails
  , FlattenState(..)
  , FlattenedQuery
  , FlatQuery_(..)
  , FlatQuery
  , FlatStatement(..)
  , falseStmt
  , FlatStatementGroup(..)
  , Ordered(..)
  , unOrdered
  , grouping
  , flatDisjunction
  , mkStatementGroup
  , singletonGroup
  , boundVars
  , boundVarsOf
  , boundVarsOfOrdStmt
  , boundVarsOfGen
  , freshWildStmt
  , freshWildGen
  , freshWildQuery
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import Compat.Prettyprinter hiding ((<>))

import Glean.Angle.Types ( PredicateId )
import Glean.Query.Codegen.Types
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Database.Types (EnableRecursion(..))
import Glean.Display
import Glean.RTS.Types as RTS
import Glean.Query.Vars


type FlattenedQuery = QueryWithInfo FlatQuery

data FlatQuery_ p = FlatQuery p (Maybe p) FlatStatementGroup
  -- Statements are grouped by the flattener:

type FlatQuery = FlatQuery_ Pat

-- | A FlatStatementGroup is a non-empty group of FlatStatement.
--
-- A group of statements arise from a single statement in the
-- un-flattened Angle code.  The statements within each group will be
-- rearranged by the Reorder pass, but the relative order of the
-- groups will be retained.  In other words: we don't change the order
-- of statements you write, but we'll try to optimise the order of
-- matches in a nested match.
newtype FlatStatementGroup = FlatStatementGroup [Ordered FlatStatement]
  deriving Show

data Ordered s
  = Ordered s
  | Floating s
  deriving (Show, Functor, Foldable, Traversable)

unOrdered :: Ordered s -> s
unOrdered (Ordered s) = s
unOrdered (Floating s) = s

singletonGroup :: FlatStatement -> FlatStatementGroup
singletonGroup s = FlatStatementGroup [Floating s]

instance Display pat => Display (FlatQuery_ pat) where
  display opts (FlatQuery key maybeVal g@(FlatStatementGroup stmts)) =
    case stmts of
      [] -> head
      _ -> hang 2 (sep [head <+> "where", display opts g])
    where
    head = display opts key <>
      maybe mempty (\val -> " -> " <> display opts val) maybeVal

instance Display FlatStatementGroup where
  display opts (FlatStatementGroup stmts)
    = sep [hang 2 (sep ["[", p stmts]), "]"]
    where
    p stmts = sep (punctuate ";" (map (display opts) stmts))

instance Display (Ordered FlatStatement) where
  display opts (Ordered s) = display opts s
  display opts (Floating s) = "~" <+> display opts s

data FlatStatement
  = FlatStatement Type Pat Generator
    -- ^ A simple statement: P = gen
  | FlatAllStatement Var Pat FlatStatementGroup
    -- ^ An all() statement.
    -- @FlatAllStatement X P S@ means @X = all (P where S)@
    -- Like disjunction and negation, Variables occurring in
    -- @P where S@ are not considered bound.
  | FlatNegation FlatStatementGroup
    -- ^ The negation of a series of statements
  | FlatDisjunction [FlatStatementGroup]
    -- ^ A disjunction of alternatives: (stmts; ...) | ... | (stmts; ...)
    -- As a special case, if there is just one alternative this construct
    -- is used for nesting groups within a FlatStatementGroup. See
    -- Glean.Query.Flatten.floatGroups.
  | FlatConditional
      { cond :: FlatStatementGroup
      , then_ :: FlatStatementGroup
      , else_ :: FlatStatementGroup
      }
    -- ^ An if-then-else statement.
    -- If the statements in the condition match, the 'then' statements are
    -- evaluated, otherwide the 'else' statements are.

  deriving Show

-- | Smart constructor for a subgroup of statements, ensures we don't
-- create unnecessary nested singleton groups.
grouping :: FlatStatementGroup -> FlatStatement
grouping (FlatStatementGroup [one]) = unOrdered one
grouping group = FlatDisjunction [group]

-- | Flatten nested FlatDisjunction
flatDisjunction :: [FlatStatementGroup] -> FlatStatement
flatDisjunction groups =
  FlatDisjunction
    [ group
    | g <- groups
    , group <- case g of
        FlatStatementGroup [Ordered (FlatDisjunction gs)] -> gs
        FlatStatementGroup [Floating (FlatDisjunction gs)] -> gs
        _ -> [g]
    ]

-- | Smart constructor for a FlatStatementGroup, as with "grouping"
-- this flattens unnecessary nesting.
mkStatementGroup :: [Ordered FlatStatement] -> FlatStatementGroup
mkStatementGroup [Ordered (FlatDisjunction [group])] = group
mkStatementGroup [Floating (FlatDisjunction [group])] = group
mkStatementGroup ord = FlatStatementGroup ord

instance VarsOf FlatStatementGroup where
  varsOf w (FlatStatementGroup stmts) r = foldr (varsOf w) r stmts

instance VarsOf (Ordered FlatStatement) where
  varsOf w = varsOf w . unOrdered

instance VarsOf FlatStatement where
  varsOf w s r = case s of
    FlatStatement _ lhs rhs -> varsOf w lhs $! varsOf w rhs r
    FlatAllStatement (Var _ v _) pat stmts ->
      IntSet.insert v $! varsOf w pat $! varsOf w stmts r
    FlatNegation stmts      -> varsOf w stmts r
    FlatDisjunction stmtss  -> foldr (varsOf w) r stmtss
    FlatConditional cond then_ else_ ->
      foldr (varsOf w) r [cond, then_, else_]

freshWildQuery :: (Monad m, Fresh m) => FlatQuery -> m FlatQuery
freshWildQuery (FlatQuery p v stmts) =
  FlatQuery
    <$> freshWild p
    <*> mapM freshWild v
    <*> freshWildGroup stmts

freshWildStmt :: (Monad m, Fresh m) => FlatStatement -> m FlatStatement
freshWildStmt (FlatStatement ty pat gen) = do
  pat' <- freshWild pat
  gen' <- freshWildGen gen
  return (FlatStatement ty pat' gen')
freshWildStmt (FlatAllStatement var pat stmts) = do
  pat' <- freshWild pat
  stmts' <- freshWildGroup stmts
  return (FlatAllStatement var pat' stmts')
freshWildStmt (FlatNegation group) =
  FlatNegation <$> freshWildGroup group
freshWildStmt (FlatDisjunction alts) =
  FlatDisjunction <$> mapM freshWildGroup alts
freshWildStmt (FlatConditional cond then_ else_) =
  FlatConditional
    <$> freshWildGroup cond
    <*> freshWildGroup then_
    <*> freshWildGroup else_

freshWildGroup
  :: (Monad m, Fresh m)
  => FlatStatementGroup
  -> m FlatStatementGroup
freshWildGroup (FlatStatementGroup ord) =
  FlatStatementGroup <$> mapM (mapM freshWildStmt) ord

freshWildGen :: (Monad m, Fresh m) => Generator -> m Generator
freshWildGen gen = case gen of
  FactGenerator pid pat val sect ->
    FactGenerator pid
      <$> freshWild pat
      <*> freshWild val
      <*> pure sect
  TermGenerator expr -> TermGenerator <$> freshWild expr
  DerivedFactGenerator pid key val ->
    DerivedFactGenerator pid
      <$> freshWild key
      <*> freshWild val
  ArrayElementGenerator ty expr ->
    ArrayElementGenerator ty <$> freshWild expr
  SetElementGenerator ty expr ->
    SetElementGenerator ty <$> freshWild expr
  PrimCall op args ty ->
    PrimCall op <$> mapM freshWild args <*> pure ty

-- | Like 'varsOf', but only including variables that can be bound by
-- this statement.
boundVars :: FlatStatement -> VarSet
boundVars stmt = boundVarsOf stmt IntSet.empty

boundVarsOf :: FlatStatement -> VarSet -> VarSet
boundVarsOf (FlatStatement _ lhs rhs) r =
  varsOf AllVars lhs (boundVarsOfGen rhs r)
boundVarsOf (FlatAllStatement (Var _ v _) p stmts) r =
  IntSet.insert v $! varsOf AllVars p $! boundVarsOfGroup stmts r
boundVarsOf (FlatNegation _) r = r -- a negated query cannot bind variables
boundVarsOf (FlatDisjunction []) r = r
boundVarsOf (FlatDisjunction groups) r = s `IntSet.union` r
  where
  s = foldr1 IntSet.intersection $
    map (\g -> boundVarsOfGroup g IntSet.empty) groups
boundVarsOf (FlatConditional cond then_ else_) r =
  varsThen `IntSet.intersection` varsElse
  where
    varsThen = boundVarsOfGroup cond $ boundVarsOfGroup then_ r
    varsElse = boundVarsOfGroup else_ r

boundVarsOfGroup :: FlatStatementGroup -> VarSet -> VarSet
boundVarsOfGroup (FlatStatementGroup ord) r =
  foldr boundVarsOfOrdStmt r ord

boundVarsOfOrdStmt :: Ordered FlatStatement -> VarSet -> VarSet
boundVarsOfOrdStmt s = boundVarsOf (unOrdered s)

boundVarsOfGen :: Generator -> VarSet -> VarSet
boundVarsOfGen DerivedFactGenerator{} r = r
boundVarsOfGen ArrayElementGenerator{} r = r
boundVarsOfGen PrimCall{} r = r
boundVarsOfGen other r = varsOf AllVars other r

-- | a statement that always fails
falseStmt :: FlatStatement
falseStmt = FlatDisjunction []

instance Display FlatStatement where
  display opts = \case
    FlatStatement _ lhs rhs ->
      hang 2 $ sep [display opts lhs <+> "=", display opts rhs ]
    FlatAllStatement v e stmts ->
      display opts v <+> "=" <+> "all" <+>
        sep [hang 2 (display opts stmts), display opts e]
    FlatNegation group ->
      "!" <> display opts group
    FlatDisjunction groupss ->
      sep (punctuate " |" (map (display opts) groupss))
    FlatConditional cond then_ else_ -> sep
      [ nest 2 $ sep ["if", display opts cond ]
      , nest 2 $ sep ["then", display opts then_]
      , nest 2 $ sep ["else", display opts else_]
      ]

data FlattenState = FlattenState
  { flDbSchema :: DbSchema
  , flNextVar :: Int
  , flDeriveStored :: Maybe PredicateId
    -- ^ we should derive this DerivedAndStored predicate
  , flStack :: [PredicateId]
    -- ^ Stack of derived predicates, to prevent recursion. (for now,
    -- until we have support for recursion).
  , flRecursion :: EnableRecursion
  }

getPredicateDetails :: PredicateId -> F PredicateDetails
getPredicateDetails pred = do
  dbSchema <- gets flDbSchema
  case lookupPredicateId pred dbSchema of
    Nothing -> error $ "predicateKeyTYpe: " <> show (displayDefault pred)
    Just d -> return d

initialFlattenState
  :: EnableRecursion
  -> DbSchema
  -> Int
  -> Maybe PredicateId
  -> FlattenState
initialFlattenState rec dbSchema nextVar deriveStored = FlattenState
  { flDbSchema = dbSchema
  , flNextVar = nextVar
  , flDeriveStored = deriveStored
  , flStack = []
  , flRecursion = rec
  }

type F a = StateT FlattenState (Except Text) a

instance Fresh (StateT FlattenState (Except Text)) where
  peek = gets flNextVar
  alloc = do
    state@FlattenState{..} <- get
    put state { flNextVar = flNextVar + 1 }
    return flNextVar

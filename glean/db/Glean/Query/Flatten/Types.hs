module Glean.Query.Flatten.Types
  ( F
  , initialFlattenState
  , FlattenState(..)
  , fresh
  , FlattenedQuery
  , FlatQuery_(..)
  , FlatQuery
  , FlatStatement(..)
  , falseStmt
  , FlatStatementGroup
  , singletonGroup
  ) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))

import Glean.Angle.Types hiding (Type)
import Glean.Query.Codegen
import Glean.Database.Schema
import Glean.RTS.Types as RTS


type FlattenedQuery = QueryWithInfo FlatQuery

data FlatQuery_ p = FlatQuery p (Maybe p) [FlatStatementGroup]
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
type FlatStatementGroup = NonEmpty FlatStatement

singletonGroup :: FlatStatement -> FlatStatementGroup
singletonGroup s = s :| []

instance Pretty pat => Pretty (FlatQuery_ pat) where
  pretty (FlatQuery key maybeVal stmts) = case stmts of
    [] -> head
    _ -> hang 2 (sep (head <+> "where" : map prettyGroup stmts))
    where
    head = pretty key <> maybe mempty (\val -> " -> " <> pretty val) maybeVal
    prettyGroup stmts =
      brackets (align (sep (punctuate ";"
        (map pretty (NonEmpty.toList stmts)))))

data FlatStatement
  = FlatStatement Type Pat Generator
    -- ^ A simple statement: P = gen
  | FlatDisjunction [[FlatStatementGroup]]
    -- ^ A disjunction of alternatives: (stmts; ...) | ... | (stmts; ...)
    -- As a special case, if there is just one alternative this construct
    -- is used for nesting groups within a FlatStatementGroup. See
    -- Glean.Query.Flatten.floatGroups.
  deriving Show

-- | a statement that always fails
falseStmt :: FlatStatement
falseStmt = FlatDisjunction []

instance Pretty FlatStatement where
  pretty (FlatStatement _ lhs rhs) =
    hang 2 $ sep [pretty lhs <+> "=", pretty rhs ]
  pretty (FlatDisjunction groupss) =
    sep (punctuate " |" (map doStmts groupss))
    where
    doStmts groups =
      hang 2 (sep [sep ("(" : punctuate ";" (map pretty groups)), ")"])


data FlattenState = FlattenState
  { flDbSchema :: DbSchema
  , flNextVar :: Int
  , flDeriveStored :: Maybe PredicateRef
    -- ^ we should derive this DerivedAndStored predicate
  , flStack :: [PredicateRef]
    -- ^ Stack of derived predicates, to prevent recursion. (for now,
    -- until we have support for recursion).
  }

initialFlattenState :: DbSchema -> Int -> Maybe PredicateRef -> FlattenState
initialFlattenState dbSchema nextVar deriveStored = FlattenState
  { flDbSchema = dbSchema
  , flNextVar = nextVar
  , flDeriveStored = deriveStored
  , flStack = []
  }

type F a = StateT FlattenState (Except Text) a


fresh :: Type -> F Var
fresh ty = do
  state@FlattenState{..} <- get
  put state { flNextVar = flNextVar + 1 }
  return (Var ty flNextVar Nothing)

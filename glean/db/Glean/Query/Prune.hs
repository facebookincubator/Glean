{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Prune (pruneDerivations) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Glean.Database.Schema.Types (PredicateDetails(..))
import Glean.Query.Codegen (Match(..), QueryWithInfo(..))
import Glean.RTS.Types (PidRef(..))
import Glean.Angle.Types (PredicateId(..), DerivingInfo(..))
import Glean.Query.Typecheck (tcQueryDeps)
import Glean.Query.Typecheck.Types
  (TcPat, TcTerm(..), Typed(..), TcStatement(..), TcQuery(..), TypecheckedQuery)
import Glean.Query.Transform (renumberVars)
import Glean.RTS.Term (Term(..))

-- | Remove all branches that we know will not succeed given the content of the
-- database.
pruneDerivations
  :: (PredicateId -> Bool)
  -> HashMap PredicateId PredicateDetails
  -> HashMap PredicateId PredicateDetails
pruneDerivations hasStoredFacts details =
  updateDerivation <$> details
  where
    updateDerivation details = fromMaybe details $ do
      Derive when _ <- return $ predicateDeriving details
      let newDerivation =
            case HashMap.lookup (predicateId details) pruned of
              Nothing -> NoDeriving
              Just query -> Derive when query
      return $ details { predicateDeriving = newDerivation }

    pruned :: HashMap PredicateId TypecheckedQuery
    pruned = pruneQueries hasStoredFacts derivations

    derivations :: HashMap PredicateId TypecheckedQuery
    derivations = flip HashMap.mapMaybe details $ \PredicateDetails{..} ->
      case predicateDeriving of
        NoDeriving -> Nothing
        Derive _ query -> Just query

pruneQueries
  :: (PredicateId -> Bool)
  -> HashMap PredicateId TypecheckedQuery
  -> HashMap PredicateId TypecheckedQuery
pruneQueries hasStoredFacts derivations =
  foldr add mempty $ topologicalSort derivations
  where
    add :: (PredicateId, TypecheckedQuery)
        -> HashMap PredicateId TypecheckedQuery
        -> HashMap PredicateId TypecheckedQuery
    add (predId, query) derivedWithFacts =
       case prune (hasFacts predId derivedWithFacts) query of
          Nothing -> derivedWithFacts
          Just pruned -> HashMap.insert predId pruned derivedWithFacts

    hasFacts predId derivedWithFacts child =
      child == predId
        || hasStoredFacts child
        || child `HashMap.member` derivedWithFacts

topologicalSort
  :: HashMap PredicateId TypecheckedQuery
  -> [(PredicateId, TypecheckedQuery)]
topologicalSort derivations =
  [ entry
  | vertex <- Graph.topSort graph
  , let (entry,_,_) = fromVertex vertex
  ]
  where
    (graph, fromVertex, _) = Graph.graphFromEdges edges
    edges =
      [ (entry, predId, dependencies)
      | entry@(predId, query) <- HashMap.toList derivations
      , let dependencies = Set.toList $ tcQueryDeps $ qiQuery query
      ]

-- | Remove paths that we know will not yield any result.
-- This will save us a lot of compilation at query time.
--
-- We can determine that a path is doomed to fail if it has a FactGenerator
-- that will search on a predicate with no facts.
--
-- Pruning won't leave us with unbound variables because:
-- - If a statement in a sequence is set to fail, the entire sequence is removed
-- - If one sequence is removed from a disjunction, all variables bound by it
--   should also be bound by the other disjunction alternatives
prune :: (PredicateId -> Bool) -> TypecheckedQuery -> Maybe TypecheckedQuery
prune hasFacts (QueryWithInfo q _ t) = do
  renumberVars t <$> pruneTcQuery q
  where
  pruneTcQuery :: TcQuery -> Maybe TcQuery
  pruneTcQuery (TcQuery ty keyPat mvalPat stmts) =
    TcQuery ty
      <$> prunePat keyPat
      <*> case mvalPat of
            Nothing -> Just Nothing
            Just v -> Just <$> prunePat v
      <*> pruneSequence stmts

  pruneSequence :: [TcStatement] -> Maybe [TcStatement]
  pruneSequence = traverse pruneStmt

  pruneStmt :: TcStatement -> Maybe TcStatement
  pruneStmt (TcStatement ty lhs rhs) =
    TcStatement ty <$> prunePat lhs <*> prunePat rhs

  prunePat :: TcPat -> Maybe TcPat
  prunePat pat = case pat of
    Byte{} -> Just pat
    Nat{} -> Just pat
    ByteArray{} -> Just pat
    String{} -> Just pat
    Array xs -> Array <$> traverse prunePat xs
    Tuple xs -> Tuple <$> traverse prunePat xs
    Alt i x -> Alt i <$> prunePat x
    Ref m -> case m of
      MatchWild{} -> Just pat
      MatchNever{} -> Nothing
      MatchFid{} -> Just pat
      MatchBind{} -> Just pat
      MatchVar{} -> Just pat
      MatchAnd a b -> Ref <$> (MatchAnd <$> prunePat a <*> prunePat b)
      MatchPrefix s x -> Ref . MatchPrefix s <$> prunePat x
      MatchArrayPrefix t xs -> Ref . MatchArrayPrefix t <$> traverse prunePat xs
      MatchExt (Typed ty tcterm) -> case tcterm of
        TcFactGen (PidRef _ predId) _ _
          | not $ hasFacts predId
          -> Nothing
        TcFactGen pidref k v -> Ref . MatchExt . Typed ty
          <$> (TcFactGen pidref <$> prunePat k <*> prunePat v)
        TcElementsOfArray x -> Ref . MatchExt . Typed ty . TcElementsOfArray
          <$> prunePat x
        TcQueryGen q ->
          Ref . MatchExt . Typed ty . TcQueryGen <$> pruneTcQuery q
        -- we dont' want to handle negation here because if it tries to match
        -- against things that are not in the database it should succeed.
        TcNegation{} -> Just pat
        TcPrimCall op xs -> Ref . MatchExt . Typed ty . TcPrimCall op
          <$> traverse prunePat xs
        TcIf (Typed t' c) t e ->
          let cond = prunePat c
              condt = Typed t' <$> cond
              then_ = prunePat t
              else_ = prunePat e
          in
          asum
            [ Ref . MatchExt . Typed ty <$> (TcIf <$> condt <*> then_ <*> else_)
            , Ref <$> (MatchAnd <$> cond <*> then_)
            , else_ :: Maybe TcPat
            ]
        TcOr a b ->
          let pa = prunePat a
              pb = prunePat b
          in
          asum
            [ Ref . MatchExt . Typed ty <$> (TcOr <$> pa <*> pb)
            , pa
            , pb
            ]

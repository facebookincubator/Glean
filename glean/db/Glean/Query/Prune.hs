{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Prune (pruneDerivations) where

import Control.Monad
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Bitraversable (bitraverse)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

import Glean.Database.Schema.Types (PredicateDetails(..))
import Glean.RTS.Types (PidRef(..), Type)
import Glean.Angle.Types (PredicateId(..), DerivingInfo(..))
import Glean.Query.Codegen.Types
  (Match(..), QueryWithInfo(..), Typed(..), Var(..))
import Glean.Query.Typecheck (tcQueryDeps)
import Glean.Query.Typecheck.Types
  (TcPat, TcTerm(..), TcStatement(..), TcQuery(..), TypecheckedQuery)
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
    Set xs -> Set <$> traverse prunePat xs
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
        TcFactGen (PidRef _ predId) _ _ _
          | not $ hasFacts predId
          -> Nothing
        TcFactGen pidref k v range -> Ref . MatchExt . Typed ty
          <$> (TcFactGen pidref <$> prunePat k <*> prunePat v <*> pure range)
        TcElementsOfArray x -> Ref . MatchExt . Typed ty . TcElementsOfArray
          <$> prunePat x
        TcElements x -> Ref . MatchExt . Typed ty . TcElements <$> prunePat x
        TcQueryGen q ->
          Ref . MatchExt . Typed ty . TcQueryGen <$> pruneTcQuery q
        -- we dont' want to handle negation here because if it tries to match
        -- against things that are not in the database it should succeed.
        TcAll query ->
          Ref . MatchExt . Typed ty . TcAll <$> pruneTcQuery query
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
        TcDeref ty' valTy p ->
          Ref . MatchExt . Typed ty . TcDeref ty' valTy <$> prunePat p
        TcFieldSelect (Typed ty' p) f -> do
          p' <- prunePat p
          return $ Ref $ MatchExt $ Typed ty $ TcFieldSelect (Typed ty' p') f
        TcAltSelect (Typed ty' p) f -> do
          p' <- prunePat p
          return $ Ref $ MatchExt $ Typed ty $ TcAltSelect (Typed ty' p') f
        TcPromote _ p -> prunePat p
        TcDemote _ p -> prunePat p
        TcStructPat{} -> error "prune: TcStructPat"

type R a = State S a

data S = S
  { nextVar :: Int
  , mappings :: IntMap Int
  }

-- | After removing branches from the query we must now update
-- variable names to ensure we use the smallest numbers possible.
renumberVars :: Type -> TcQuery -> TypecheckedQuery
renumberVars ty q =
  let (newQuery, S varCount _) = runState (renameQuery q) (S 0 mempty)
  in
  QueryWithInfo newQuery varCount ty
  where
  renameQuery :: TcQuery -> R TcQuery
  renameQuery (TcQuery ty key mval stmts) =
    TcQuery ty
      <$> renamePat key
      <*> traverse renamePat mval
      <*> traverse renameStmt stmts

  renameStmt :: TcStatement -> R TcStatement
  renameStmt (TcStatement ty lhs rhs) =
    TcStatement ty <$> renamePat lhs <*> renamePat rhs

  renamePat :: TcPat -> R TcPat
  renamePat = traverse (bitraverse renameTyped renameVar)

  renameTyped :: Typed TcTerm -> R (Typed TcTerm)
  renameTyped (Typed ty term) = Typed ty <$> renameTcTerm term

  renameTcTerm :: TcTerm -> R TcTerm
  renameTcTerm = \case
    TcOr a b -> TcOr <$> renamePat a <*> renamePat b
    TcFactGen ref k v range ->
      TcFactGen ref <$> renamePat k <*> renamePat v <*> pure range
    TcElementsOfArray x -> TcElementsOfArray <$> renamePat x
    TcElements x -> TcElements <$> renamePat x
    TcQueryGen q -> TcQueryGen <$> renameQuery q
    TcAll query -> TcAll <$> renameQuery query
    TcNegation xs -> TcNegation <$> traverse renameStmt xs
    TcPrimCall op xs -> TcPrimCall op <$> traverse renamePat xs
    TcIf cond then_ else_ ->
      TcIf <$> traverse renamePat cond <*> renamePat then_ <*> renamePat else_
    TcDeref ty valTy p -> TcDeref ty valTy <$> renamePat p
    TcFieldSelect (Typed ty p) f -> do
      p' <- renamePat p
      return $ TcFieldSelect (Typed ty p') f
    TcAltSelect (Typed ty p) f -> do
      p' <- renamePat p
      return $ TcAltSelect (Typed ty p') f
    TcPromote ty p -> TcPromote ty <$> renamePat p
    TcDemote ty p -> TcDemote ty <$> renamePat p
    TcStructPat fs -> fmap TcStructPat $ forM fs $ \(n,p) ->
      (n,) <$> renamePat p

  renameVar :: Var -> R Var
  renameVar (Var ty old n) = State.state $ \s ->
    case IntMap.lookup old (mappings s) of
      Just new -> (Var ty new n, s)
      Nothing ->
        let new = nextVar s
            mappings' = IntMap.insert old new (mappings s)
            next' = new + 1
        in
        (Var ty new n, S next' mappings')

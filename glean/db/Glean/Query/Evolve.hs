{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Glean.Query.Evolve
  ( evolveFlattenedQuery
  , unEvolveResults
  , Evolutions
  , fromEvolutions
  , toEvolutions
  ) where

import Control.Monad.State (State, runState, modify)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Vector (Vector)

import Glean.Angle.Types (FieldDef_(..))
import qualified Glean.Angle.Types as Type
import Glean.Query.Codegen
import Glean.Query.Flatten.Types
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.RTS.Types
import Glean.RTS.Term (Term(..))
import Glean.RTS.Foreign.Query (QueryResults(..))
import Glean.Schema.Util (tupleSchema)
import qualified Glean.Types as Thrift

-- | A map of the predicate transformations performed by evolves.
-- value was mapped into key
newtype Evolutions = Evolutions (IntMap PredicateEvolution)
  deriving newtype (Semigroup, Monoid)

toEvolutions :: DbSchema -> Map Int64 Int64 -> Evolutions
toEvolutions DbSchema{..} mappings = Evolutions $ IntMap.fromList
  [ (fromIntegral new, evolution)
  | (new, old) <- Map.toList mappings
  , Just evolution <- [IntMap.lookup (fromIntegral old) predicatesEvolution]
  ]

fromEvolutions :: Evolutions -> Map Int64 Int64
fromEvolutions (Evolutions e) = Map.fromList
  [ (fromIntegral new, toPid evolutionOld)
  | (new, PredicateEvolution{..}) <- IntMap.toList e
  ]
  where toPid = fromIntegral . fromPid . predicatePid

-- | Transform a query such that it operates on the most evolved
-- version available of the predicates it mentions.
evolveFlattenedQuery
  :: DbSchema
  -> FlattenedQuery
  -> (FlattenedQuery, Evolutions)
evolveFlattenedQuery DbSchema{..} q = flip runState mempty $ do
  evolveQueryWithInfo q
  where
    evolveQueryWithInfo (QueryWithInfo q vars ty) = do
      let FlatQuery key maybeVal stmts = q
      (ty', key') <- evolveReturnType ty key
      stmts' <- traverse evolveFlatStmtGroup stmts
      return $ QueryWithInfo (FlatQuery key' maybeVal stmts') vars ty'

    evolveReturnType ty pat
      | Type.Record fields <- derefType ty
      , [ resultTy, _, _ ] <- map (derefType . fieldDefType) fields
      , Type.Predicate (PidRef old _) <- derefType resultTy
      , Tuple [p, key, val] <- pat
      , Just PredicateEvolution{..} <-
          IntMap.lookup (intPid old) predicatesEvolution
      = do
        let new = evolutionNew
            ty' = tupleSchema
              [ Type.Predicate (pidRef new)
              , predicateKeyType new
              , predicateValueType new
              ]
            pat' = Tuple
              [ p
              , evolutionEvolveKey key
              , evolutionEvolveValue val
              ]
        modify $ addEvolutionsFor (predicatePid new : evolutionNested)
        return (ty', pat')
      | otherwise = return (ty, pat)

    pidRef details = PidRef (predicatePid details) (predicateRef details)

    evolveFlatStmtGroup = traverse evolveFlatStmt

    evolveFlatStmt = \case
      FlatNegation groups ->
        FlatNegation <$> traverse evolveFlatStmtGroup groups
      FlatDisjunction groupss ->
        FlatDisjunction <$> traverse (traverse evolveFlatStmtGroup) groupss
      FlatStatement ty pat gen -> do
        mEvolution <- evolveGenerator gen
        return $ case mEvolution of
          Nothing -> FlatStatement ty pat gen
          Just (PredicateEvolution{..}, newGen) ->
            FlatStatement ty (evolutionEvolveKey pat) newGen

    evolveGenerator
      :: Generator
      -> State Evolutions (Maybe (PredicateEvolution, Generator))
    evolveGenerator gen = case gen of
      -- these only deal with expressions, so there is no
      -- mapping to be done.
      TermGenerator{} -> return Nothing
      ArrayElementGenerator{} -> return Nothing
      PrimCall{} -> return Nothing

      -- todo
      DerivedFactGenerator{} -> return Nothing

      FactGenerator old key val ->
        case IntMap.lookup (intPid $ pid old) predicatesEvolution of
          Nothing -> return Nothing
          Just evolution@PredicateEvolution{..} -> do
            let new = pidRef evolutionNew
                key' = evolutionEvolveKey key
                val' = evolutionEvolveValue val
            modify $ addEvolutionsFor (pid old : evolutionNested)
            return $ Just (evolution, FactGenerator new key' val')

    pid (PidRef x _) = x

    intPid = fromIntegral . fromPid

    addEvolutionsFor :: [Pid] -> Evolutions -> Evolutions
    addEvolutionsFor preds (Evolutions evolutions) =
      Evolutions $ evolutions <> newEvolutions
      where
        newEvolutions = IntMap.fromList
          [ (fromIntegral (fromPid new), evolution)
          | old <- preds
          , Just evolution <- [IntMap.lookup (intPid old) predicatesEvolution]
          , let new = predicatePid (evolutionNew evolution)
          ]

-- | Transform evolved facts back into the type the query originally asked for.
unEvolveResults :: Evolutions -> QueryResults -> QueryResults
unEvolveResults (Evolutions evolutions) results@QueryResults{..}
  | IntMap.null evolutions = results
  | otherwise = results
    { queryResultsFacts = unEvolveFacts queryResultsFacts
    , queryResultsNestedFacts = unEvolveFacts queryResultsNestedFacts
    }
  where
    unEvolveFacts :: Vector (Fid, Thrift.Fact) -> Vector (Fid, Thrift.Fact)
    unEvolveFacts = fmap (fmap unEvolveFact)

    unEvolveFact :: Thrift.Fact -> Thrift.Fact
    unEvolveFact fact@(Thrift.Fact pid _ _) =
      case IntMap.lookup (fromIntegral pid) evolutions of
        Nothing -> fact
        Just PredicateEvolution{..} -> evolutionUnevolve  fact


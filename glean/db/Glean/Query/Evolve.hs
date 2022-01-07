{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Glean.Query.Evolve
  ( evolveTcQuery
  , evolveType
  , devolveType
  , evolutionsFor
  , devolveResults
  , Evolutions
  , fromEvolutions
  , toEvolutions
  ) where

import Data.Bifunctor
import Data.Bifoldable
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.IntMap.Strict as IntMap
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Glean.Angle.Types as Type
import Glean.Schema.Util (showPredicateRef)
import Glean.Query.Codegen
import Glean.Query.Typecheck.Types
import Glean.Database.Schema
import Glean.Database.Schema.Evolve (evolvePat, transitiveDeps)
import Glean.Database.Schema.Types
import Glean.RTS.Types
import Glean.RTS.Foreign.Query (QueryResults(..))
import qualified Glean.Types as Thrift

-- | A map of the predicate transformations performed by evolves.
-- value was mapped into key
newtype Evolutions = Evolutions (IntMap PredicateEvolution)
  deriving newtype (Semigroup, Monoid)

toEvolutions :: DbSchema -> Map Int64 Int64 -> Evolutions
toEvolutions schema mappings = Evolutions $ IntMap.fromList
  [ (fromIntegral new, evolution)
  | (new, old) <- Map.toList mappings
  , Just evolution <- [lookupEvolution (Pid old) schema]
  ]

fromEvolutions :: Evolutions -> Map Int64 Int64
fromEvolutions (Evolutions e) = Map.fromList
  [ (fromIntegral new, toPid evolutionOld)
  | (new, PredicateEvolution{..}) <- IntMap.toList e
  ]
  where toPid = fromIntegral . fromPid . predicatePid

-- ========================
-- Evolve TypecheckedQuery
-- ========================

evolveTcQuery :: DbSchema -> TcQuery -> TcQuery
evolveTcQuery schema q@(TcQuery ty _ _ _) = evolveTcQuery' ty Nothing q
  where
    evolveTcQuery' old mnew (TcQuery _ key mval stmts) =
      let new = fromMaybe (evolveType schema old) mnew in
      TcQuery new
        (evolve old new key)
        (evolveInnerPat <$> mval)
        (fmap evolveStmt stmts)

    evolveStmt (TcStatement old lhs rhs) =
      let new = evolveType schema old in
      TcStatement new
        (evolve old new lhs) -- could this just use evolveInnerPat instead?
        (evolve old new rhs)

    -- evolve inner structures in a TcPat
    evolveInnerPat :: TcPat -> TcPat
    evolveInnerPat = fmap (bimap overTyped overVar)
      where
        overTyped :: Typed TcTerm -> Typed TcTerm
        overTyped (Typed old term) =
          let new = evolveType schema old in
          Typed new (evolveTcTerm old new term)

        overVar :: Var -> Var
        overVar (Var old vid name) =
          let new = evolveType schema old in
          Var new vid name

    evolveTcTerm :: Type -> Type -> TcTerm -> TcTerm
    evolveTcTerm old newish term =
      let new = evolveType schema newish in
      case term of
        TcOr left right -> TcOr (evolve old new left) (evolve old new right)
        TcFactGen pref key val -> evolveTcFactGen pref key val
        TcElementsOfArray pat -> TcElementsOfArray $
          evolve (Type.Array old) (Type.Array new) pat
        TcQueryGen q -> TcQueryGen $ evolveTcQuery' old (Just new) q
        TcNegation stmts -> TcNegation $ fmap evolveStmt stmts
        TcPrimCall op pats -> TcPrimCall op $ evolveInnerPat <$> pats

    evolveTcFactGen :: PidRef -> TcPat -> TcPat -> TcTerm
    evolveTcFactGen pref key val =
      case lookupEvolution (pid pref) schema of
        Just evolution -> do
          TcFactGen (pidRef $ evolutionNew evolution)
            (evolveKey evolution key)
            (evolveValue evolution val)
        Nothing ->
          TcFactGen pref
            (evolveInnerPat key)
            (evolveInnerPat val)

    evolveKey :: PredicateEvolution -> TcPat -> TcPat
    evolveKey PredicateEvolution{..} pat = evolve
      (predicateKeyType evolutionOld)
      (predicateKeyType evolutionNew)
      pat

    evolveValue  :: PredicateEvolution -> TcPat -> TcPat
    evolveValue PredicateEvolution{..} pat = evolve
      (predicateValueType evolutionOld)
      (predicateValueType evolutionNew)
      pat

    evolve :: Type -> Type -> TcPat -> TcPat
    evolve = evolvePat overTyped overVar
      where
        overTyped old newish (Typed _ pat) =
          let new = evolveType schema newish in
          Typed new (evolveTcTerm old new pat)
        overVar _ newish (Var _ vid name) =
          let new = evolveType schema newish in
          Var new vid name

-- | Evolutions for a type and all its transitively nested types
-- It is an error if the type uses multiple old versions of an
-- evolved predicate.
evolutionsFor :: DbSchema -> Type -> Either Text Evolutions
evolutionsFor schema ty =
  if null repeated
  then Right evolutions
  else Left $ "multiple versions of evolved predicates: "
      <> Text.unlines (map showRepeated repeated)
  where
    detailsFor pid = case lookupPid pid schema of
      Nothing -> error $ "unknown predicate " <> show pid
      Just details -> details

    inType :: [Pid]
    inType = getPids ty
      where getPids = bifoldMap (pure . pid) (getPids . expandType)
            expandType (ExpandedType _ t) = t

    withDeps :: [Pid]
    withDeps = nubOrd
      inType <> concatMap (transitiveDeps detailsFor) inType

    -- values are mapped to the key
    mappings :: Map Pid (Set Pid)
    mappings =
      Map.fromListWith (<>)
        [ (new, Set.singleton old)
        | old <- withDeps
        , let new = case lookupEvolution old schema of
                Nothing -> old
                Just e -> predicatePid (evolutionNew e)
        ]

    repeated :: [(Pid, Set Pid)]
    repeated = Map.toList $ Map.filter ((> 1) . Set.size) mappings

    evolutions :: Evolutions
    evolutions = Evolutions $ IntMap.fromList
      [ (new, evolution)
      | evolution <- mapMaybe (`lookupEvolution` schema) withDeps
      , let new = intPid $ predicatePid $ evolutionNew evolution
      ]

    showRepeated :: (Pid, Set Pid) -> Text
    showRepeated (new, olds) =
      showRef new <> " evolves "
      <> Text.intercalate " and " (showRef <$> Set.toList olds)
      where showRef = showPredicateRef . predicateRef . detailsFor

-- | Evolve predicates inside the type but keep its structure.
evolveType :: DbSchema -> Type -> Type
evolveType schema ty = evolve ty
  where
    evolve ty = bimap overPidRef overExpandedType ty

    overPidRef pref =
      case lookupEvolution (pid pref) schema of
        Nothing -> pref
        Just PredicateEvolution{..} -> pidRef evolutionNew

    overExpandedType (ExpandedType tref ty) =
      ExpandedType tref (evolve ty)

lookupEvolution :: Pid -> DbSchema -> Maybe PredicateEvolution
lookupEvolution pid DbSchema{..} =
  IntMap.lookup (fromIntegral $ fromPid pid) predicatesEvolution

intPid :: Pid -> Int
intPid = fromIntegral . fromPid

pid :: PidRef -> Pid
pid (PidRef x _) = x

pidRef :: PredicateDetails -> PidRef
pidRef details = PidRef (predicatePid details) (predicateRef details)

-- ========================
-- Devolve
-- ========================

-- | Map predicates from new to old
devolveType :: Evolutions -> Type -> Type
devolveType (Evolutions evolutions) ty = devolve ty
  where
    devolve ty = bimap overPidRef overExpandedType ty

    overPidRef pref =
      case IntMap.lookup (fromIntegral $ fromPid $ pid pref) evolutions of
        Nothing -> pref
        Just PredicateEvolution{..} -> pidRef evolutionOld

    overExpandedType (ExpandedType tref ty) =
      ExpandedType tref (devolve ty)

-- | Transform evolved facts back into the type the query originally asked for.
devolveResults :: Evolutions -> QueryResults -> QueryResults
devolveResults (Evolutions evolutions) results@QueryResults{..}
  | IntMap.null evolutions = results
  | otherwise = results
    { queryResultsFacts = devolveFacts queryResultsFacts
    , queryResultsNestedFacts = devolveFacts queryResultsNestedFacts
    }
  where
    devolveFacts :: Vector (Fid, Thrift.Fact) -> Vector (Fid, Thrift.Fact)
    devolveFacts = fmap (fmap devolveFact)

    devolveFact :: Thrift.Fact -> Thrift.Fact
    devolveFact fact@(Thrift.Fact pid _ _) =
      case IntMap.lookup (fromIntegral pid) evolutions of
        Nothing -> fact
        Just PredicateEvolution{..} -> evolutionDevolve fact


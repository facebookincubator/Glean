{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Glean.Query.Transform
  ( transformQuery
  , transformType
  , undoTypeTransformation
  , transformationsFor
  , transformResultsBack
  , Transformations
  , fromTransformations
  , toTransformations
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
import Glean.Database.Schema.Transform (transformPat, transitiveDeps)
import Glean.Database.Schema.Types
import Glean.RTS.Types
import Glean.RTS.Foreign.Query (QueryResults(..))
import qualified Glean.Types as Thrift

-- | A map of predicate transformations applied to a query
-- Keyed by the predicate available in the database
newtype Transformations = Transformations (IntMap PredicateTransformation)
  deriving newtype (Semigroup, Monoid)

toTransformations :: DbSchema -> Map Int64 Int64 -> Transformations
toTransformations schema mappings = Transformations $ IntMap.fromList
  [ (fromIntegral available, transformation)
  | (available, requested) <- Map.toList mappings
  , Just transformation <- [lookupTransformation (Pid requested) schema]
  ]

fromTransformations :: Transformations -> Map Int64 Int64
fromTransformations (Transformations e) = Map.fromList
  [ (fromIntegral available, toPid tRequested)
  | (available, PredicateTransformation{..}) <- IntMap.toList e
  ]
  where toPid = fromIntegral . fromPid . predicatePid

-- ========================
-- Transform TypecheckedQuery
-- ========================

-- Transform types requested in the query into types available in the database.
transformQuery :: DbSchema -> TcQuery -> TcQuery
transformQuery schema q@(TcQuery ty _ _ _) = transformTcQuery ty Nothing q
  where
    transformTcQuery from mto (TcQuery _ key mval stmts) =
      let to = fromMaybe (transformType schema from) mto in
      TcQuery to
        (transform from to key)
        (transformInnerPat <$> mval)
        (fmap transformStmt stmts)

    transformStmt (TcStatement from lhs rhs) =
      let to = transformType schema from in
      TcStatement to
        (transform from to lhs) -- could this use transformInnerPat instead?
        (transform from to rhs)

    -- transform inner structures in a TcPat
    transformInnerPat :: TcPat -> TcPat
    transformInnerPat = fmap (bimap overTyped overVar)
      where
        overTyped :: Typed TcTerm -> Typed TcTerm
        overTyped (Typed from term) =
          let to = transformType schema from in
          Typed to (transformTcTerm from to term)

        overVar :: Var -> Var
        overVar (Var from vid name) =
          let to = transformType schema from in
          Var to vid name

    transformTcTerm :: Type -> Type -> TcTerm -> TcTerm
    transformTcTerm from to0 term =
      let to = transformType schema to0 in
      case term of
        TcOr left right -> TcOr
          (transform from to left)
          (transform from to right)
        TcIf (Typed fromc cond) then_ else_ ->
          let toc = transformType schema fromc in
          TcIf
            (Typed toc (transform fromc toc cond))
            (transform from to then_)
            (transform from to else_)
        TcFactGen pref key val -> transformTcFactGen pref key val
        TcElementsOfArray pat -> TcElementsOfArray $
          transform (Type.Array from) (Type.Array to) pat
        TcQueryGen q -> TcQueryGen $ transformTcQuery from (Just to) q
        TcNegation stmts -> TcNegation $ fmap transformStmt stmts
        TcPrimCall op pats -> TcPrimCall op $ transformInnerPat <$> pats

    transformTcFactGen :: PidRef -> TcPat -> TcPat -> TcTerm
    transformTcFactGen pref key val =
      case lookupTransformation (pid pref) schema of
        Just evolution -> do
          TcFactGen (pidRef $ tAvailable evolution)
            (transformKey evolution key)
            (transformValue evolution val)
        Nothing ->
          TcFactGen pref
            (transformInnerPat key)
            (transformInnerPat val)

    transformKey :: PredicateTransformation -> TcPat -> TcPat
    transformKey PredicateTransformation{..} pat = transform
      (predicateKeyType tRequested)
      (predicateKeyType tAvailable)
      pat

    transformValue  :: PredicateTransformation -> TcPat -> TcPat
    transformValue PredicateTransformation{..} pat = transform
      (predicateValueType tRequested)
      (predicateValueType tAvailable)
      pat

    transform :: Type -> Type -> TcPat -> TcPat
    transform = transformPat overTyped overVar
      where
        overTyped from to0 (Typed _ pat) =
          let to = transformType schema to0 in
          Typed to (transformTcTerm from to pat)
        overVar _ to0 (Var _ vid name) =
          let to = transformType schema to0 in
          Var to vid name

-- | Transformations for a type and all its transitively nested types
-- It is an error if the type uses multiple versions of the same predicate.
transformationsFor :: DbSchema -> Type -> Either Text Transformations
transformationsFor schema ty =
  if null repeated
  then Right transformations
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
        [ (to, Set.singleton from)
        | from <- withDeps
        , let to = case lookupTransformation from schema of
                Nothing -> from
                Just e -> predicatePid (tAvailable e)
        ]

    repeated :: [(Pid, Set Pid)]
    repeated = Map.toList $ Map.filter ((> 1) . Set.size) mappings

    transformations :: Transformations
    transformations = Transformations $ IntMap.fromList
      [ (to, evolution)
      | evolution <- mapMaybe (`lookupTransformation` schema) withDeps
      , let to = intPid $ predicatePid $ tAvailable evolution
      ]

    showRepeated :: (Pid, Set Pid) -> Text
    showRepeated (to, froms) =
      showRef to <> " evolves "
      <> Text.intercalate " and " (showRef <$> Set.toList froms)
      where showRef = showPredicateRef . predicateRef . detailsFor

-- | Transform predicates inside the type but keep its structure.
transformType :: DbSchema -> Type -> Type
transformType schema ty = transform ty
  where
    transform ty = bimap overPidRef overExpandedType ty

    overPidRef pref =
      case lookupTransformation (pid pref) schema of
        Nothing -> pref
        Just PredicateTransformation{..} -> pidRef tAvailable

    overExpandedType (ExpandedType tref ty) =
      ExpandedType tref (transform ty)

lookupTransformation :: Pid -> DbSchema -> Maybe PredicateTransformation
lookupTransformation pid DbSchema{..} =
  IntMap.lookup (fromIntegral $ fromPid pid) predicatesTransformations

intPid :: Pid -> Int
intPid = fromIntegral . fromPid

pid :: PidRef -> Pid
pid (PidRef x _) = x

pidRef :: PredicateDetails -> PidRef
pidRef details = PidRef (predicatePid details) (predicateRef details)

-- ========================
-- Transform back
-- ========================

-- | Revert a transformation. Goes from predicate available in the db to
-- predicate requested in the query
undoTypeTransformation :: Transformations -> Type -> Type
undoTypeTransformation (Transformations transformations) ty =
  undoTransformation ty
  where
    undoTransformation ty = bimap overPidRef overExpandedType ty

    overPidRef pref =
      case IntMap.lookup (fromIntegral $ fromPid $ pid pref) transformations of
        Nothing -> pref
        Just PredicateTransformation{..} -> pidRef tRequested

    overExpandedType (ExpandedType tref ty) =
      ExpandedType tref (undoTransformation ty)

-- | Transform facts back into the type the query originally asked for.
transformResultsBack :: Transformations -> QueryResults -> QueryResults
transformResultsBack (Transformations trans) results@QueryResults{..}
  | IntMap.null trans = results
  | otherwise = results
    { queryResultsFacts = overFacts queryResultsFacts
    , queryResultsNestedFacts = overFacts queryResultsNestedFacts
    }
  where
    overFacts :: Vector (Fid, Thrift.Fact) -> Vector (Fid, Thrift.Fact)
    overFacts = fmap (fmap overFact)

    overFact :: Thrift.Fact -> Thrift.Fact
    overFact fact@(Thrift.Fact pid _ _) =
      case IntMap.lookup (fromIntegral pid) trans of
        Nothing -> fact
        Just PredicateTransformation{..} -> tTransformFactBack fact

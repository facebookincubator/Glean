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
  , transformTypecheckedQuery
  , transformationsFor
  , transformResultsBack
  , Transformations
  , fromTransformations
  , toTransformations
  , renumberVars
  ) where

import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.Bifoldable
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List.Extra (nubOrd, elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import qualified Data.IntMap.Strict as IntMap
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text.Prettyprint.Doc (pretty)

import qualified Glean.Angle.Types as Type
import Glean.Angle.Types (PredicateId, Type_(..), FieldDef_(..))
import Glean.Schema.Util (showRef, lowerEnum, lowerMaybe, lowerBool)
import Glean.Query.Codegen.Types
  (Match(..), Var(..), QueryWithInfo(..), Typed(..))
import Glean.Query.Typecheck.Types
import Glean.Database.Schema.Transform (defaultValue)
import Glean.Database.Schema.Types
import Glean.RTS.Term (Term(..))
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
  , Just transformation <- [lookupTransformation (Pid requested) tmap]
  ]
  where tmap = predicatesTransformations schema


fromTransformations :: Transformations -> Map Int64 Int64
fromTransformations (Transformations e) = Map.fromList
  [ (fromIntegral available, toPid tRequested)
  | (available, PredicateTransformation{..}) <- IntMap.toList e
  ]
  where
    toPid (PidRef pid _) = fromIntegral $ fromPid pid

-- ========================
-- Transform TypecheckedQuery
-- ========================

transformTypecheckedQuery
  :: HashMap PredicateId PredicateDetails
  -> IntMap PredicateTransformation
  -> TypecheckedQuery
  -> TypecheckedQuery
transformTypecheckedQuery pmap tmap (QueryWithInfo query v ty) =
  if True
     then QueryWithInfo (transformQuery pmap tmap query) v ty
     else renumberVars ty $ transformQuery pmap tmap query

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
    TcFactGen ref k v -> TcFactGen ref <$> renamePat k <*> renamePat v
    TcElementsOfArray x -> TcElementsOfArray <$> renamePat x
    TcQueryGen q -> TcQueryGen <$> renameQuery q
    TcNegation xs -> TcNegation <$> traverse renameStmt xs
    TcPrimCall op xs -> TcPrimCall op <$> traverse renamePat xs
    TcIf cond then_ else_ ->
      TcIf <$> traverse renamePat cond <*> renamePat then_ <*> renamePat else_

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

-- Transform types requested in the query into types available in the database.
transformQuery
  :: HashMap PredicateId PredicateDetails
  -> IntMap PredicateTransformation
  -> TcQuery
  -> TcQuery
transformQuery pmap tmap q@(TcQuery ty _ _ _) = transformTcQuery ty Nothing q
  where
    getDetails (PidRef _ predId) =
      fromMaybe (error err) $ HashMap.lookup predId pmap
      where err = "transformQuery: unknown predicate: " <> show (pretty predId)

    transformTcQuery from mto (TcQuery _ key mval stmts) =
      let to = fromMaybe (transformType tmap from) mto in
      TcQuery to
        (transform from to key)
        (transformInnerPat <$> mval)
        (fmap transformStmt stmts)

    transformStmt (TcStatement from lhs rhs) =
      let to = transformType tmap from in
      TcStatement to
        (transform from to lhs) -- could this use transformInnerPat instead?
        (transform from to rhs)

    -- transform inner structures in a TcPat
    transformInnerPat :: TcPat -> TcPat
    transformInnerPat = fmap (bimap overTyped overVar)
      where
        overTyped :: Typed TcTerm -> Typed TcTerm
        overTyped (Typed from term) =
          let to = transformType tmap from in
          Typed to (transformTcTerm from to term)

        overVar :: Var -> Var
        overVar (Var from vid name) =
          let to = transformType tmap from in
          Var to vid name

    transformTcTerm :: Type -> Type -> TcTerm -> TcTerm
    transformTcTerm from to0 term =
      let to = transformType tmap to0 in
      case term of
        TcOr left right -> TcOr
          (transform from to left)
          (transform from to right)
        TcIf (Typed fromc cond) then_ else_ ->
          let toc = transformType tmap fromc in
          TcIf
            (Typed toc (transform fromc toc cond))
            (transform from to then_)
            (transform from to else_)
        TcFactGen pref key val -> transformTcFactGen pref key val
        TcElementsOfArray pat -> TcElementsOfArray $
          transform (Type.ArrayTy from) (Type.ArrayTy to) pat
        TcQueryGen q -> TcQueryGen $ transformTcQuery from (Just to) q
        TcNegation stmts -> TcNegation $ fmap transformStmt stmts
        TcPrimCall op pats -> TcPrimCall op $ transformInnerPat <$> pats

    transformTcFactGen :: PidRef -> TcPat -> TcPat -> TcTerm
    transformTcFactGen pref key val =
      case lookupTransformation (pid pref) tmap of
        Just evolution -> do
          TcFactGen (tAvailable evolution)
            (transformKey evolution key)
            (transformValue evolution val)
        Nothing ->
          TcFactGen pref
            (transformInnerPat key)
            (transformInnerPat val)

    transformKey :: PredicateTransformation -> TcPat -> TcPat
    transformKey PredicateTransformation{..} pat = transform
      (predicateKeyType $ getDetails tRequested)
      (predicateKeyType $ getDetails tAvailable)
      pat

    transformValue  :: PredicateTransformation -> TcPat -> TcPat
    transformValue PredicateTransformation{..} pat = transform
      (predicateValueType $ getDetails tRequested)
      (predicateValueType $ getDetails tAvailable)
      pat

    overTyped from to0 (Typed _ pat) =
      let to = transformType tmap to0 in
      Typed to (transformTcTerm from to pat)

    overVar _ to0 (Var _ vid name) =
      let to = transformType tmap to0 in
      Var to vid name

    transform :: Type -> Type -> TcPat -> TcPat
    transform from@(NamedTy _) to pat = transform (derefType from) to pat
    transform from to@(NamedTy _) pat = transform from (derefType to) pat
    transform from to pat = case pat of
      Byte x -> Byte x
      Nat x -> Nat x
      ByteArray x -> ByteArray x
      String x -> String x
      Ref match -> Ref $ case match of
        -- we can keep variable bindings as they are given any value of type T
        -- assigned to a variable will have been changed to type transformed(T).
        MatchBind var -> MatchBind $ overVar from to var
        MatchVar var -> MatchVar $ overVar from to var
        MatchWild _ -> MatchWild to
        MatchNever _ -> MatchNever to
        MatchFid fid -> MatchFid fid
        MatchAnd a b -> MatchAnd
          (transform from to a)
          (transform from to b)
        MatchPrefix prefix rest -> MatchPrefix prefix $ transform from to rest
        MatchArrayPrefix _ty prefix
          | ArrayTy fromElem <- from
          , ArrayTy toElem <- to
          -> MatchArrayPrefix toElem (map (transform fromElem toElem) prefix)
          | otherwise -> error "unexpected"
        MatchExt extra -> MatchExt $ overTyped from to extra
      Alt fromIx term
        | BooleanTy <- from
        , BooleanTy <- to ->
            transform lowerBool lowerBool pat
        | MaybeTy fromTy <- from
        , MaybeTy toTy <- to ->
            transform (lowerMaybe fromTy) (lowerMaybe toTy) pat
        | EnumeratedTy fromAlts <- from
        , EnumeratedTy toAlts <- to ->
            transform
              (lowerEnum fromAlts)
              (lowerEnum toAlts)
              pat
        | SumTy fromAlts <- from
        , SumTy toAlts <- to
        -- alternatives could change order
        , Just fromAlt <- fromAlts `maybeAt` fromIntegral fromIx
        , Just toIx <-
            elemIndex (fieldDefName fromAlt) (fieldDefName <$> toAlts)
        , Just toAlt <- toAlts `maybeAt` toIx ->
            Alt (fromIntegral toIx) $ transform
              (fieldDefType fromAlt)
              (fieldDefType toAlt)
              term
      Array terms
        | ArrayTy fromTy <- from
        , ArrayTy toTy <- to ->
          Array $ transform fromTy toTy <$> terms
      Tuple terms
        | RecordTy fromFields <- from
        , RecordTy toFields <- to
        ->
          let
            fromMap = Map.fromList
              [ (name, (fromTy, term))
              | (FieldDef name fromTy, term) <- zip fromFields terms ]

            termForField  (FieldDef name toTy) =
              case Map.lookup name fromMap of
                Just (fromTy, term) -> transform fromTy toTy term
                -- Field in 'to' missing in 'from'.
                -- We can accept any value here.
                Nothing -> Ref (MatchWild toTy)

            -- fields in 'from' missing in 'to'
            extraFields = Map.elems $ fromMap `Map.withoutKeys` toFieldNames
              where toFieldNames = Set.fromList $ map fieldDefName toFields

            -- Imagine we have
            --
            --  predicate P.1 { x : Q }
            --  predicate P.2 { x : Q, y : maybe R }
            --
            -- If we are converting a query for P.2 like
            --
            --    _ = P.2 { X, Y }
            --
            -- into one for P.1, we want the result to be:
            --
            --    _ = (P.1 { X } where Y = <default value for R>)
            --
            matchDefaultValue (fromTy, term) =
              TcStatement fromTy term (defaultValue fromTy)

            where_ p ss =
              Ref $ MatchExt $ Typed to $ TcQueryGen $
              TcQuery to p Nothing ss

            transformed = Tuple $ fmap termForField toFields
          in
          if null extraFields
          then transformed
          else transformed `where_` map matchDefaultValue extraFields

      _ -> error "unexpected"

    maybeAt list ix = listToMaybe (drop ix list)


-- | Transformations for a type and all its transitively nested types
-- It is an error if the type uses multiple versions of the same predicate.
transformationsFor :: DbSchema -> Type -> Either Text Transformations
transformationsFor schema ty =
  if null repeated
  then Right transformations
  else Left $ "multiple versions of evolved predicates: "
      <> Text.unlines (map showRepeated repeated)
  where
    tmap = predicatesTransformations schema

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
        , let to = case lookupTransformation from tmap of
                Nothing -> from
                Just e -> toPid $ tAvailable e
        ]

    repeated :: [(Pid, Set Pid)]
    repeated = Map.toList $ Map.filter ((> 1) . Set.size) mappings

    transformations :: Transformations
    transformations = Transformations $ IntMap.fromList
      [ (to, evolution)
      | evolution <- mapMaybe (`lookupTransformation` tmap) withDeps
      , let to = intPid $ toPid  $ tAvailable evolution
      ]

    showRepeated :: (Pid, Set Pid) -> Text
    showRepeated (to, froms) =
      showPid to <> " evolves "
      <> Text.intercalate " and " (showPid <$> Set.toList froms)
      where showPid = showRef . predicateRef . detailsFor

toPid :: PidRef -> Pid
toPid (PidRef pid _) = pid

transitiveDeps :: (Pid -> PredicateDetails) -> Pid -> [Pid]
transitiveDeps = transitive . predicateDeps
  where
    -- All predicates mentioned in a predicate's type.
    -- Does not include predicates from the derivation query.
    predicateDeps :: (Pid -> PredicateDetails) -> Pid -> [Pid]
    predicateDeps detailsFor pred =
      typeDeps (predicateKeyType details) <>
        typeDeps (predicateValueType details)
      where
        details = detailsFor pred
        typeDeps = bifoldMap overPidRef overExpanded
        overExpanded (ExpandedType _ ty) = typeDeps ty
        overPidRef (PidRef pid _) = [pid]

    transitive :: Ord a => (a -> [a]) -> a -> [a]
    transitive next root = Set.elems $ go (next root) mempty
      where
        go [] visited = visited
        go (x:xs) visited
          | x `Set.member`visited = go xs visited
          | otherwise = go xs $ go (next x) $ Set.insert x visited

-- | Transform predicates inside the type but keep its structure.
transformType :: IntMap PredicateTransformation -> Type -> Type
transformType tmap ty = transform ty
  where
    transform ty = bimap overPidRef overExpandedType ty

    overPidRef pref =
      case lookupTransformation (pid pref) tmap of
        Nothing -> pref
        Just PredicateTransformation{..} -> tAvailable

    overExpandedType (ExpandedType tref ty) =
      ExpandedType tref (transform ty)

lookupTransformation
  :: Pid
  -> IntMap PredicateTransformation
  -> Maybe PredicateTransformation
lookupTransformation pid tmap =
  IntMap.lookup (fromIntegral $ fromPid pid) tmap

intPid :: Pid -> Int
intPid = fromIntegral . fromPid

pid :: PidRef -> Pid
pid (PidRef x _) = x

-- ========================
-- Transform back
-- ========================

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

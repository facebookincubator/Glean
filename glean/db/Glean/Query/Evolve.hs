{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
  {-# LANGUAGE DerivingStrategies #-}
module Glean.Query.Evolve
  ( evolveFlattenedQuery
  , unEvolveResults
  , Evolutions
  , fromEvolutions
  , toEvolutions
  ) where

import Control.Monad.State (State, runState, modify)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector (Vector)

import Glean.Angle.Types (FieldDef_(..), Type_)
import qualified Glean.Angle.Types as Type
import Glean.Query.Codegen
import Glean.Query.Flatten.Types
import Glean.Database.Schema
import qualified Glean.RTS as RTS
import Glean.RTS.Types
import Glean.RTS.Term (Value, Term(..))
import Glean.RTS.Foreign.Query (QueryResults(..))
import Glean.Schema.Util (lowerMaybe, lowerBool, lowerEnum, tupleSchema)
import qualified Glean.Types as Thrift

-- | A map of the predicate transformations performed by evolves.
-- value was mapped into key
newtype Evolutions = Evolutions (Map Pid Pid)
  deriving newtype (Semigroup, Monoid)

toEvolutions :: Map Int64 Int64 -> Evolutions
toEvolutions = Evolutions . mapKeyAndValue Pid

fromEvolutions :: Evolutions -> Map Int64 Int64
fromEvolutions (Evolutions e) = mapKeyAndValue fromPid e

mapKeyAndValue :: Ord b => (a -> b) -> Map a a -> Map b b
mapKeyAndValue f = Map.fromList . map (both f) . Map.toList
  where both f (a, b) = (f a, f b)

data EvolveFact = EvolveFact
  { evolvedPidRef :: PidRef
  , evolveKey :: Pat -> Pat
  , evolveValue :: Pat -> Pat
  }

-- | Transform a query such that it operates on the most evolved
-- version available of the predicates it mentions.
evolveFlattenedQuery
  :: DbSchema
  -> FlattenedQuery
  -> (FlattenedQuery, Evolutions)
evolveFlattenedQuery dbSchema@DbSchema{..} q = flip runState mempty $ do
  evolveQueryWithInfo q
  where
    evolveQueryWithInfo (QueryWithInfo (FlatQuery key maybeVal stmts) vars ty ) = do
      (ty', key') <- evolveReturnType ty key
      stmts' <- traverse evolveFlatStmtGroup stmts
      return $ QueryWithInfo (FlatQuery key' maybeVal stmts') vars ty'

    evolveReturnType ty pat
      | Type.Record fields <- derefType ty
      , [ resultTy, _, _ ] <- map (derefType . fieldDefType) fields
      , Type.Predicate old <- derefType resultTy
      , Tuple [p, key, val] <- pat
      = do
        mEvolver <- evolverForPredicate old
        return $ fromMaybe (ty, pat) $ do
          EvolveFact{..} <- mEvolver
          PredicateDetails{..} <- lookupPid (pid evolvedPidRef) dbSchema
          let ty' = tupleSchema
                [ Type.Predicate evolvedPidRef
                , predicateKeyType
                , predicateValueType
                ]
              pat' = Tuple [p, evolveKey key, evolveValue val]
          return (ty', pat')
      | otherwise = return (ty, pat)

    evolveFlatStmtGroup = traverse evolveFlatStmt

    evolveFlatStmt = \case
      FlatNegation groups ->
        FlatNegation <$> traverse evolveFlatStmtGroup groups
      FlatDisjunction groupss ->
        FlatDisjunction <$> traverse (traverse evolveFlatStmtGroup) groupss
      FlatStatement ty pat gen -> do
        mEvolved <- evolveGenerator gen
        return $ case mEvolved of
          Nothing -> FlatStatement ty pat gen
          Just (evolve, newGen) -> FlatStatement ty (evolve pat) newGen

    evolveGenerator
      :: Generator
      -> State Evolutions (Maybe (Pat -> Pat, Generator))
    evolveGenerator gen = case gen of
      -- these only deal with expressions, so there is no
      -- mapping to be done.
      TermGenerator{} -> return Nothing
      ArrayElementGenerator{} -> return Nothing
      PrimCall{} -> return Nothing

      -- todo
      DerivedFactGenerator{} -> return Nothing

      FactGenerator old key val -> do
        mEvolver <- evolverForPredicate old
        return $ do
          EvolveFact{..} <- mEvolver
          let new = evolvedPidRef
              key' = evolveKey key
              val' = evolveValue val
          return (evolveKey, FactGenerator new key' val')

    evolverForPredicate :: PidRef -> State Evolutions (Maybe EvolveFact)
    evolverForPredicate old = sequence $ do
      new <- Map.lookup old predicatesEvolved
      newTy <- lookupPid (pid new) dbSchema
      oldTy <- lookupPid (pid old) dbSchema
      let evolveKey = evolvePat
              (predicateKeyType oldTy)
              (predicateKeyType newTy)
          evolveValue = evolvePat
              (predicateValueType oldTy)
              (predicateValueType newTy)
      return $ do
        modify (addEvolution new old)
        return $ EvolveFact new evolveKey evolveValue

    addEvolution new old (Evolutions e) =
      Evolutions $ Map.insert (pid new) (pid old) e

    pid (PidRef x _) = x

    maybeAt list ix = listToMaybe (drop ix list)

    evolvePat
      :: Type_ PidRef ExpandedType
      -> Type_ PidRef ExpandedType
      -> Term (Match () Var)
      -> Term (Match () Var)
    evolvePat old new pat = case pat of
      Byte _ -> pat
      Nat _ -> pat
      ByteArray _ -> pat
      String _ -> pat
      Ref match -> Ref $ case match of
        -- we can keep variable bindings as they are given any value of type T
        -- assigned to a variable will have been changed to type evolved(T).
        MatchBind _ -> match
        MatchVar _ -> match
        MatchWild _ -> MatchWild new
        MatchNever _ -> MatchNever new
        MatchFid fid
          -- we don't want to have a type mismatch on the fact, so
          -- unless the type didn't change we will always fail to match.
          | old == new -> MatchFid fid
          | otherwise -> MatchNever new
        MatchAnd a b -> MatchAnd (evolvePat old new a) (evolvePat old new b)
        MatchSum mterms -> MatchSum $ map (fmap $ evolvePat old new) mterms
        MatchPrefix prefix rest -> MatchPrefix prefix $ evolvePat old new rest
        MatchExt () -> match
      Alt oldIx term
        | Type.Boolean <- old
        , Type.Boolean <- new ->
            evolvePat lowerBool lowerBool pat
        | Type.Maybe oldTy <- old
        , Type.Maybe newTy <- new ->
            evolvePat (lowerMaybe oldTy) (lowerMaybe newTy) pat
        | Type.Enumerated oldAlts <- old
        , Type.Enumerated newAlts <- new ->
            evolvePat
              (lowerEnum oldAlts)
              (lowerEnum newAlts)
              pat
        | Type.Sum oldAlts <- old
        , Type.Sum newAlts <- new
        -- alternatives could change order
        , Just oldAlt <- oldAlts `maybeAt` fromIntegral oldIx
        , Just newIx <-
            elemIndex (fieldDefName oldAlt) (fieldDefName <$> newAlts)
        , Just newAlt <- newAlts `maybeAt` newIx ->
            Alt (fromIntegral newIx) $ evolvePat
              (fieldDefType oldAlt)
              (fieldDefType newAlt)
              term
      Array terms
        | Type.Array oldTy <- old
        , Type.Array newTy <- new ->
          Array $ evolvePat oldTy newTy <$> terms
      Tuple terms
        | Type.Record oldFields <- old
        , Type.Record newFields <- new
        ->  let termsMap = Map.fromList
                  [ (name, (oldTy, term))
                  | (FieldDef name oldTy, term) <- zip oldFields terms ]
                termForField  (FieldDef name newTy) =
                  case Map.lookup name termsMap of
                    Just (oldTy, term) -> evolvePat oldTy newTy term
                    Nothing -> Ref (MatchWild newTy)
            in
            Tuple (termForField <$> newFields)
      _ -> error "unexpected"

-- | Transform evolved facts back into the type the query originally asked for.
unEvolveResults :: DbSchema -> Evolutions -> QueryResults -> QueryResults
unEvolveResults dbschema (Evolutions mappings) results@QueryResults{..}
  | Map.null mappings = results
  | otherwise = results
    { queryResultsFacts = unEvolveFacts queryResultsFacts
    , queryResultsNestedFacts = unEvolveFacts queryResultsNestedFacts
    }
  where
    unEvolveFacts :: Vector (Fid, Thrift.Fact) -> Vector (Fid, Thrift.Fact)
    unEvolveFacts = fmap (fmap unEvolveFact)

    unEvolveFact :: Thrift.Fact -> Thrift.Fact
    unEvolveFact fact@(Thrift.Fact pid _ _) =
      case IntMap.lookup (fromIntegral pid) transformations of
        Nothing -> fact
        Just f -> f fact

    transformations :: IntMap (Thrift.Fact -> Thrift.Fact)
    transformations = IntMap.fromList
      [ (pid, f)
      | (from , to) <- Map.toList mappings
      , pid <- [fromIntegral $ fromPid from]
      , Just f <- return $ do
          detailsFrom <- lookupPid from dbschema
          detailsTo <- lookupPid to dbschema
          mkFactTransformation detailsFrom detailsTo
      ]

mkFactTransformation
  :: PredicateDetails
  -> PredicateDetails
  -> Maybe (Thrift.Fact -> Thrift.Fact)
mkFactTransformation from to
  | Nothing <- mTransformKey
  , Nothing <- mTransformValue = Nothing
  | otherwise = Just $ \(Thrift.Fact _ key value) ->
     Thrift.Fact
       (fromPid $ predicatePid to)
       (RTS.fromValue $ overKey $ RTS.toValue keyRep key)
       (RTS.fromValue $ overValue $ RTS.toValue valueRep value)
  where
    overKey = fromMaybe id mTransformKey
    overValue = fromMaybe id mTransformValue
    keyRep = repType $ predicateKeyType from
    valueRep = repType $ predicateValueType from
    mTransformKey = mkValueTransformation
      (predicateKeyType from)
      (predicateKeyType to)
    mTransformValue = mkValueTransformation
      (predicateValueType from)
      (predicateValueType to)

-- | Create a transformation from a term into another term of a compatible
-- type.  Returns Nothing if no change is needed.
mkValueTransformation :: Type -> Type -> Maybe (Value -> Value)
mkValueTransformation from to = go from to
  where
    names = map fieldDefName
    transformsFor from to = Map.fromList
      [ (name, f)
      | FieldDef name defFrom <- from
      , FieldDef nameTo defTo <- to
      , name == nameTo
      , Just f <- [go defFrom defTo]
      ]

    go :: Type -> Type -> Maybe (Value -> Value)
    go from@(Type.NamedType _) to = go (derefType from) to
    go from to@(Type.NamedType _) = go from (derefType to)
    go Type.Byte Type.Byte = Nothing
    go Type.Nat Type.Nat = Nothing
    go Type.String Type.String = Nothing
    go Type.Boolean Type.Boolean = Nothing
    go (Type.Maybe from) (Type.Maybe to) = go (lowerMaybe from) (lowerMaybe to)
    go (Type.Predicate _) (Type.Predicate _) = Nothing
    go (Type.Enumerated from) (Type.Enumerated to) =
      go (lowerEnum from) (lowerEnum to)
    go (Type.Array from) (Type.Array to) = do
      f <- go from to
      return $ \term -> case term of
        Array vs -> Array (map f vs)
        _ -> error $ "expected Array, got " <> show term

    go (Type.Record from) (Type.Record to) =
      let transformsMap = transformsFor from to
          orderedTransforms =
            [ (field, transform)
            | field <- names to
            , transform <- return $ fromMaybe id $ Map.lookup field transformsMap
            ]
          transforms = map snd orderedTransforms
          sameFieldOrder = and $ zipWith (==) (names from) (names to)
          noChanges = null transformsMap
          sameFields = names from == names to
      in
      if sameFields && noChanges
      then Nothing
      else Just $ \term -> case term of
        Tuple contents | sameFieldOrder -> Tuple
          [ transform value
          | (transform, value) <- zip transforms contents
          ]
        Tuple contents -> Tuple
          [ transform content
          | (field, transform) <- orderedTransforms
          , Just content <- [lookup field withNames]
          ]
          where withNames = zip (names from) contents
        _ -> error $ "expected Tuple, got " <> show term

    go (Type.Sum from) (Type.Sum to) =
      let sameOpts = names from == names to
          transforms = transformsFor from to
          noChanges = Map.null transforms
          altMap = Map.fromList
              [ (fromAlt, (toAlt, change))
              | (nameFrom, fromAlt) <- zip (names from) [0..]
              , (nameTo, toAlt) <- zip (names to) [0..]
              , nameFrom == nameTo
              , change <- [ fromMaybe id $ Map.lookup nameFrom transforms ]
              ]
      in
      if sameOpts && noChanges
        then Nothing
        else Just $ \term -> case term of
          Alt n content -> case Map.lookup n altMap of
            Nothing -> Alt n content
            Just (n', change) -> Alt n' (change content)
          _ -> error $ "expected Alt, got " <> show term
    go from to =
      error $ "invalid type conversion: "
        <> show from <> " to " <> show to

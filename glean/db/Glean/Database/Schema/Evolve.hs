{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema.Evolve
  ( mkPredicateEvolution
  ) where

import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set

import Glean.Angle.Types (FieldDef_(..), Type_)
import qualified Glean.Angle.Types as Type
import Glean.Query.Codegen
import Glean.Database.Schema.Types
import qualified Glean.RTS as RTS
import Glean.RTS.Types
import Glean.RTS.Term (Value, Term(..))
import Glean.Schema.Util (lowerMaybe, lowerBool, lowerEnum)
import qualified Glean.Types as Thrift

mkPredicateEvolution
  :: (Pid -> PredicateDetails)
  -> Pid
  -> Pid
  -> PredicateEvolution
mkPredicateEvolution detailsFor oldPid newPid =
  PredicateEvolution
    { evolutionOld = old
    , evolutionNew = new
    , evolutionEvolveKey = evolvePat
        (predicateKeyType old)
        (predicateKeyType new)
    , evolutionEvolveValue = evolvePat
        (predicateValueType old)
        (predicateValueType new)
    , evolutionUnevolve = fromMaybe id $
        mkFactTransformation new old
    , evolutionNested = transitive (predicateDeps detailsFor) oldPid
    }
  where
      new = detailsFor newPid
      old = detailsFor oldPid

-- All predicates mentioned in a predicate's type.
-- Does not include predicates from the derivation query.
predicateDeps :: (Pid -> PredicateDetails) -> Pid -> [Pid]
predicateDeps detailsFor pred =
  typeDeps (predicateKeyType details) <>
    typeDeps (predicateValueType details)
  where
    details = detailsFor pred
    typeDeps = \case
      Type.Predicate (PidRef pid _) -> [pid]
      Type.NamedType (ExpandedType _ ty) -> typeDeps ty
      Type.Record xs -> concatMap (typeDeps  . fieldDefType) xs
      Type.Sum xs -> concatMap (typeDeps . fieldDefType) xs
      Type.Array ty -> typeDeps ty
      Type.Maybe ty -> typeDeps ty
      Type.Byte -> mempty
      Type.Nat -> mempty
      Type.String -> mempty
      Type.Enumerated _ -> mempty
      Type.Boolean -> mempty

transitive :: Ord a => (a -> [a]) -> a -> [a]
transitive next root = Set.elems $ go (next root) mempty
  where
    go [] visited = visited
    go (x:xs) visited
      | x `Set.member`visited = go xs visited
      | otherwise = go xs $ go (next x) $ Set.insert x visited

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
  where
    maybeAt list ix = listToMaybe (drop ix list)

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
            , transform <- [fromMaybe id $ Map.lookup field transformsMap]
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

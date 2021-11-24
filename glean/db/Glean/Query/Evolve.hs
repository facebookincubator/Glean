{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Evolve
  ( evolveFlattenedQuery
  , unEvolveResults
  ) where

import Control.Monad.State (State, runState, modify)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
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
import Glean.Schema.Util (lowerMaybe, lowerBool, lowerEnum)
import qualified Glean.Types as Thrift

evolveFlattenedQuery
  :: DbSchema
  -> FlattenedQuery
  -> (FlattenedQuery, Map PidRef PidRef)
evolveFlattenedQuery dbSchema@DbSchema{..} q = flip runState mempty $ do
  query <- evolveFlatQuery (qiQuery q)
  return q { qiQuery = query }
  where
    evolveFlatQuery (FlatQuery key maybeVal stmts) =
      FlatQuery key maybeVal <$> traverse evolveFlatStmtGroup stmts
    evolveFlatStmtGroup = traverse evolveFlatStmt
    evolveFlatStmt = \case
      FlatNegation groups ->
        FlatNegation <$> traverse evolveFlatStmtGroup groups
      FlatDisjunction groupss ->
        FlatDisjunction <$> traverse (traverse evolveFlatStmtGroup) groupss
      FlatStatement ty pat gen -> do
        evolved <- evolveGenerator gen
        return $ case evolved of
          Nothing -> FlatStatement ty pat gen
          Just (evolve, newGen) -> FlatStatement ty (evolve pat) newGen

    evolveGenerator
      :: Generator
      -> State
          (Map PidRef PidRef)
          (Maybe (Pat -> Pat, Generator))
    evolveGenerator gen = case gen of
      -- these only deal with expressions, so there is no
      -- mapping to be done.
      TermGenerator{} -> return Nothing
      ArrayElementGenerator{} -> return Nothing
      PrimCall{} -> return Nothing

      -- todo
      DerivedFactGenerator{} -> return Nothing

      FactGenerator old key val -> sequence $ do
        new <- Map.lookup old predicatesEvolved
        newTy <- lookupPid (pid new) dbSchema
        oldTy <- lookupPid (pid old) dbSchema
        let evolveKey = evolvePat
                (predicateKeyType oldTy)
                (predicateKeyType newTy)
            evolveVal = evolvePat
                (predicateValueType oldTy)
                (predicateValueType newTy)
        return $ do
          modify (Map.insert new old)
          return (evolveKey , FactGenerator new (evolveKey key) (evolveVal val))

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

-- | Transform evolved facts into the type the query originally asked for.
unEvolveResults
  :: Map PidRef PidRef -- ^ Evolved mappings. key evolved value
  -> DbSchema
  -> QueryResults
  -> QueryResults
unEvolveResults mappings dbschema results@QueryResults{..}
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
      | (PidRef pidFrom _ , PidRef pidTo _) <- Map.toList mappings
      , pid <- [fromIntegral $ fromPid pidFrom]
      , Just f <- return $ do
          detailsFrom <- lookupPid pidFrom dbschema
          detailsTo <- lookupPid pidTo dbschema
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
mkValueTransformation = error "TODO"

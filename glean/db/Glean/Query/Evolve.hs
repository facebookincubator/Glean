{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Evolve
  ( evolveFlattenedQuery
  ) where

import Control.Monad.State (State, runState, modify)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)

import Glean.Angle.Types (FieldDef_(..), Type_)
import qualified Glean.Angle.Types as Type
import Glean.Query.Codegen
import Glean.Query.Flatten.Types
import Glean.Database.Schema
import Glean.RTS.Types
import Glean.RTS.Term (Term(..))
import Glean.Schema.Util (lowerMaybe, lowerBool, enumFields)

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
              (Type.Sum $ enumFields oldAlts)
              (Type.Sum $ enumFields newAlts)
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

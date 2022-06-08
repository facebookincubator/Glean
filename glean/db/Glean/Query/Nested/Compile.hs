{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Nested.Compile
  ( toGenerators
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.Text (Text)

import qualified Glean.Angle.Types as Angle
import Glean.Query.Codegen as Compiled
import Glean.Query.Opt
import Glean.Query.Reorder
import Glean.Query.Nested.Types
import Glean.Query.Nested
import Glean.Query.Transform
import Glean.Query.Flatten
import Glean.Query.Typecheck.Types
import Glean.RTS.Types
import qualified Glean.RTS.Term as RTS
import Glean.RTS.Term (Term)
import Glean.Database.Schema
import Glean.Schema.Util

-- | The monad state is the next fresh variable name, and the current
-- set of global statements.
type M a = StateT Int (Except Text) a

-- | Make a fresh variable
fresh :: M Int
fresh = do n <- get; put (n+1); return n

-- | Convert a nested query into a TcQuery
toGenerators
  :: DbSchema
  -> Bool -- ^ True <=> derive DerivedAndStored predicates
  -> PredicateDetails
  -> Term (RTS.Match (Nested Fid))
  -> Either Text (CodegenQuery, Transformations)
toGenerators dbSchema deriveStored details term =
  runExcept $ do
    (query, numVars) <- compileResult
    let typechecked = QueryWithInfo query numVars queryTy
    (flat, evolutions) <- flatten
      dbSchema
      Angle.latestAngleVersion
      deriveStored
      typechecked
    optimised <- optimise flat
    (,evolutions) <$> reorder dbSchema optimised
  where
  pid = predicatePid details
  ref = predicateId details
  ty = predicateKeyType details
  queryTy = tupleSchema [predTy, ty]
  pidRef = PidRef pid ref
  predTy = Angle.PredicateTy pidRef

  compileResult = flip runStateT 0 $ do
    pat <- nestedTerm ty term
    idv <- Var predTy <$> fresh <*> return Nothing
    let
      valpat = RTS.Ref (MatchWild (predicateValueType details))
      gen = TcFactGen pidRef pat valpat
    return $ TcQuery predTy
      (RTS.Ref (MatchVar idv))
      Nothing
      [ TcStatement predTy (RTS.Ref (MatchBind idv))
          (RTS.Ref (MatchExt (Typed predTy gen)))
      ]

  -- Compile a nested query into generators.
  nestedTerm
    :: Type
    -> Term (RTS.Match (Nested Fid))
    -> M TcPat
  nestedTerm ty term = case (ty,term) of
    (_, RTS.Byte b) -> return (RTS.Byte b)
    (_, RTS.Nat n) -> return (RTS.Nat n)
    (_, RTS.String s) -> return (RTS.String s)
    (_, RTS.ByteArray bs) -> return (RTS.ByteArray bs)
    (Angle.ArrayTy elemTy, RTS.Array vs) ->
      RTS.Array <$> mapM (nestedTerm elemTy) vs
    (Angle.RecordTy fieldDefs, RTS.Tuple fields) ->
      RTS.Tuple <$>
        zipWithM nestedTerm (map Angle.fieldDefType fieldDefs) fields
    (Angle.SumTy fields, RTS.Alt n term) ->
      RTS.Alt n <$>
        nestedTerm (Angle.fieldDefType (fields !! fromIntegral n)) term
    (ty, RTS.Ref match) -> nestedMatch ty match
    (Angle.NamedTy (ExpandedType _ ty), term) -> nestedTerm ty term
    (Angle.MaybeTy ty, term) -> nestedTerm (lowerMaybe ty) term
    (Angle.EnumeratedTy names, term) -> nestedTerm (lowerEnum names) term
    (Angle.BooleanTy, term) -> nestedTerm lowerBool term
    (ty,term) -> error $
      "nestedTerm: " <> show ty <> " " <> show (void term)

  -- Compile a Match into generators
  nestedMatch
    :: Type
    -> RTS.Match (Nested Fid)
    -> M TcPat
  nestedMatch ty RTS.Wildcard = return (RTS.Ref (MatchWild ty))
  nestedMatch ty RTS.Variable = do
    v <- Var ty <$> fresh <*> return Nothing
    return (RTS.Ref (MatchBind v))
  nestedMatch ty (RTS.PrefixWildcard s) =
    return (RTS.Ref (MatchPrefix s (RTS.Ref (MatchWild ty))))
  nestedMatch ty (RTS.PrefixVariable s) = do
    v <- Var ty <$> fresh <*> return Nothing
    return (RTS.Ref (MatchPrefix s (RTS.Ref (MatchBind v))))
  nestedMatch _ (RTS.MatchTerm (NestedRef fid)) =
    return (RTS.Ref (MatchFid fid))
  nestedMatch ty (RTS.MatchTerm (NestedPred _ (Just ids) _)) = do
    return (or ty [ RTS.Ref (MatchFid f) | f <- ids ])
  nestedMatch ty (RTS.MatchTerm
      (NestedPred PredicateDetails{..} Nothing (Just term)))
      | refutableNested term = do
    pat <- nestedTerm predicateKeyType term
    return (RTS.Ref (MatchExt
      (Typed ty (TcFactGen (PidRef predicatePid predicateId) pat
        (RTS.Ref (MatchWild predicateValueType))))))
  nestedMatch ty (RTS.MatchTerm NestedPred{}) =
    return (RTS.Ref (MatchWild ty))
  nestedMatch ty@(Angle.SumTy fields)
      (RTS.MatchTerm (NestedSum SumMatchThese alts))
    -- Just one alt: compile directly to a match on Alt
    | [(ix,ty,alt)] <-
        [ (ix,ty,alt)
        | (ix, Angle.FieldDef _ ty, Just alt) <- zip3 [0..] fields alts ] =
      RTS.Alt ix <$> nestedTerm ty alt

    | otherwise =
      fmap (or ty . catMaybes) $ forM (zip3 [0..] fields alts) $
        \(n, Angle.FieldDef _ altTy, maybeAlt) ->
          case maybeAlt of
            Nothing -> return Nothing
            Just alt -> do
              pat <- nestedTerm altTy alt
              return (Just (RTS.Alt n pat))

  nestedMatch ty (RTS.MatchTerm (NestedSum SumMatchAny _)) =
    return (RTS.Ref (MatchWild ty))
  nestedMatch ty (RTS.MatchTerm NestedArray{}) =
    return (RTS.Ref (MatchWild ty))
  nestedMatch (Angle.NamedTy (ExpandedType _ ty)) term = nestedMatch ty term
  nestedMatch Angle.BooleanTy term = nestedMatch lowerBool term
  nestedMatch (Angle.MaybeTy t) term = nestedMatch (lowerMaybe t) term
  nestedMatch (Angle.EnumeratedTy names) term = nestedMatch (lowerEnum names) term
  nestedMatch _ _ = error "nestedMatch"

  or ty = foldr f (RTS.Ref (MatchNever ty))
    where
    f (RTS.Ref MatchNever{}) b = b
    f a (RTS.Ref MatchNever{}) = a
    f a b = RTS.Ref (MatchExt (Typed ty (TcOr a b)))

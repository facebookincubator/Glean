{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema.Transform
  ( mkPredicateTransformation
  , defaultValue
  ) where

import Control.Monad.Cont
import Data.Coerce (Coercible, coerce)
import qualified Data.Map.Strict as Map
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Word (Word64)

import Glean.Angle.Types (Type_(..), FieldDef_(..))
import Glean.Bytecode.Types (Ty(..), Register)
import Glean.Database.Schema.Types
import Glean.Query.Codegen.Types (Match(..), Output, Typed(..))
import qualified Glean.RTS as RTS
import Glean.RTS.Bytecode.Code (Code, local)
import Glean.RTS.Bytecode.Gen.Issue (getOutput)
import Glean.RTS.Types as RTS
import Glean.RTS.Term (Term(..), Value)
import Glean.Schema.Util (lowerMaybe, lowerEnum)
import qualified Glean.Types as Thrift

mkPredicateTransformation
  :: (Pid -> PredicateDetails)
  -> Pid
  -> Pid
  -> Maybe PredicateTransformation
mkPredicateTransformation detailsFor fromPid toPid
  | fromPid == toPid = Nothing
  | otherwise = Just PredicateTransformation
    { tRequested = pidRef from
    , tAvailable = pidRef to
    , tTransformFactBack = fromMaybe id $
        mkFactTransformation to from

    , transformPrefix = transformPattern
        (predicateKeyType from)
        (predicateKeyType to)
    , transformKey = Nothing
    , transformValue = Nothing
    }
  where
      to = detailsFor toPid
      from = detailsFor fromPid

transformPattern
  :: Type
  -> Type
  -> Maybe
        ( Term (Match () Output)
        -> (Term (Match () Output) -> Code a)
        -> Code a)
transformPattern from to = do
  f <- transformTerm transformMatch from to
  return $ \term -> runCont (f term)

transformBytes
  :: Type
  -> Type
  -> Maybe (Bytes -> (Register 'BinaryOutputPtr -> Code a) -> Code a)
transformBytes = undefined

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

-- | Type compatibility is not checked in this function.
-- It assumes that a transformation is possible and required.
transformMatch
  :: Type
  -> Type
  -> (Term (Match () Output) -> Cont (Code x) (Term (Match () Output)))
  -> Match () Output
  -> Cont (Code x) (Match () Output)
transformMatch from to overTerm match = case match of
  MatchWild _ -> return $ MatchWild to
  MatchNever _ -> return $ MatchNever to
  MatchFid fid -> return $ MatchFid fid
  -- this transformation is for the creation of prefixes only and it
  -- must not support variable binding otherwise we could be binding
  -- variables to a value of the wrong type.
  MatchBind _ -> return $ MatchWild to
  MatchVar (Typed _ output) ->
    case transformBytes from to of
      Nothing -> return $ MatchVar $ Typed to output
      Just transform ->
        cont $ \r ->
          local $ \start end -> do
          getOutput output start end
          transform (Bytes start end) $ \transformed -> do
          r $ MatchVar $ Typed to transformed
  MatchAnd left right -> do
    left' <- overTerm left
    right' <- overTerm right
    return $ MatchAnd left' right'
  -- both types must be StringTy so this transformation is a no-op
  MatchPrefix prefix rest -> return $ MatchPrefix prefix rest
  MatchArrayPrefix _ prefix -> do
    res <- overTerm (Array prefix)
    case res of
      Array prefix' -> return $ MatchArrayPrefix to prefix'
      _ -> error "array transformation did not yield another array"
  MatchExt () -> return $ MatchExt ()

mkValueTransformation :: Type -> Type -> Maybe (Value -> Value)
mkValueTransformation from to =
  case transformTerm inner from to of
    Nothing -> Nothing
    Just f -> Just $ \ta -> f ta id
  where
    inner _ _ _ a f = f a

-- | Create a transformation from a term into another term of a compatible
-- type.  Returns Nothing if no change is needed.
transformTerm
  :: forall a b m. (Coercible a b, Show a, Show b, Monad m)
  => (Type -> Type -> (Term a -> m (Term b)) -> a -> m b)
  -> Type
  -> Type
  -> Maybe (Term a -> m (Term b))
transformTerm inner from to = go from to
  where
    -- Types are the same. No transformation is required
    id' :: Term a -> m (Term b)
    id' term = return (coerce term)

    names = map fieldDefName

    -- one entry for each common field
    transformationsFor
      :: [RTS.FieldDef]
      -> [RTS.FieldDef]
      -> Map Text (Maybe (Word64, Term a -> m (Term b)))
    transformationsFor from to =
      Map.intersectionWith trans fromFields toFields
      where
        trans (ixFrom, defFrom) (ixTo, defTo) =
          case go defFrom defTo of
            -- fields are identical
            Nothing | ixTo == ixFrom -> Nothing
            -- field order changed
            Nothing -> Just (ixTo, id')
            -- field content changed
            Just f -> Just (ixTo, f)

        fromFields :: Map Text (Word64, Type)
        fromFields = Map.fromList $ flip map (zip from [0..])
          $ \(FieldDef name def, ix) -> (name, (ix, def))

        toFields :: Map Text (Word64, Type)
        toFields = Map.fromList $ flip map (zip to [0..])
          $ \(FieldDef name def, ix) -> (name, (ix, def))

    go :: Type -> Type -> Maybe (Term a -> m (Term b))
    go from@(NamedTy _) to = go (derefType from) to
    go from to@(NamedTy _) = go from (derefType to)
    go ByteTy ByteTy = Nothing
    go NatTy NatTy = Nothing
    go StringTy StringTy = Nothing
    go BooleanTy BooleanTy = Nothing
    go (MaybeTy from) (MaybeTy to) = go (lowerMaybe from) (lowerMaybe to)
    go (PredicateTy _) (PredicateTy _) = Nothing
    go (EnumeratedTy from) (EnumeratedTy to) =
      go (lowerEnum from) (lowerEnum to)
    go (ArrayTy from) (ArrayTy to) = do
      f <- go from to
      return $ fix $ \rec term ->
        case term of
          Array vs -> Array <$> traverse f vs
          Ref a -> Ref <$> inner (ArrayTy from) (ArrayTy to) rec a
          _ -> error $ "expected Array, got " <> show term

    go (RecordTy from) (RecordTy to) =
      let transformations = transformationsFor from to
          sameFieldCount = length from == length to
          -- implies same field order as well
          sameFieldContents = Map.null (Map.filter isJust transformations)
          noChange = sameFieldCount && sameFieldContents
      in
      if noChange
      then Nothing
      else Just $ fix $ \rec term -> case term of
        Tuple contents -> do
          contents' <- sequence
            [ case Map.lookup name transMap of
                -- 'to' field doesn't exist in 'from'
                Nothing -> return $ defaultValue ty
                Just (content, Nothing) -> id' content
                Just (content, Just (_, transform)) -> transform content
            | FieldDef name ty <- to
            ]
          return (Tuple contents')
          where
            transMap = Map.intersectionWith (,) contentsByName transformations
            contentsByName = Map.fromList $ zip (names from) contents
        Ref a -> Ref <$> inner (RecordTy from) (RecordTy to) rec a
        _ -> error $ "expected Tuple, got " <> show term

    go (SumTy from) (SumTy to) =
      let transformations = transformationsFor from to
          safeAltCount = length from <= length to
          sameAltContents = Map.null (Map.filter isJust transformations)
          noChange = safeAltCount && sameAltContents
          transformationsByIx = Map.fromList
              [ (ixFrom, trans)
              | (name, ixFrom) <- zip (names from) [0..]
              , Just trans <- [Map.lookup name transformations]
              ]
          unknown = Alt (fromIntegral $ length to) (Tuple [])
      in
      if noChange
        then Nothing
        else Just $ fix $ \rec term -> case term of
          Alt n content -> case Map.lookup n transformationsByIx of
            -- alternative in 'from' doesn't exist in 'to'
            Nothing -> return unknown
            Just Nothing -> id' term
            Just (Just (n', transform)) -> do
              content' <- transform content
              return (Alt n' content')
          Ref a -> Ref <$> inner (SumTy from) (SumTy to) rec a
          _ -> error $ "expected Alt, got " <> show term
    go from to =
      error $ "invalid type conversion: "
        <> show from <> " to " <> show to

defaultValue :: Type -> Term a
defaultValue ty = case derefType ty of
  MaybeTy _ -> Alt 0 (Tuple [])
  ByteTy -> Byte 0
  NatTy -> Nat 0
  BooleanTy -> Alt 0 (Tuple [])
  StringTy -> String ""
  ArrayTy _ -> Array []
  RecordTy fields -> Tuple (map (defaultValue . fieldDefType) fields)
  EnumeratedTy{} -> Alt 0 (Tuple [])
  SumTy (first : _) -> Alt 0 (defaultValue (fieldDefType first))
  _ -> error $ "type doesn't have a default value: " <> show ty

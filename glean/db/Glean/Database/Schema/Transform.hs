{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE RecursiveDo #-}
module Glean.Database.Schema.Transform
  ( mkPredicateTransformation
  , defaultValue
  ) where

import Control.Monad.Cont
import Data.Coerce (Coercible, coerce)
import Data.Either (isLeft)
import Data.Either.Extra (fromEither)
import qualified Data.Map.Strict as Map
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Word (Word64)

import Glean.Angle.Types (Type_(..), FieldDef_(..), Name)
import Glean.Bytecode.Types (Ty(..))
import Glean.Database.Schema.Types
import Glean.Query.Codegen.Types (Match(..), Output, Typed(..))
import Glean.Query.Codegen (skipTrusted, buildTerm)
import qualified Glean.RTS as RTS
import Glean.RTS.Bytecode.Code
import Glean.RTS.Bytecode.Gen.Issue
import Glean.RTS.Types as RTS
import Glean.RTS.Term (Term(..), Value)
import Glean.Schema.Util (lowerMaybe, lowerEnum)
import qualified Glean.Types as Thrift

mkPredicateTransformation
  :: (Pid -> PredicateDetails)
  -> Pid
  -> Pid
  -> Maybe PredicateTransformation
mkPredicateTransformation detailsFor requestedPid availablePid
  | requestedPid == availablePid = Nothing
  | otherwise = Just PredicateTransformation
    { tRequested = pidRef requested
    , tAvailable = pidRef available
    , tTransformFactBack = fromMaybe id $ transformFact available requested
    , transformPrefix = transformPattern (key requested) (key available)
    , transformKey   = transformBytes (key available) (key requested)
    , transformValue = transformBytes (val available) (val requested)
    }
  where
      key = predicateKeyType
      val = predicateValueType
      available = detailsFor availablePid
      requested = detailsFor requestedPid

transformFact
  :: PredicateDetails
  -> PredicateDetails
  -> Maybe (Thrift.Fact -> Thrift.Fact)
transformFact from to
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
    mTransformKey = transformExpression
      (predicateKeyType from)
      (predicateKeyType to)
    mTransformValue = transformExpression
      (predicateValueType from)
      (predicateValueType to)

-- | Transform an expression into a another type.
-- Missing fields are filled with their default value.
transformExpression :: Type -> Type -> Maybe (Value -> Value)
transformExpression from to =
  case transformTerm defaultValue inner from to of
    Nothing -> Nothing
    Just f -> Just $ \ta -> f ta id
  where
    inner _ _ _ a f = f a

-- | Transform a matching pattern into a another type.
-- All variable bindings are removed.
-- Missing fields are filled with wildcards.
transformPattern
  :: Type
  -> Type
  -> Maybe
      (Term (Match () Output)
      -> (Term (Match () Output) -> Code a)
      -> Code a)
transformPattern from to = do
  f <- transformTerm defaultForType transformMatch from to
  return $ \term -> runCont (f term)
  where
    defaultForType ty = Ref (MatchWild ty)

-- | Transform a Match from one type into another for use as a prefix.
--
-- The result must not be used for binding, only for prefix matching, otherwise
-- it would need to change the type of the variables it is binding into and
-- this is incorrect.
--
-- Type compatibility is not checked. Assumes that a transformation is possible
-- and required.
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
  -- bindings are removed as we would otherwise be changing the type of the
  -- variable we are binding into in only one of its occurrences.
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

-- | Create a transformation from a term into another term of a compatible type
transformTerm
  :: forall a b m. (Coercible a b, Show a, Show b, Monad m)
  => (Type -> Term b) -- default value for type
  -> (Type -> Type -> (Term a -> m (Term b)) -> a -> m b)
  -> Type
  -> Type
  -> Maybe (Term a -> m (Term b))
transformTerm defaultForType inner src dst = go src dst
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
      return $ fix $ \recurse term ->
        case term of
          Array vs -> Array <$> traverse f vs
          Ref a -> Ref <$> inner (ArrayTy from) (ArrayTy to) recurse a
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
      else Just $ fix $ \recurse term -> case term of
        Tuple contents -> do
          contents' <- sequence
            [ case Map.lookup name transMap of
                -- 'to' field doesn't exist in 'from'
                Nothing -> return $ defaultForType ty
                Just (content, Nothing) -> id' content
                Just (content, Just (_, transform)) -> transform content
            | FieldDef name ty <- to
            ]
          return (Tuple contents')
          where
            transMap = Map.intersectionWith (,) contentsByName transformations
            contentsByName = Map.fromList $ zip (names from) contents
        Ref a -> Ref <$> inner (RecordTy from) (RecordTy to) recurse a
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
        else Just $ fix $ \recurse term -> case term of
          Alt n content -> case Map.lookup n transformationsByIx of
            -- alternative in 'from' doesn't exist in 'to'
            Nothing -> return unknown
            Just Nothing -> id' term
            Just (Just (n', transform)) -> do
              content' <- transform content
              return (Alt n' content')
          Ref a -> Ref <$> inner (SumTy from) (SumTy to) recurse a
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

-- | Transform a fact key or value contained in some binary input into another
-- compatible type.
transformBytes
  :: Type
  -> Type
  -> Maybe (Bytes -> (Register 'BinaryOutputPtr -> Code a) -> Code a)
transformBytes src dst =
  case go src dst of
    Left _ -> Nothing
    Right transform -> Just $ \bytes act ->
      output $ \out -> do
        resetOutput out
        transform out bytes
        act out
  where
  go :: Type
     -> Type
     -> Either
          (Register 'BinaryOutputPtr -> Bytes -> Code ()) -- copy as is
          (Register 'BinaryOutputPtr -> Bytes -> Code ()) -- transform and copy
  go from@(NamedTy _) to = go (derefType from) to
  go from to@(NamedTy _) = go from (derefType to)
  go ByteTy ByteTy = Left $ copy ByteTy
  go NatTy NatTy = Left $ copy NatTy
  go StringTy StringTy = Left $ copy StringTy
  go BooleanTy BooleanTy = Left $ copy BooleanTy
  go (MaybeTy from) (MaybeTy to) = go (lowerMaybe from) (lowerMaybe to)
  go (PredicateTy _) (PredicateTy pid) = Left $ copy (PredicateTy pid)
  go (EnumeratedTy from) (EnumeratedTy to) = go (lowerEnum from) (lowerEnum to)
  go (ArrayTy from) (ArrayTy to) =
    case go from to of
      Left _ -> Left $ copy (ArrayTy to)
      Right trans -> Right $ \out (Bytes start end) ->
        local $ \size -> mdo
          inputNat start end size
          jumpIf0 size finish
          loop <- label
          trans out (Bytes start end)
          decrAndJumpIfNot0 size loop
          finish <- label
          return ()
  go (SumTy from) (SumTy to)
    | sameOrder && sameTypes = Left $ copy (SumTy to)
    | otherwise = Right $ \out (Bytes start end) -> mdo
        local $ \sel -> mdo
          inputNat start end sel
          let unknown = [finish]
          select sel (lbls ++ unknown)
          -- TODO: Add tests for this unknown.
          raise "selector out of range"
        lbls <- forM alts $ \(ixTo, trans) -> mdo
          lbl <- label
          outputNatImm (fromIntegral ixTo) out
          -- the start pointer was already moved to the beginning
          -- of the content when the selector was read.
          fromEither trans out (Bytes start end)
          jump finish
          return lbl
        finish <- label
        return ()
      where
        toAlts :: Map Name (Int, Type)
        toAlts = Map.fromList
            [ (name, (ixTo, tyTo))
            | (ixTo, FieldDef name tyTo) <- zip [0..] to
            ]
        alts =
          [ case Map.lookup name toAlts of
              -- removed alternative
              Nothing -> (unknownAltIx, unknownAlt tyFrom)
              Just (ixTo, tyTo) -> (ixTo, go tyFrom tyTo)
          | FieldDef name tyFrom <- from
          ]

        unknownAltIx = length to
        unknownAlt tyFrom = Right $ \_ (Bytes start end) ->
          skipTrusted start end tyFrom

        sameIndex (ixFrom, (ixTo, _)) = ixFrom == ixTo
        sameOrder = all sameIndex (zip [0..] alts)
        sameTypes = all (isLeft . snd) alts
  go (RecordTy from) (RecordTy to)
    | sameFieldCount && sameFieldContents = Left $ copy (RecordTy to)
    | otherwise = Right walk
    where
    sameFieldCount = length from == length to
    sameFieldContents = all identical (zip from to)
    identical (FieldDef nameFrom tyFrom, FieldDef nameTo tyTo) =
      nameFrom == nameTo && isLeft (go tyFrom tyTo)

    fieldsFrom = Set.fromList $ map fieldDefName from
    fieldsTo = Set.fromList $ map fieldDefName to

    -- walk through the record being copied alongside the record being built.
    walk out bytes@(Bytes start end) = step mempty from to
      where
      step saved from' to' = case to' of
        [] ->
          -- nothing more to add, skip origin record
          skipTrusted start end (RecordTy from')
        FieldDef nameTo tyTo : restTo
          | Just (fieldBytes, savedTyFrom) <- Map.lookup nameTo saved -> do
            -- we've saved that field before, move it over now.
            fromEither (go savedTyFrom tyTo) out fieldBytes
            let saved' = Map.delete nameTo saved
            step saved' from' restTo
          | not (nameTo `Set.member` fieldsFrom) -> do
            -- target field is not in origin, use default value
            buildTerm out mempty (defaultValue tyTo)
            step saved from' restTo
          | otherwise ->
            case from' of
              [] ->
                -- if there are no more 'from' fields, all remaining 'to'
                -- fields should either be missing in 'fieldsFrom or already
                -- saved for copying.
                error $ "field unaccounted for: '" <> unpack nameTo <> "'"
              FieldDef nameFrom tyFrom : restFrom
                | not (nameFrom `Set.member` fieldsTo) -> do
                  -- origin field is not in target, skip it
                  skipTrusted start end tyFrom
                  step saved restFrom to'
                | nameTo == nameFrom -> do
                  -- matching field, move it over
                  fromEither (go tyFrom tyTo) out bytes
                  step saved restFrom restTo
                | otherwise ->
                  -- the field will be required later. Save its location
                  local $ \fieldStart -> do
                  move start fieldStart
                  skipTrusted start end tyFrom
                  let fieldBytes = Bytes fieldStart end
                      saved' = Map.insert nameFrom (fieldBytes, tyFrom) saved
                  step saved' restFrom to'
  go from to = error $ "invalid type conversion: "
    <> show from <> " to " <> show to

  -- moves the Bytes start position
  copy :: Type -> Register 'BinaryOutputPtr -> Bytes -> Code ()
  copy ty out (Bytes start end)
    | RecordTy [] <- ty = return ()
    | otherwise =
      local $ \saved_start -> do
        move start saved_start
        skipTrusted start end ty
        outputBytes saved_start start out

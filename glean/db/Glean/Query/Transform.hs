{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
module Glean.Query.Transform
  ( transformationsFor
  , transformResultsBack
  , Transformations
  , transformType
  , transformBytes
  , transformPattern
  , transformFact
  , skipTrusted
  , buildTerm
  , isWordTy
  , defaultValue
  ) where

import Control.Monad.Cont
import Data.Bifoldable
import Data.Bifunctor (bimap)
import qualified Data.ByteString as ByteString
import Data.Coerce (Coercible, coerce)
import Data.Either (isLeft)
import Data.Either.Extra (fromEither)
import Data.Function (fix)
import Data.List.Extra (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Word (Word64)

import Glean.Angle.Types (Type_(..), FieldDef_(..), Name)
import Glean.Bytecode.Types (Ty(..))
import Glean.Schema.Util (lowerEnum, lowerMaybe, showRef)
import Glean.Query.Codegen.Types
  ( Match(..)
  , Var(..)
  , Typed(..)
  , Output
  , TransformAndBind(..) )
import Glean.Database.Schema.Types
import qualified Glean.RTS as RTS
import Glean.RTS.Bytecode.Code
import Glean.RTS.Bytecode.Gen.Issue
import Glean.RTS.Foreign.Query (QueryResults(..))
import Glean.RTS.Types as RTS
import Glean.RTS.Term (Term(..), Value)
import qualified Glean.Types as Thrift

-- | A map of predicate transformations applied to a query
-- Keyed by the predicate available in the database
newtype Transformations = Transformations (IntMap PredicateTransformation)
  deriving newtype (Semigroup, Monoid)

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

    -- predicates available are mapped to the predicates requested
    mappings :: Map Pid (Set Pid)
    mappings =
      Map.fromListWith (<>)
        [ (to, Set.singleton from)
        | from <- withDeps
        , let to = case lookupTransformation from tmap of
                Nothing -> from
                Just e -> pid $ tAvailable e
        ]

    repeated :: [(Pid, Set Pid)]
    repeated = Map.toList $ Map.filter ((> 1) . Set.size) mappings

    transformations :: Transformations
    transformations = Transformations $ IntMap.fromList
      [ (to, evolution)
      | evolution <- mapMaybe (`lookupTransformation` tmap) withDeps
      , let to = fromIntegral $ fromPid $ pid  $ tAvailable evolution
      ]

    showRepeated :: (Pid, Set Pid) -> Text
    showRepeated (to, froms) =
      showPid to <> " evolves "
      <> Text.intercalate " and " (showPid <$> Set.toList froms)
      where showPid = showRef . predicateRef . detailsFor

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

lookupTransformation
  :: Pid
  -> IntMap PredicateTransformation
  -> Maybe PredicateTransformation
lookupTransformation pid tmap =
  IntMap.lookup (fromIntegral $ fromPid pid) tmap

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

-- ===========================================================================
-- Byte code manipulation
-- ===========================================================================

-- | Generate code to skip over a value of the given type in the input
--
skipTrusted
  :: Register 'DataPtr
  -> Register 'DataPtr
  -> Type
  -> Code ()
skipTrusted input inputend ty = skip (repType ty)
  where
  skip ty = case ty of
    ByteRep -> do size <- constant 1; inputBytes input inputend size
    NatRep -> inputSkipNat input inputend
    ArrayRep eltTy ->
      local $ \size -> do
        inputNat input inputend size
        case eltTy of
          ByteRep -> inputBytes input inputend size
          _ -> mdo
            jumpIf0 size end
            loop2 <- label
            skip eltTy
            decrAndJumpIfNot0 size loop2
            end <- label
            return ()
    TupleRep tys -> mapM_ skip tys
    SumRep tys -> mdo
      local $ \sel -> do
        inputNat input inputend sel
        let unknown = [end]
        select sel (alts ++ unknown)
      raise "selector out of range"
      alts <- forM tys $ \ty -> do
        -- TODO: for (Tuple []) we don't need to generate any bytecode
        alt <- label
        skip ty
        jump end
        return alt
      end <- label
      return ()
    StringRep -> inputSkipTrustedString input inputend
    PredicateRep _ -> inputSkipNat input inputend


-- | Serialize a term into the given output register.
buildTerm
  :: Register 'BinaryOutputPtr
  -> Vector (Register 'Word)
  -> Term (Match () Var)
  -> Code ()
buildTerm output vars term = go term
  where
  go term = case term of
    Byte b -> outputByteImm (fromIntegral b) output
    Nat n -> outputNatImm n output
    String s ->
      local $ \ptr end -> do
        -- NOTE: We assume that the string has been validated during parsing.
        loadLiteral (RTS.mangleString s) ptr end
        outputBytes ptr end output
    Array vs -> do
      outputNatImm (fromIntegral (length vs)) output
      mapM_ go vs
    Tuple fields -> mapM_ go fields
    Alt n term -> do outputNatImm n output; go term
    Ref (MatchFid f) -> outputNatImm (fromIntegral (fromFid f)) output
    Ref (MatchPrefix str rest) -> do
      local $ \ptr end -> do
        let
          mangled = RTS.fromValue (String str)
          withoutTerminator =
            ByteString.take (ByteString.length mangled - 2) mangled
        loadLiteral withoutTerminator ptr end
        outputBytes ptr end output
      go rest
    Ref (MatchVar (Var ty var _))
      | isWordTy ty -> outputNat (vars ! var) output
      | otherwise ->
        local $ \ptr end -> do
          getOutput (castRegister (vars ! var)) ptr end
          outputBytes ptr end output
    other -> error $ "buildTerm: " <> show other

-- | A 'ResultTerm' is represented in two ways depending on the type
-- of the value being returned:
--
-- * 'PredicateTy' and 'NatTy' results are stored directly in a 'Register Word'
-- * Other types are built in a 'binary::Output' and are represented by a
--   'Register BinaryOutputPtr'.
--
-- 'isWordTy' returns 'True' for the first kind.
--
isWordTy :: Type -> Bool
isWordTy = isWordRep . repType
  where
  isWordRep PredicateRep{} = True
  isWordRep ByteRep = True
  isWordRep NatRep = True
  isWordRep _ = False

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
  case transformTerm inner defaultValue from to of
    Nothing -> Nothing
    Just f -> Just $ \ta -> f discard ta id
  where
    inner _ _ _ _ a f = f a
    discard _ _ = return ()

type Matcher = Match TransformAndBind Output

-- | Transform a matching pattern into a another type.
-- All variable bindings are removed.
-- Missing fields are filled with wildcards.
transformPattern
  :: forall a. Type
  -> Type
  -> Maybe
        (  (Type -> Term Matcher -> Code ())
        -> Term Matcher
        -> (Term Matcher -> Code a)
        -> Code a
        )
transformPattern from to = do
  f <- transformTerm transformMatch defaultForType from to
  return $ \discard term ->
    let
        discard' :: Type -> Term Matcher -> Cont (Code a) ()
        discard' a b = cont $ \r -> do
          () <- discard a b
          r ()
    in
    runCont (f discard' term)
  where
    defaultForType ty = Ref (MatchWild ty)

-- | Transform a Match which matches against a value of 'from' type into one
-- that matches against a value of 'to' type.
--
-- NB. Type compatibility is not checked. Assumes that a transformation is
-- possible and required.
transformMatch
  :: (Type -> Matcher -> Cont (Code x) ())  -- ^ handle discarded record fields
  -> Type
  -> Type
  -> (Term Matcher -> Cont (Code x) (Term Matcher))
  -> Matcher
  -> Cont (Code x) Matcher
transformMatch discard from to overTerm match = case match of
  MatchWild _ -> return $ MatchWild to
  MatchNever _ -> return $ MatchNever to
  MatchFid fid -> return $ MatchFid fid
  -- Convert value from 'to' type into 'from' before binding.
  -- If we get to this case it means that this conversion is required,
  MatchBind out -> return $ MatchExt $ TransformAndBind to out
  MatchVar (Typed _ var) ->
    case transformBytes' discard' from to of
      Nothing -> return $ MatchVar $ Typed to var
      Just transform ->
        cont $ \r ->
          output $ \result -> do
          local $ \start end -> do
            getOutput var start end
            resetOutput result
            transform (Bytes start end) result
          r $ MatchVar $ Typed to result
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
  MatchExt x -> return $ MatchExt x
  where
    discard' :: Type -> Bytes -> Code ()
    discard' ty (Bytes start end) = do
      output $ \out -> do
        resetOutput out
        outputBytes start end out
        run $ discard ty (MatchVar $ Typed ty out)
    run :: forall x. Cont (Code x) () -> Code ()
    run m = void $ runCont m (\() -> return undefined)

type TransformTerm m a b
  = (Type -> Term a -> m ())   -- ^ discard term
  -> Term a                    -- ^ source term
  -> m (Term b)

-- | Create a transformation from a term into another term of a compatible type.
--
-- This will be used for transforming patterns and expressions. In each of
-- these two cases we will handle default values and discarded records fields
-- differently so we take those handling functions as input.
transformTerm
  :: forall a b m. (Coercible a b, Show a, Show b, Monad m)
  => ( (Type -> a -> m ())       --  discard inner value
    -> Type                      --  from type
    -> Type                      --  to type
    -> (Term a -> m (Term b))    --  handle inner terms
    -> a
    -> m b)
  -> (Type -> Term b)            -- ^ default value for type
  -> Type                        -- ^ from type
  -> Type                        -- ^ to type
  -> Maybe (TransformTerm m a b)
transformTerm inner defaultForType src dst = go src dst
  where
    -- Types are the same. No transformation is required
    id' :: Term a -> m (Term b)
    id' term = return (coerce term)

    names = map fieldDefName

    -- one entry for each common field
    transformationsFor
      :: [RTS.FieldDef]
      -> [RTS.FieldDef]
      -> Map Text (Maybe (Word64, TransformTerm m a b))
    transformationsFor from to =
      Map.intersectionWith trans fromFields toFields
      where
        trans (ixFrom, defFrom) (ixTo, defTo) =
          case go defFrom defTo of
            -- fields are identical
            Nothing | ixTo == ixFrom -> Nothing
            -- field order changed
            Nothing -> Just (ixTo, const id')
            -- field content changed
            Just f -> Just (ixTo, f)

        fromFields :: Map Text (Word64, Type)
        fromFields = Map.fromList $ flip map (zip from [0..])
          $ \(FieldDef name def, ix) -> (name, (ix, def))

        toFields :: Map Text (Word64, Type)
        toFields = Map.fromList $ flip map (zip to [0..])
          $ \(FieldDef name def, ix) -> (name, (ix, def))

    go :: Type -> Type -> Maybe (TransformTerm m a b)
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
      return $ fix $ \recurse discard term ->
        case term of
          Array vs -> Array <$> traverse (f discard) vs
          Ref a -> Ref <$> inner
            (\ty val -> discard ty (Ref val))
            (ArrayTy from)
            (ArrayTy to)
            (recurse discard)
            a
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
      else Just $ fix $ \recurse discard term -> case term of
        Tuple contents -> do
          contents' <- sequence
            [ case Map.lookup name transMap of
                -- 'to' field doesn't exist in 'from'
                Nothing -> return $ defaultForType ty
                Just (content, Nothing) -> id' content
                Just (content, Just (_, transform)) -> transform discard content
            | FieldDef name ty <- to
            ]

          sequence_
            [ discard tyFrom term
            | (FieldDef nameFrom tyFrom, term) <- zip from contents
            , nameFrom `notElem` names to
            ]

          return (Tuple contents')
          where
            transMap = Map.intersectionWith (,) contentsByName transformations
            contentsByName = Map.fromList $ zip (names from) contents
        Ref a -> Ref <$> inner
          (\ty -> discard ty . Ref)
          (RecordTy from)
          (RecordTy to)
          (recurse discard)
          a
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
        else Just $ fix $ \recurse discard term -> case term of
          Alt n content -> case Map.lookup n transformationsByIx of
            -- alternative in 'from' doesn't exist in 'to'
            Nothing -> return unknown
            Just Nothing -> id' term
            Just (Just (n', transform)) -> do
              content' <- transform discard content
              return (Alt n' content')
          Ref a -> Ref <$> inner
            (\ty -> discard ty . Ref)
            (SumTy from)
            (SumTy to)
            (recurse discard)
            a
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
--
-- The transformation function will always leave the start pointer of `Bytes` at
-- the end of the transformed input.
transformBytes
  :: Type
  -> Type
  -> Maybe (Bytes -> Register 'BinaryOutputPtr -> Code ())
transformBytes = transformBytes' ignoreDiscarded
  where
    ignoreDiscarded _ _ = return ()

transformBytes'
  :: (Type -> Bytes -> Code ()) -- handle discarded record fields
  -> Type
  -> Type
  -> Maybe (Bytes -> Register 'BinaryOutputPtr -> Code ())
transformBytes' discard src dst =
  case go src dst of
    Left _ -> Nothing
    Right transform -> Just $ \bytes out -> transform out bytes
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
                  -- origin field is not in target, discard it
                  local $ \saved_start -> do
                    move start saved_start
                    skipTrusted start end tyFrom
                    discard tyFrom (Bytes saved_start start)
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

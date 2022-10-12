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
  ( transformQuery
  , transformType
  , transformTypecheckedQuery
  , transformationsFor
  , transformResultsBack
  , Transformations
  , fromTransformations
  , toTransformations
  , renumberVars

  -- codegen
  , transformBytes
  , transformPattern
  , transformFact
  , skipTrusted
  , buildTerm
  , isWordTy
  , defaultValue
  ) where

import Control.Monad.Cont
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.Bifoldable
import qualified Data.ByteString as ByteString
import Data.Coerce (Coercible, coerce)
import Data.Either (isLeft)
import Data.Either.Extra (fromEither)
import Data.Function (fix)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List.Extra (nubOrd, elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, isJust)
import qualified Data.IntMap.Strict as IntMap
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Word (Word64)

import qualified Glean.Angle.Types as Type
import Glean.Angle.Types (PredicateId, Type_(..), FieldDef_(..), Name)
import Glean.Bytecode.Types (Ty(..))
import Glean.Schema.Util (showRef, lowerEnum, lowerMaybe, lowerBool)
import Glean.Query.Codegen.Types
  (Match(..), Var(..), QueryWithInfo(..), Typed(..), Output)
import Glean.Query.Typecheck.Types
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

type Matcher = Match () Output

-- | Transform a matching pattern into a another type.
-- All variable bindings are removed.
-- Missing fields are filled with wildcards.
transformPattern
  :: Type
  -> Type
  -> Maybe (Term Matcher -> (Term Matcher -> Code a) -> Code a)
transformPattern from to = do
  f <- transformTerm defaultForType transformMatch from to
  return $ \term -> runCont (f term)
  where
    defaultForType ty = Ref (MatchWild ty)

-- | Transform a Match which matches against a value of 'from' type into one
-- that matches against a value of 'to' type.
--
-- NB. Type compatibility is not checked. Assumes that a transformation is
-- possible and required.
transformMatch
  :: Type
  -> Type
  -> (Term Matcher -> Cont (Code x) (Term Matcher))
  -> Matcher
  -> Cont (Code x) Matcher
transformMatch from to overTerm match = case match of
  MatchWild _ -> return $ MatchWild to
  MatchNever _ -> return $ MatchNever to
  MatchFid fid -> return $ MatchFid fid
  -- Convert value from 'to' type into 'from' before binding.
  -- If we get to this case it means that this conversion is required,
  MatchBind _ -> return $ MatchWild to
  MatchVar (Typed _ var) ->
    case transformBytes from to of
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
  -> Maybe (Bytes -> Register 'BinaryOutputPtr -> Code ())
transformBytes src dst =
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

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema
  ( DbSchema(..)
  , PredicateDetails(..)
  , TypeDetails(..)
  , schemaSize, schemaPredicates
  , fromSchemaInfo, toSchemaInfo
  , newDbSchema
  , newMergedDbSchema
  , Override(..)
  , lookupPid, lookupPredicate, lookupPredicateRef, lookupTypeRef
  , validateNewSchema
  , DbContent
  , readOnlyContent
  , readWriteContent
  , thinSchemaInfo
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Graph
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Lazy as Lazy.HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List (foldl', find)
import Data.List.Extra (firstJust)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Tuple (swap)

import Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Schema.Types
import Glean.Database.Schema.Transform (mkPredicateTransformation)
import qualified Glean.Database.Schema.Transform as Transform
import Glean.RTS.Traverse
import Glean.RTS.Typecheck
import Glean.RTS.Types as RTS

import Glean.Angle.Types as Schema
import Glean.Schema.Resolve hiding (schemaPredicates)
import Glean.Schema.Util (showSourceRef, showPredicateRef)
import Glean.Query.Codegen (QueryWithInfo(..))
import Glean.Query.Typecheck
import qualified Glean.Types as Thrift
import Glean.Types (PredicateStats(..))

-- | Used to decide whether to activate 'derive default' definitions and
-- 'evolves' directives.
-- 'derive default' takes effect for a read-only DB when there are no
-- facts of a predicate.
-- 'evolves' directives take effect for a read-only DB when there are no facts
-- of any predicate in a schema evolved by another schema.
data DbContent
  = DbWritable
  | DbReadOnly (HashMap Pid PredicateStats)

readOnlyContent :: [(Pid, PredicateStats)] -> DbContent
readOnlyContent = DbReadOnly . HashMap.fromList

readWriteContent :: DbContent
readWriteContent = DbWritable


schemaSize :: DbSchema -> Int
schemaSize = HashMap.size . predicatesByRef

schemaPredicates :: DbSchema -> [PredicateDetails]
schemaPredicates = HashMap.elems . predicatesByRef


fromSchemaInfo :: Thrift.SchemaInfo -> DbContent -> IO DbSchema
fromSchemaInfo Thrift.SchemaInfo{..} dbContent =
  mkDbSchemaFromSource
    (mkGetPids schemaInfo_predicateIds)
    dbContent
    schemaInfo_schema

mkGetPids :: Map Thrift.Id PredicateRef -> [PredicateRef] -> [Pid]
mkGetPids m refs =
  let by_ref = HashMap.fromList
        $ map (second Pid . swap)
        $ Map.toList m
  in
  map
    (\ref -> HashMap.lookupDefault
      (Pid Thrift.iNVALID_ID)
      ref
      by_ref)
    refs

toSchemaInfo :: DbSchema -> Thrift.SchemaInfo
toSchemaInfo DbSchema{..} = Thrift.SchemaInfo
  { schemaInfo_schema =
      Text.encodeUtf8 $ renderStrict $ layoutCompact $ pretty schemaSource
  , schemaInfo_predicateIds = Map.fromList
      [(fromPid $ predicatePid p, predicateRef p)
        | p <- HashMap.elems predicatesByRef]
  }

-- | Produce a "thinned" SchemaInfo, containing only the schemas
-- necessary to describe the facts in the DB.  The idea is that
--
-- 1. It's less brittle than storing all current schemas, because
--    there's less chance for conflict when we open the DB and merge
--    its schema with the global schema later.
--
-- 2. We don't store irrelevant information.
--
thinSchemaInfo
  :: DbSchema
  -> [(Pid, PredicateStats)]
  -> (Thrift.SchemaInfo, [Name])
thinSchemaInfo DbSchema{..} predicateStats =
  (schemaInfo, map schemaName allNeededSchemas)
  where
  schemaInfo = Thrift.SchemaInfo
    { schemaInfo_schema =
        Text.encodeUtf8 $ renderStrict $ layoutCompact $ pretty $ schemaSource
          { srcSchemas = allNeededSchemas
          -- schema evolvolutions only work when the older schema contains no
          -- facts. Thinning will remove the older factless schema and the
          -- evolution will have no meaning. Therefore we always drop schema
          -- evolutions.
          , srcEvolves = [] }
    , schemaInfo_predicateIds = Map.fromList
        [ (fromPid $ predicatePid p, predicateRef p)
        | p <- HashMap.elems predicatesByRef
        {-
          TODO: we should also filter out Pids of predicates that were
          removed from the schema:

          , predicateRef p `HashSet.member` nonEmptyPredicates

          but the problem is that those Pids will end up being
          reassigned, which can cause problems for suspending/resuming
          queries. The proper fix for this will be to use PredicateRefs
          not Pids in suspended queries, but this workaround will work
          in most cases for now.
        -}
        ]
    }

  -- Predicates that have some facts
  nonEmptyPredicates :: HashSet PredicateRef
  nonEmptyPredicates = HashSet.fromList
    [ predicateRef pred
    | (pid, stats) <- predicateStats
    , predicateStats_count stats /= 0
    , Just pred <- [IntMap.lookup (fromIntegral $ fromPid pid) predicatesById]
    ]

  -- Names of schemas that define one or more non-empty predicates
  neededSchemas :: [Name]
  neededSchemas =
    [ showSourceRef (SourceRef resolvedSchemaName (Just resolvedSchemaVersion))
    | ResolvedSchema{..} <- schemaResolved
    , any (`HashSet.member` nonEmptyPredicates)
        (HashMap.keys resolvedSchemaPredicates)
    ]

  -- Maps names of schemas to their source and dependencies
  sourceSchemaMap :: HashMap Name (SourceSchema, [Name])
  sourceSchemaMap = HashMap.fromList
    [ (schemaName schema, (schema, deps))
    | schema <- srcSchemas schemaSource
    , let deps =
            schemaInherits schema ++
            [ n | SourceImport n <- schemaDecls schema ]
    ]

  -- All source schemas that either define a non-empty predicate, or
  -- are a (transitive) dependency of such a schema.
  allNeededSchemas = HashMap.elems (close neededSchemas HashMap.empty)
    where
    close [] r = r
    close (n:ns) r
      | n `HashMap.member` r = close ns r
      | otherwise =
        case HashMap.lookup n sourceSchemaMap of
          Nothing -> close ns r -- shouldn't happen really
          Just (schema, deps) ->
            close (deps ++ ns) (HashMap.insert n schema r)


data Override
  = TakeOld
  | TakeNew
  | MustBeEqual

newMergedDbSchema
  :: Thrift.SchemaInfo                  -- ^ schema from a DB
  -> SourceSchemas                      -- ^ current schema source
  -> Schemas                            -- ^ current schema
  -> Override                           -- ^ override DB schema?
  -> DbContent
  -> IO DbSchema
newMergedDbSchema Thrift.SchemaInfo{..} source current override dbContent = do
  let
    -- predicates in the schema from the DB
    oldPredPids = HashMap.fromList
       $ map (second Pid . swap)
       $ Map.toList schemaInfo_predicateIds

    maxOldPid = maximum $ lowestPid : map Pid (Map.keys schemaInfo_predicateIds)

    lookupPids !newpid (ref : rest)
      | Just pid <- HashMap.lookup ref oldPredPids =
        pid : lookupPids newpid rest
      | otherwise = newpid : lookupPids (succ newpid) rest
    lookupPids _ [] = []

  (_fromDBSource, fromDBResolved) <-
    case parseAndResolveSchema schemaInfo_schema of
      Left msg -> throwIO $ ErrorCall msg
      Right resolved -> return resolved

  -- For the "source", we'll use the current schema source. It doesn't
  -- reflect what's in the DB, but it reflects what you can query for.
  mkDbSchema override  (lookupPids (succ maxOldPid)) dbContent source
    fromDBResolved
    (schemasResolved current)

-- | Build a DbSchema from parsed/resolved Schemas. Note that we still need
-- the original source for the schema for 'toSchemaInfo' (otherwise we would
-- need to pretty-print the resolved Schemas back into source.
newDbSchema
  :: SourceSchemas
  -> Schemas
  -> DbContent
  -> IO DbSchema
newDbSchema str schemas dbContent =
  mkDbSchema TakeOld (zipWith const [lowestPid ..]) dbContent str schemas []

inventory :: [PredicateDetails] -> Inventory
inventory ps = Inventory.new
  [ Inventory.CompiledPredicate
      { compiledPid = predicatePid
      , compiledRef = predicateRef
      , compiledTypecheck = predicateTypecheck
      , compiledTraversal = predicateTraversal
      }
    | PredicateDetails{..} <- ps ]

-- |
-- Make a DbSchema for an existing database, using the predicates
-- stored in the database.  Anything in the 'Schemas' but not
-- in the database is dropped from the 'DbSchema'
mkDbSchemaFromSource
  :: ([PredicateRef] -> [Pid])
  -> DbContent
  -> ByteString
  -> IO DbSchema
mkDbSchemaFromSource getPids dbContent source = do
  case parseAndResolveSchema source of
    Left str -> throwIO $ ErrorCall str
    Right (srcSchemas, resolved) ->
      mkDbSchema TakeOld getPids dbContent srcSchemas resolved []

mkDbSchema
  :: Override   -- later schemas override earlier ones?
  -> ([PredicateRef] -> [Pid])
  -> DbContent
  -> Schema.SourceSchemas
  -> Schemas
  -> [ResolvedSchema]
  -> IO DbSchema
mkDbSchema override getPids dbContent source base addition = do
  let resolved = schemasResolved base <> addition
  let refs = HashMap.keys
        $ HashMap.unions
        $ map resolvedSchemaPredicates
        $ resolved
  let pids = getPids refs
  let refToPid = HashMap.fromList
        $ filter (\(_,pid) -> pid /= Pid Thrift.iNVALID_ID)
        $ zip refs pids

  -- typecheck the schemas in dependency order, building up an
  -- environment containing all the known predicates and types.
  envStored <- foldM
    (typecheckSchema override refToPid dbContent True)
    emptyTcEnv
    (schemasResolved base)

  -- Now typecheck the additional schemas we want to merge in. Clashes
  -- are resolved according to the value of override, which under
  -- normal circumstances (TakeOld) means the schema stored in the DB
  -- takes precedence.
  env <- foldM
    (typecheckSchema override refToPid dbContent False)
    envStored
    addition

  -- Check the invariant that stored predicates do not make use of
  -- negation or refer to derived predicates that make use of it.
  -- See Note [Negation in stored predicates]
  let
    usingNegation = usesOfNegation (tcEnvPredicates env)
    isStored = \case
      NoDeriving -> True
      Derive when _ -> case when of
        DeriveOnDemand -> False
        DerivedAndStored -> True
        DeriveIfEmpty -> True
    useOfNegation ref = HashMap.lookup ref usingNegation

  forM_ (tcEnvPredicates env) $ \PredicateDetails{..} ->
    when (isStored predicateDeriving) $
      case useOfNegation predicateRef of
        Nothing -> return ()
        Just use ->
          let feature = case use of
                PatternNegation -> "negation"
                IfStatement -> "if-statements"
          in
          throwIO $ Thrift.Exception
          $ "use of " <> feature  <> " is not allowed in a stored predicate: "
          <> showPredicateRef predicateRef

  let predicates = HashMap.elems (tcEnvPredicates env)

      byId = IntMap.fromList
        [ (fromIntegral (fromPid predicatePid), deets)
        | deets@PredicateDetails{..} <- predicates ]

      byRef = HashMap.fromList
        [ (predicateRef, deets)
        | deets@PredicateDetails{..} <- predicates ]

      maxPid = maybe lowestPid (Pid . fromIntegral . fst)
        (IntMap.lookupMax byId)

      mkPredicatesByName predicates = HashMap.fromListWith latest
        [ (predicateRef_name ref, deets)
        | ref <- predicates
        , Just deets <- [HashMap.lookup ref byRef] ]
        where
        latest a b
          | predicateRef_version (predicateRef a) >
            predicateRef_version (predicateRef b) = a
          | otherwise = b

      mkSchemaTypesByName types = HashMap.fromListWith latest
        [ (typeRef_name ref, deets)
        | ref <- types
        , Just deets@TypeDetails{} <- [HashMap.lookup ref (tcEnvTypes env)] ]
        where
        latest a b
          | typeRef_version (typeRef a) >
            typeRef_version (typeRef b) = a
          | otherwise = b

      resolvedAlls = filter ((== "all") . resolvedSchemaName) resolved

      -- When a query mentions an unversioned predicate, we use a default
      -- version of the predicate. The default version is whatever is
      -- exported by the highest version of the distinguished schema "all"
      latestVer = case resolvedAlls of
        [] -> Nothing
        xs -> Just $ maximum $ map resolvedSchemaVersion xs

      predicatesByName = IntMap.fromList
        [ (fromIntegral resolvedSchemaVersion,
             mkPredicatesByName (
               HashMap.keys resolvedSchemaReExportedPredicates))
        | ResolvedSchema{..} <- resolvedAlls
        ]

      schemaTypesByName = IntMap.fromList
        [ (fromIntegral resolvedSchemaVersion,
            mkSchemaTypesByName (
              HashMap.keys resolvedSchemaReExportedTypes))
        | ResolvedSchema{..} <- resolvedAlls
        ]

      dependencies = fmap f byId
        where
          f = Set.fromList
            . Transform.transitiveDeps (detailsFor byId)
            . predicatePid

  return $ DbSchema
    { predicatesByRef = byRef
    , predicatesByName = predicatesByName
    , predicatesById = byId
    , predicatesTransformations =
        mkTransformations dbContent override byId byRef resolved
    , predicatesDeps = dependencies
    , schemaTypesByRef = tcEnvTypes env
    , schemaTypesByName = schemaTypesByName
    , schemaInventory = inventory predicates
    , schemaResolved = resolved
    , schemaSource = source
    , schemaMaxPid = maxPid
    , schemaLatestVersion = fromMaybe 0 latestVer
    }

detailsFor :: IntMap PredicateDetails -> Pid -> PredicateDetails
detailsFor byId pid =
  case IntMap.lookup (fromIntegral $ fromPid pid) byId of
    Just details -> details
    Nothing -> error $ "unknown pid " <> show pid

mkTransformations
  :: DbContent
  -> Override
  -> IntMap PredicateDetails
  -> HashMap PredicateRef PredicateDetails
  -> [ResolvedSchema]
  -> IntMap PredicateTransformation
mkTransformations DbWritable _ _ _ _ = mempty
mkTransformations (DbReadOnly stats) override byId byRef resolved =
  IntMap.fromList
    [ (fromIntegral (fromPid old), evolution)
    | (old, new) <- Map.toList $
        transformedPredicates stats override byRef resolved
    , Just evolution <- [mkTransformation old new]
    ]
  where
    mkTransformation old new =
      mkPredicateTransformation (detailsFor byId) old new

-- ^ Create a map of which should be transformed into which
transformedPredicates
  :: HashMap Pid PredicateStats
  -> Override
  -> HashMap PredicateRef PredicateDetails
  -> [ResolvedSchema]
  -> Map Pid Pid -- ^ when key pred is requested, get value pred from db
transformedPredicates stats override byRef resolved =
  HashMap.foldMapWithKey mapPredicates schemaTransformations
  where
    schemaTransformations :: HashMap SchemaRef SchemaRef
    schemaTransformations = HashMap.fromList
      [ (requested, available)
      | requested <- map schemaRef resolved
      , Just available <- [transformationTarget requested]
      ]

    -- map each schema to the schema its queries should be transformed into
    transformationTarget :: SchemaRef -> Maybe SchemaRef
    transformationTarget src
      | hasFactsInDb src = Nothing
        -- Pick closest related schema with facts in the db.
        -- If a schema has only derived facts there is no need to
        -- transform it since the original version will still work.
      | otherwise =
          find hasFactsInDb newerSchemas <|>
          find hasFactsInDb olderSchemas
          where
            -- given an 'a' returns [b,c...] such that 'b evolves a', 'c
            -- evolves b', ...
            newerSchemas = close directEvolves src
            -- given an 'd' returns [c,b...] such that 'd evolves c', 'c
            -- evolves b', ...
            olderSchemas = close reverseDirectEvolves src

    close :: (Hashable a, Eq a) => HashMap a a -> a -> [a]
    close m x = case HashMap.lookup x m of
        Just y -> y : close m y
        Nothing -> []

    -- as specified by a `schema x evolves y` line
    directEvolves :: HashMap SchemaRef SchemaRef
    directEvolves = either (error . show) id (resolveEvolves resolved)

    reverseDirectEvolves :: HashMap SchemaRef SchemaRef
    reverseDirectEvolves =
      HashMap.fromList $ map swap $ HashMap.toList directEvolves

    hasFactsInDb :: SchemaRef -> Bool
    hasFactsInDb sref = fromMaybe False $ do
      schema <- find ((sref ==) . schemaRef) resolved
      let definedPreds = map predicatePid
            $ mapMaybe (`HashMap.lookup` byRef)
            $ HashMap.keys
            $ resolvedSchemaPredicates schema
      return $ any predicateHasFactsInDb definedPreds

    predicateHasFactsInDb :: Pid -> Bool
    predicateHasFactsInDb id = factCount > 0
      where factCount = maybe 0 predicateStats_count (HashMap.lookup id stats)

    pid (PidRef x _) = x

    mapPredicates
      :: SchemaRef
      -> SchemaRef
      -> Map Pid Pid
    mapPredicates requestedRef availableRef = Map.fromList
      [ (pid requested, pid available)
      | (name, requested) <- Map.toList (exports requestedRef)
      -- we don't transform derived predicates, only their derivation query.
      , not (isDerivedPred requested)
      , Just available <- return $ Map.lookup name (exports availableRef)
      ]

    isDerivedPred :: PidRef -> Bool
    isDerivedPred (PidRef pid ref) = case predicateDeriving details of
      NoDeriving -> False
      Derive when _ -> case when of
        DeriveOnDemand -> True
        DerivedAndStored -> False
        DeriveIfEmpty -> not $ predicateHasFactsInDb pid
      where
        details = case HashMap.lookup ref byRef of
          Nothing -> error $ "unknown predicate " <> show ref
          Just details -> details

    exports :: SchemaRef -> Map Name PidRef
    exports sref = HashMap.lookupDefault mempty sref exportsBySchemaRef

    exportsBySchemaRef :: HashMap SchemaRef (Map Name PidRef)
    exportsBySchemaRef = HashMap.fromListWith choose
      [ (schemaRef schema, exportsResolved schema )
      | schema <- resolved ]
      where
        choose new old = case override of
          TakeOld -> old
          _ -> new

    exportsResolved :: ResolvedSchema -> Map Name PidRef
    exportsResolved schema = Map.fromList
      [ (predicateRef_name ref, PidRef (predicatePid details) ref)
      | ref <- HashMap.keys $
          resolvedSchemaPredicates schema
          <> resolvedSchemaReExportedPredicates schema
      , Just details <- return $ HashMap.lookup ref byRef
      ]

{- Note [Negation in stored predicates]

Facts derived based on the absence of other facts could be invalidated when new
facts are added to an incremental database.

The work required to identify which fact to invalidate is close to that of
re-deriving those predicates all over again.

To keep the overhead on incremental databases as low as possible, stored
derived predicates are not allowed to use negation.
-}

typecheckSchema
  :: Override  -- ^ new schema overrides existing types/predicates?
  -> HashMap PredicateRef Pid
  -> DbContent
  -> Bool  -- ^ True: this schema is stored in the DB
  -> TcEnv
  -> ResolvedSchema
  -> IO TcEnv
typecheckSchema override refToPid dbContent stored
    TcEnv{..} ResolvedSchema{..} = do

  predicates <- fmap catMaybes $
    forM (HashMap.toList resolvedSchemaPredicates) $ \(ref,def) ->
      case
          ( HashMap.lookup ref refToPid
          , rtsType $ predicateDefKeyType def
          , rtsType $ predicateDefValueType def ) of
        (Just pid, Just keyType, Just valueType) -> do
          typecheck <- checkSignature keyType valueType
          traversal <- genTraversal keyType valueType
          return $ Just PredicateDetails
            { predicatePid = pid
            , predicateRef = ref
            , predicateSchema = def
            , predicateKeyType = keyType
            , predicateValueType = valueType
            , predicateTraversal = traversal
            , predicateTypecheck = typecheck
            , predicateDeriving = NoDeriving
            , predicateInStoredSchema = stored
            }
        _ -> return Nothing
  let
    insert
      :: (Eq k, Hashable k)
      => (v -> v -> IO ())
      -> HashMap k v
      -> (k,v)
      -> IO (HashMap k v)
    insert cmp = case override of
      TakeOld -> takeOld
      TakeNew -> takeNew
      MustBeEqual -> \m (k,v) -> if
        | Just oldV <- HashMap.lookup k m -> do cmp oldV v; return m
        | otherwise -> takeNew m (k,v)

    takeOld m (k,v) = return $ HashMap.insertWith (\_ old -> old) k v m
    takeNew m (k,v) = return $ HashMap.insert k v m

    mergePredicates = insert comparePredicates
    mergeTypes = insert compareTypes

    comparePredicates a b =
      when (not (predicateKeyType a `eqType` predicateKeyType b)
        || not (predicateValueType a `eqType` predicateValueType b)) $
        throwIO $ Thrift.Exception $ "predicate " <>
          Text.pack (show (pretty (predicateRef a))) <> " has changed"

    compareTypes a b =
      when (not (typeType a `eqType` typeType b)) $
        throwIO $ Thrift.Exception $ "type " <>
          Text.pack (show (pretty (typeRef a))) <> " has changed"

  preds <-
    foldM mergePredicates tcEnvPredicates
      [ (predicateRef, details)
      | details@PredicateDetails{..} <- predicates ]

  types <-
    foldM mergeTypes tcEnvTypes
      [ (ref, TypeDetails ref ty)
      | (ref, Just ty) <- HashMap.toList typedefs ]

  let
    -- Build the environment in which we typecheck predicates from
    -- this schema.
    env = TcEnv
      { tcEnvPredicates = preds
      , tcEnvTypes = types
      }

    tcDeriving pred info = runExcept $ do
      typecheckDeriving env resolvedSchemaAngleVersion
          (UseScope resolvedSchemaScope rtsType) pred info

  -- Typecheck all the derivations, which can be for predicates in
  -- this schema or for any schema it inherits from.
  typecheckedPredicates <-
    fmap catMaybes $
    forM (HashMap.toList resolvedSchemaDeriving) $ \(ref, info) ->
      case HashMap.lookup ref preds of
        Nothing -> return Nothing
        Just pred -> case tcDeriving pred info of
          Left err -> throwIO $ ErrorCall $ Text.unpack err
          Right tc ->
            return (Just (predicateRef pred, pred { predicateDeriving = tc }))

  let
    -- we want to attach a deriving to an existing predicate if:
    --   1. overriding is on, or
    --   2. if 'derive default', then iff this is a read-only DB and
    --      the predicate is empty
    --   3. both the predicate and the derivation come from a stored schema,
    --      or both come from a non-stored schema
    --
    -- That is, we don't attach a deriving to a predicate in the
    -- stored schema if the deriving wasn't present when the DB was
    -- created.
    addDeriving details
      | Derive DeriveIfEmpty _ <- predicateDeriving details =
        case dbContent of
          DbWritable -> False
          DbReadOnly stats ->
            maybe True ((== 0) . predicateStats_count)
              (HashMap.lookup (predicatePid details) stats)
      | TakeNew <- override = True
      | predicateInStoredSchema details == stored = True
      | otherwise = False

    mergeDeriving m item@(_, details)
      | addDeriving details = takeNew m item
      | otherwise = takeOld m item

  -- Build the final environment.
  finalPreds <- foldM mergeDeriving preds typecheckedPredicates

  -- Check the invariant that stored predicates do not refer to derived
  -- predicates. We have to defer this check until last, because
  -- derivations may have been attached to existing predicates now. We
  -- have to check
  --  (a) all the predicates in this schema, and
  --  (b) all the predicates that have derivations in this schema
  forM_ (map predicateRef predicates ++ map fst typecheckedPredicates) $ \ref ->
    case HashMap.lookup ref finalPreds of
      Nothing -> return ()
      Just PredicateDetails{..} -> do
        let
          check = do
            checkStoredType finalPreds types ref predicateKeyType
            checkStoredType finalPreds types ref predicateValueType
        case predicateDeriving of
          NoDeriving -> return ()
             -- In principle we should really enforce that predicates without
             -- a derivation don't refer to non-stored derived predicates.
             -- However, this can crop up in benign ways with backwards
             -- compatibility. For example,
             --
             -- schema S.2 : S.1 {
             --   predicate P : T default P.1 ...
             --   predicate Q : P
             -- }
             --
             -- if we have no facts of P.2 then the derivation would be
             -- enabled, and Q doesn't make sense. But there won't be any
             -- facts of Q either, so it doesn't matter.
          Derive DerivedAndStored _ -> check
          _ -> return ()

  return env
    { tcEnvPredicates = finalPreds
    }

  where
    -- NOTE: We store Maybe Type rather than filtering out those typedefs
    -- for which rtsType returns Nothing here because we need to be sufficiently
    -- lazy to tie the knot in rtsType. We get Nothing for typedefs which refer
    -- to predicates that are not in the database but do exist in the 'Schemas'
    -- (cf. 'mkDbSchema') or, more specifically, in `refToPid`, and then also
    -- for typedefs which refer to those typedefs and so on.
    --
    -- Also note we need a lazy HashMap here to avoid forcing the
    -- elements too early, which would also cause a loop.
    typedefs :: Lazy.HashMap.HashMap TypeRef (Maybe RTS.Type)
    typedefs = Lazy.HashMap.fromList
      [ (ref, rtsType (typeDefType def))
      | (ref, def) <- Lazy.HashMap.toList resolvedSchemaTypes ]

    lookupType :: TypeRef -> Maybe RTS.Type
    lookupType ref =
      case Lazy.HashMap.lookup ref typedefs of
        Nothing -> typeType <$> Lazy.HashMap.lookup ref tcEnvTypes
        Just r -> r

    rtsType = mkRtsType lookupType (`HashMap.lookup` refToPid)

usesOfNegation
  :: HashMap PredicateRef PredicateDetails
  -> HashMap PredicateRef UseOfNegation
usesOfNegation preds =
  foldl' recordUseOfNegation mempty derivations
  where
    recordUseOfNegation usesNegation (mUseOfNeg, ref, deps)
      | Just use <- mUseOfNeg
      = HashMap.insert ref use usesNegation
      | Just use <- firstJust (`HashMap.lookup` usesNegation) deps
      = HashMap.insert ref use usesNegation
      | otherwise
      -- remove from Map in case a derived predicate in this schema that does
      -- not use negation overrode a derived predicate that used negation.
      = HashMap.delete ref usesNegation

    -- derivations in dependency order with flag for use of negation
    derivations :: [(Maybe UseOfNegation, PredicateRef, [PredicateRef])]
    derivations = toPred . getNode <$> reverse (topSort graph)
      where
        (graph, getNode, _) = graphFromEdges
          [ fromPred
            (tcQueryUsesNegation query, ref, Set.toList (tcQueryDeps query))
          | (ref, details) <- HashMap.toList preds
          , Derive _ (QueryWithInfo query _ _)  <- [predicateDeriving details]
          ]
        fromPred (b, p, ps) = (b, toKey p, map toKey ps)
        toPred (b, k, ks)   = (b, fromKey k, map fromKey ks)
        toKey (PredicateRef n v) = (n, v)
        fromKey (n, v) = PredicateRef n v

-- | Check that the type of a stored predicate doesn't refer to any
-- derived predicates.
checkStoredType
  :: HashMap PredicateRef PredicateDetails
  -> HashMap TypeRef TypeDetails
  -> PredicateRef
  -> RTS.Type
  -> IO ()
checkStoredType preds types def ty = go ty
  where
  go (Array ty) = go ty
  go (Maybe ty) = go ty
  go (Record fields) = forM_ fields $ \(FieldDef _ ty) -> go ty
  go (Sum fields) = forM_ fields $ \(FieldDef _ ty) -> go ty
  go (Predicate (PidRef _ ref)) = case HashMap.lookup ref preds of
    Just PredicateDetails{..} -> case predicateDeriving of
      Derive DerivedAndStored _ -> return ()
      Derive _ _ -> throwIO $ ErrorCall $ show $
         "stored predicate " <> pretty def <>
         " refers to non-stored derived predicate " <> pretty ref
      _ -> return ()
    Nothing -> error "checkStoredType"
  go (NamedType (ExpandedType ref _)) = case HashMap.lookup ref types of
    Just TypeDetails{..} -> go typeType
    Nothing -> error "checkStoredType"
  go _ = return ()


validateNewSchema :: ByteString -> SourceSchemas -> Schemas -> IO ()
validateNewSchema newSrc curSrc curSchemas = do
  (newSource, newResolved) <- case parseAndResolveSchema newSrc of
    Left msg -> throwIO $ Thrift.Exception $ Text.pack msg
    Right resolved -> return resolved

  curDbSchema <- newDbSchema curSrc curSchemas readWriteContent
  void $ newMergedDbSchema
    (toSchemaInfo curDbSchema)
    newSource
    newResolved
    MustBeEqual
    readWriteContent

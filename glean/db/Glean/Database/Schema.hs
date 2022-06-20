{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema
  ( DbSchema(..)
  , PredicateDetails(..)
  , predicateRef
  , TypeDetails(..)
  , schemaSize, schemaPredicates
  , fromSchemaInfo, toSchemaInfo
  , newDbSchema
  , newMergedDbSchema
  , CheckChanges(..)
  , lookupPid
  , validateNewSchema
  , DbContent
  , readOnlyContent
  , readWriteContent
  ) where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State as State
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Graph
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Lazy as Lazy.HashMap
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
import Safe (maximumMay)
import TextShow

import Util.Log.Text

import Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Schema.Types
import Glean.Database.Schema.Transform (mkPredicateTransformation)
import Glean.RTS.Traverse
import Glean.RTS.Typecheck
import Glean.RTS.Types as RTS
import Glean.Angle.Types as Schema
import Glean.Schema.Resolve
import Glean.Schema.Util (showRef)
import Glean.Query.Codegen (QueryWithInfo(..))
import Glean.Query.Typecheck
import Glean.Types as Thrift
import Glean.Schema.Types
import Glean.Database.Schema.ComputeIds

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
schemaSize = HashMap.size . predicatesById

schemaPredicates :: DbSchema -> [PredicateDetails]
schemaPredicates = HashMap.elems . predicatesById


fromSchemaInfo :: Thrift.SchemaInfo -> DbContent -> IO DbSchema
fromSchemaInfo info@Thrift.SchemaInfo{..} dbContent =
  mkDbSchemaFromSource
    (Just (schemaInfoPids info))
    dbContent
    schemaInfo_schema

schemaInfoPids :: Thrift.SchemaInfo -> HashMap PredicateRef Pid
schemaInfoPids Thrift.SchemaInfo{..} = HashMap.fromList
  [ (ref, Pid pid) | (pid, ref) <- Map.toList schemaInfo_predicateIds ]

toSchemaInfo :: DbSchema -> Thrift.SchemaInfo
toSchemaInfo DbSchema{..} = Thrift.SchemaInfo
  { schemaInfo_schema =
      Text.encodeUtf8 $ renderStrict $ layoutCompact $ pretty schemaSource
  , schemaInfo_predicateIds = Map.fromList
      [ (fromPid $ predicatePid p, predicateRef p)
      | p <- HashMap.elems predicatesById ]
  }


data CheckChanges
  = AllowChanges
  | MustBeEqual

newMergedDbSchema
  :: Thrift.SchemaInfo                  -- ^ schema from a DB
  -> SourceSchemas                      -- ^ current schema source
  -> Schemas                            -- ^ current schema
  -> CheckChanges                           -- ^ validate DB schema?
  -> DbContent
  -> IO DbSchema
newMergedDbSchema info@Thrift.SchemaInfo{..} source current validate dbContent = do
  let
  (_fromDBSource, fromDBResolved) <-
    case parseAndResolveSchema schemaInfo_schema of
      Left msg -> throwIO $ ErrorCall msg
      Right resolved -> return resolved

  -- For the "source", we'll use the current schema source. It doesn't
  -- reflect what's in the DB, but it reflects what you can query for.
  mkDbSchema validate  (Just (schemaInfoPids info)) dbContent source
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
  mkDbSchema AllowChanges Nothing dbContent str schemas []

inventory :: [PredicateDetails] -> Inventory
inventory ps = Inventory.new
  [ Inventory.CompiledPredicate
      { compiledPid = predicatePid
      , compiledRef = predicateRef d
      , compiledTypecheck = predicateTypecheck
      , compiledTraversal = predicateTraversal
      }
    | d@PredicateDetails{..} <- ps ]

-- |
-- Make a DbSchema for an existing database, using the predicates
-- stored in the database.  Anything in the 'Schemas' but not
-- in the database is dropped from the 'DbSchema'
mkDbSchemaFromSource
  :: Maybe (HashMap PredicateRef Pid)
  -> DbContent
  -> ByteString
  -> IO DbSchema
mkDbSchemaFromSource knownPids dbContent source = do
  case parseAndResolveSchema source of
    Left str -> throwIO $ ErrorCall str
    Right (srcSchemas, resolved) ->
      mkDbSchema AllowChanges knownPids dbContent srcSchemas resolved []

mkDbSchema
  :: CheckChanges
  -> Maybe (HashMap PredicateRef Pid)
  -> DbContent
  -> Schema.SourceSchemas
  -> Schemas
  -> [ResolvedSchemaRef]
  -> IO DbSchema
mkDbSchema validate knownPids dbContent source base addition = do
  let
    resolved = schemasResolved base <> addition

    stored = computeIds (schemasResolved base)
    added = computeIds addition

    -- Assign Pids to predicates.
    --  - predicates in the stored schema get Pids from the SchemaInfo
    --    (unless this is a new DB, in which case we'll assign fresh Pids)
    --  - predicates in the merged schema get new fresh Pids.

    storedPids = case knownPids of
      Nothing -> zip (HashMap.keys (hashedPreds stored)) [lowestPid..]
      Just pidMap ->
        [ (id, HashMap.lookupDefault (Pid Thrift.iNVALID_ID) ref pidMap)
        | (id, _) <- HashMap.toList (hashedPreds stored)
        , let ref = predicateIdRef id
        ]

    nextPid = maybe lowestPid succ $ maximumMay (map snd storedPids)

    addedPids = zip ids [nextPid ..]
      where ids = filter (not . isStoredPred) (HashMap.keys (hashedPreds added))
            isStoredPred id = HashMap.member id (hashedPreds stored)

    idToPid = HashMap.fromList (storedPids ++ addedPids)

    refToIdEnv = RefToIdEnv {
        typeRefToId = HashMap.union
          (typeRefToId (schemaRefToIdEnv added))
          (typeRefToId (schemaRefToIdEnv stored)),
        predRefToId = HashMap.union
          (predRefToId (schemaRefToIdEnv added))
          (predRefToId (schemaRefToIdEnv stored))
      }

    -- added overrides stored above. This mostly shouldn't happen, and
    -- validate would fail if the schemas differ, but in the case
    -- where the new schema adds a default derivation to the old
    -- schema, we want the new derivation to take effect.

    verStored = maybe latestAngleVersion resolvedSchemaAngleVersion
      (listToMaybe (schemasResolved base))

    verAdded =  maybe latestAngleVersion resolvedSchemaAngleVersion
      (listToMaybe addition)

  checkForChanges validate stored added

  -- typecheck all the schemas, producing PredicateDetails / TypeDetails
  tcEnv <- typecheckSchemas idToPid dbContent
    verStored stored verAdded added

  -- Check the invariant that stored predicates do not make use of
  -- negation or refer to derived predicates that make use of it.
  -- See Note [Negation in stored predicates]
  let
    usingNegation = usesOfNegation (tcEnvPredicates tcEnv)
    isStored = \case
      NoDeriving -> True
      Derive when _ -> case when of
        DeriveOnDemand -> False
        DerivedAndStored -> True
        DeriveIfEmpty -> True
    useOfNegation ref = HashMap.lookup ref usingNegation

  forM_ (tcEnvPredicates tcEnv) $ \d@PredicateDetails{..} ->
    when (isStored predicateDeriving) $
      case useOfNegation predicateId of
        Nothing -> return ()
        Just use ->
          let feature = case use of
                PatternNegation -> "negation"
                IfStatement -> "if-statements"
          in
          throwIO $ Thrift.Exception
          $ "use of " <> feature  <> " is not allowed in a stored predicate: "
          <> showRef (predicateRef d)

  let predicates = HashMap.elems (tcEnvPredicates tcEnv)

      byPid = IntMap.fromList
        [ (fromIntegral (fromPid predicatePid), deets)
        | deets@PredicateDetails{..} <- predicates ]

      maxPid = maybe lowestPid (Pid . fromIntegral . fst)
        (IntMap.lookupMax byPid)

      resolvedAlls =
        [ schema | schema <- resolved, resolvedSchemaName schema == "all" ]

      -- In the environment that we use for resolving names in queries
      -- and derivations later, we need to accept explicitly versioned
      -- references to predicates and types even for those predicates
      -- and types that are not visible in the scope of the "all" schema.
      versionedNameEnv = HashMap.fromList
        [ (SourceRef name (Just ver), Set.singleton target)
        | (name, ver, target) <-
            [ (predicateRef_name r, predicateRef_version r, RefPred t)
            | (r, t) <- HashMap.toList (predRefToId refToIdEnv) ] ++
            [ (typeRef_name r, typeRef_version r, RefType t)
            | (r, t) <- HashMap.toList (typeRefToId refToIdEnv) ]
        ]

      schemaEnvs =
        [ (fromIntegral resolvedSchemaVersion, hashNameEnv env, env)
        | ResolvedSchema{..} <- resolvedAlls
        , let env =
                HashMap.union
                  (mapNameEnv (Just . refsToIds refToIdEnv)
                    resolvedSchemaUnqualScope) $
                 HashMap.union
                   (mapNameEnv (Just . refsToIds refToIdEnv)
                     resolvedSchemaQualScope)
                   versionedNameEnv
        ]

      legacyAllVersions = IntMap.fromList
        [ (ver, hash) | (ver, hash, _) <- schemaEnvs ]

      schemaEnvMap = Map.fromList
        [ (hash, env) | (_, hash, env) <- schemaEnvs ]

  latestHash <- maybe (throwIO $ ErrorCall "no schema version") return $ do
    ver <- maximumMay (map resolvedSchemaVersion resolvedAlls)
    IntMap.lookup (fromIntegral ver) legacyAllVersions

  vlog 2 $ "DB schema has " <>
    showt (HashMap.size (hashedTypes stored)) <> " types/" <>
    showt (HashMap.size (hashedPreds stored)) <>
    " predicates, global schema has " <>
    showt (HashMap.size (hashedTypes added)) <> " types/" <>
    showt (HashMap.size (hashedPreds added)) <>
    " predicates, final schema has " <>
    showt (HashMap.size (tcEnvTypes tcEnv)) <> " types/" <>
    showt (HashMap.size (tcEnvPredicates tcEnv)) <> " predicates."

  vlog 2 $ "all schemas: " <> Text.intercalate " " (
    map (showSchemaRef . schemaRef) resolvedAlls)

  return $ DbSchema
    { predicatesById = tcEnvPredicates tcEnv
    , typesById = tcEnvTypes tcEnv
    , schemaEnvs = schemaEnvMap
    , legacyAllVersions = legacyAllVersions
    , predicatesByPid = byPid
    , predicatesTransformations =
        mkTransformations dbContent byPid (tcEnvPredicates tcEnv)
          refToIdEnv resolved
    , schemaInventory = inventory predicates
    , schemaResolved = resolved
    , schemaSource = source
    , schemaMaxPid = maxPid
    , schemaLatestVersion = latestHash
    }

-- | Check that predicates with the same name/version have the same definitions.
-- This is done by comparing Ids, which are hashes of the representation.
checkForChanges
  :: CheckChanges
  -> HashedSchema
  -> HashedSchema
  -> IO ()
checkForChanges AllowChanges _ _ = return ()
checkForChanges MustBeEqual
    (HashedSchema typesStored predsStored _)
    (HashedSchema typesAdded predsAdded _) = do
  let (_, (errors, _, _)) = runState go ([], HashMap.empty, HashMap.empty)
  unless (null errors) $
    throwIO $ Thrift.Exception $ Text.intercalate "\n\n" errors
  where
  go = do
    let types = HashMap.toList typesStored ++ HashMap.toList typesAdded
    forM_ types $ \(id, _) -> do
      (errs, pids, tids) <- State.get
      let (err, tids') = insert "type" (typeIdRef id) id tids
      State.put (err <> errs, pids, tids')
    let preds = HashMap.toList predsStored ++ HashMap.toList predsAdded
    forM_ preds $ \(id, _) -> do
      (errs, pids, tids) <- State.get
      let (err, pids') = insert "predicate" (predicateIdRef id) id pids
      State.put (err <> errs, pids', tids)

  insert thing ref id tids
    | Just id' <- HashMap.lookup ref tids =
      if id == id'
        then ([], tids)
        else ([thing <> " " <> showRef ref <> " has changed"], tids)
    | otherwise = ([], HashMap.insert ref id tids)

detailsFor :: IntMap PredicateDetails -> Pid -> PredicateDetails
detailsFor byId pid =
  case IntMap.lookup (fromIntegral $ fromPid pid) byId of
    Just details -> details
    Nothing -> error $ "unknown pid " <> show pid

mkTransformations
  :: DbContent
  -> IntMap PredicateDetails
  -> HashMap PredicateId PredicateDetails
  -> RefToIdEnv
  -> [ResolvedSchemaRef]
  -> IntMap PredicateTransformation
mkTransformations DbWritable _ _ _ _ = mempty
mkTransformations (DbReadOnly stats) byId byRef refToId resolved =
  IntMap.fromList
    [ (fromIntegral (fromPid old), evolution)
    | (old, new) <- Map.toList $
        transformedPredicates stats byRef refToId resolved
    , Just evolution <- [mkTransformation old new]
    ]
  where
    mkTransformation old new =
      mkPredicateTransformation (detailsFor byId) old new

-- ^ Create a map of which should be transformed into which
transformedPredicates
  :: HashMap Pid PredicateStats
  -> HashMap PredicateId PredicateDetails
  -> RefToIdEnv
  -> [ResolvedSchemaRef]
  -> Map Pid Pid -- ^ when key pred is requested, get value pred from db
transformedPredicates stats byRef refToId resolved =
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
            -- if we have 'b evolves a' and 'c evolves b'
            -- given 'a' produces [b, c]
            newerSchemas = close directEvolves src
            -- given 'c' produces [b, a]
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
            $ mapMaybe ((`HashMap.lookup` (predRefToId refToId)) . predicateDefRef)
            $ HashMap.elems
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
        choose new _old = new

    exportsResolved :: ResolvedSchemaRef -> Map Name PidRef
    exportsResolved schema = Map.fromList
      [ (predicateRef_name (predicateIdRef id),
          PidRef (predicatePid details) id)
      | ref <- map predicateDefRef $ HashMap.elems $
          resolvedSchemaPredicates schema
          <> resolvedSchemaReExportedPredicates schema
      , Just id <- [HashMap.lookup ref (predRefToId refToId)]
      , Just details <- return $ HashMap.lookup id byRef
      ]

{- Note [Negation in stored predicates]

Facts derived based on the absence of other facts could be invalidated when new
facts are added to an incremental database.

The work required to identify which fact to invalidate is close to that of
re-deriving those predicates all over again.

To keep the overhead on incremental databases as low as possible, stored
derived predicates are not allowed to use negation.
-}

typecheckSchemas
  :: HashMap PredicateId Pid
  -> DbContent
  -> AngleVersion
  -> HashedSchema
       -- ^ Stored in the DB
  -> AngleVersion
  -> HashedSchema
       -- ^ Added from the global schema
  -> IO TcEnv

typecheckSchemas idToPid dbContent
    verStored (HashedSchema typesStored predsStored _)
    verAdded (HashedSchema typesAdded predsAdded _) = do

  let
    -- NOTE: We store Maybe Type rather than filtering out those typedefs
    -- for which rtsType returns Nothing here because we need to be sufficiently
    -- lazy to tie the knot in rtsType. We get Nothing for typedefs which refer
    -- to predicates that are not in the database but do exist in the 'Schemas'
    -- (cf. 'mkDbSchema') or, more specifically, in `refToPid`, and then also
    -- for typedefs which refer to those typedefs and so on.
    --
    -- Also note we need a lazy HashMap here to avoid forcing the
    -- elements too early, which would also cause a loop.
    typedefs :: Lazy.HashMap.HashMap TypeId (Maybe RTS.Type)
    typedefs = Lazy.HashMap.fromList
      [ (id, rtsType (typeDefType def))
      | (id, def) <- HashMap.toList typesStored ++ HashMap.toList typesAdded ]

    lookupType :: TypeId -> Maybe RTS.Type
    lookupType ref =
      case Lazy.HashMap.lookup ref typedefs of
        Nothing -> Nothing
        Just r -> r

    rtsType = mkRtsType lookupType (`HashMap.lookup` idToPid)

  let
    add preds (stored, (id,def))
      | HashMap.member id preds = return preds
        -- If we have already seen this predicate, no need to
        -- typecheck it again.
      | otherwise =
        case
          ( HashMap.lookup (predicateDefRef def) idToPid
          , rtsType $ predicateDefKeyType def
          , rtsType $ predicateDefValueType def ) of
        (Just pid, Just keyType, Just valueType) -> do
          typecheck <- checkSignature keyType valueType
          traversal <- genTraversal keyType valueType
          let details = PredicateDetails
                { predicatePid = pid
                , predicateId = id
                , predicateSchema = def
                , predicateKeyType = keyType
                , predicateValueType = valueType
                , predicateTraversal = traversal
                , predicateTypecheck = typecheck
                , predicateDeriving = NoDeriving
                , predicateInStoredSchema = stored
                }
          return $ HashMap.insert id details preds
        _ -> return preds

  preds <- foldM add HashMap.empty $
    map (True,) (HashMap.toList predsStored) ++
    map (False,) (HashMap.toList predsAdded)

  let
    types = HashMap.fromList
        [ (id, TypeDetails id ty)
        | (id, Just ty) <- HashMap.toList typedefs ]

    -- Build the environment in which we typecheck predicates from
    -- this schema.
    env = TcEnv
      { tcEnvPredicates = preds
      , tcEnvTypes = types
      }

    tcDeriving pred stored info =
      either (throwIO . ErrorCall . Text.unpack) return $ runExcept $
        typecheckDeriving env angleVersion rtsType pred info
      where angleVersion = if stored then verStored else verAdded

  -- typecheck the derivations
  finalPreds <- forM preds $ \details -> do
    drv <- tcDeriving details (predicateInStoredSchema details) $
      predicateDefDeriving (predicateSchema details)
    let
      enable = return details { predicateDeriving = drv }
      disable = return details { predicateDeriving = NoDeriving }
    if
      | Derive DeriveIfEmpty _ <- drv ->
        case dbContent of
          DbWritable -> disable
          DbReadOnly stats -> do
            case HashMap.lookup (predicatePid details) stats of
              Nothing -> enable
              Just stats
                | predicateStats_count stats == 0 -> enable
                | otherwise -> disable
      | otherwise -> enable

  -- Check the invariant that stored predicates do not refer to derived
  -- predicates. We have to defer this check until last, because
  -- derivations may have been attached to existing predicates now.
  forM_ finalPreds $ \PredicateDetails{..} -> do
    let
      check = do
        checkStoredType finalPreds types predicateId predicateKeyType
        checkStoredType finalPreds types predicateId predicateValueType
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

  return env { tcEnvPredicates = finalPreds }


usesOfNegation
  :: HashMap PredicateId PredicateDetails
  -> HashMap PredicateId UseOfNegation
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
    derivations :: [(Maybe UseOfNegation, PredicateId, [PredicateId])]
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
        toKey (PredicateId n v) = (n, v)
        fromKey (n, v) = PredicateId n v

-- | Check that the type of a stored predicate doesn't refer to any
-- derived predicates.
checkStoredType
  :: HashMap PredicateId PredicateDetails
  -> HashMap TypeId TypeDetails
  -> PredicateId
  -> RTS.Type
  -> IO ()
checkStoredType preds types def ty = go ty
  where
  go (ArrayTy ty) = go ty
  go (MaybeTy ty) = go ty
  go (RecordTy fields) = forM_ fields $ \(FieldDef _ ty) -> go ty
  go (SumTy fields) = forM_ fields $ \(FieldDef _ ty) -> go ty
  go (PredicateTy (PidRef _ ref)) = case HashMap.lookup ref preds of
    Just PredicateDetails{..} -> case predicateDeriving of
      Derive DerivedAndStored _ -> return ()
      Derive _ _ -> throwIO $ ErrorCall $ show $
         "stored predicate " <> pretty def <>
         " refers to non-stored derived predicate " <> pretty ref
      _ -> return ()
    Nothing -> error "checkStoredType"
  go (NamedTy (ExpandedType ref _)) = case HashMap.lookup ref types of
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

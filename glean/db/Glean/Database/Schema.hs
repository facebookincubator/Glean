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
  , fromStoredSchema, toStoredSchema
  , toSchemaInfo
  , newDbSchema
  , newMergedDbSchema
  , CheckChanges(..)
  , lookupPid
  , compareSchemaPredicates
  , validateNewSchema
  , validateNewSchemaInstance
  , DbContent
  , readOnlyContent
  , readWriteContent
  , getSchemaInfo
  , renderSchemaSource
  , toStoredVersions
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State as State
import Data.Bifoldable (bifoldMap)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Foldable
import Data.Graph
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Lazy as Lazy.HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List.Extra (firstJust, nubOrd)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Tuple (swap)
import Safe (maximumMay)
import TextShow

import ServiceData.GlobalStats
import ServiceData.Types
import Util.Log.Text

import Glean.RTS.Foreign.Inventory as Inventory
import Glean.Database.Config
import Glean.Database.Schema.Types
import Glean.Display
import Glean.Query.Transform
  ( transformPattern
  , transformBytes
  , transformFact
  )
import Glean.Internal.Types as Internal (StoredSchema(..))
import Glean.RTS.Traverse
import Glean.RTS.Typecheck
import Glean.RTS.Types as RTS
import Glean.Angle.Hash
import Glean.Angle.Types as Schema
import Glean.Schema.Evolve
  ( validateEvolutions
  , directSchemaEvolutions
  , calcEvolutions
  , visiblePredicates
  , VisiblePredicates
  , visibleDefined
  , mapVisible )
import Glean.Schema.Util (showRef, ShowRef)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Query.Codegen.Types (QueryWithInfo(..))
import Glean.Query.Typecheck

import Glean.Types as Thrift
import Glean.Schema.Types
import Glean.Database.Schema.ComputeIds
import Glean.Query.Prune (pruneDerivations)

-- | Used to decide whether to activate 'derive default' definitions and
-- 'evolves' directives.
-- 'derive default' takes effect for a read-only DB when there are no
-- facts of a predicate.
-- 'evolves' directives take effect for a read-only DB when there are no facts
-- of any predicate in a schema evolved by another schema.
data DbContent
  = DbWritable
  | DbReadOnly (HashMap Pid PredicateStats)

readOnlyContent :: HashMap Pid PredicateStats -> DbContent
readOnlyContent = DbReadOnly

readWriteContent :: DbContent
readWriteContent = DbWritable


schemaSize :: DbSchema -> Int
schemaSize = HashMap.size . predicatesById

schemaPredicates :: DbSchema -> [PredicateDetails]
schemaPredicates = HashMap.elems . predicatesById


fromStoredSchema
  :: Maybe (MVar DbSchemaCache)
  -> StoredSchema
  -> DbContent
  -> IO DbSchema
fromStoredSchema schemaCache info@StoredSchema{..} dbContent =
  mkDbSchemaFromSource
    schemaCache
    (Just (storedSchemaPids info))
    dbContent
    storedSchema_schema

storedSchemaPids :: StoredSchema -> HashMap PredicateRef Pid
storedSchemaPids StoredSchema{..} = HashMap.fromList
  [ (ref, Pid pid) | (pid, ref) <- Map.toList storedSchema_predicateIds ]

toStoredSchema :: DbSchema -> StoredSchema
toStoredSchema DbSchema{..} = StoredSchema
  { storedSchema_schema = renderSchemaSource (fst schemaSource)
  , storedSchema_predicateIds = Map.fromList $
      [ (fromPid $ predicatePid p, predicateRef p)
      | p <- HashMap.elems predicatesById
      , predicateInStoredSchema p
      ]
      -- Note: only the stored predicates. There can be multiple
      -- predicates with the same PredicateRef (different Pid and
      -- PredicateId), but only one of them is from the stored schema
      -- and corresponds to the Pids of facts stored in the DB.
  , storedSchema_versions = toStoredVersions (snd schemaSource)
  }

-- Similar to toStoredSchema, but includes non-stored predicates too
toSchemaInfo :: DbSchema -> SchemaInfo
toSchemaInfo DbSchema{..} = SchemaInfo
  { schemaInfo_schema = renderSchemaSource (fst schemaSource)
  , schemaInfo_predicateIds = Map.fromList $
      [ (fromPid $ predicatePid p, predicateRef p)
      | p <- HashMap.elems predicatesById
      ]
  , schemaInfo_schemaIds = toStoredVersions (snd schemaSource)
  }

data CheckChanges
  = AllowChanges
  | MustBeEqual

newMergedDbSchema
  :: Maybe (MVar DbSchemaCache)
  -> StoredSchema  -- ^ schema from a DB
  -> SchemaIndex  -- ^ current schema index
  -> CheckChanges  -- ^ validate DB schema?
  -> DbContent
  -> IO DbSchema
newMergedDbSchema schemaCache storedSchema@StoredSchema{..}
    index validate dbContent = do
  let
    -- If we have an identical schema in the index, then we can use
    -- the cached ProcessedSchema. This saves a lot of time
    -- re-processing the schema from the DB when we open it, and
    -- should be a common case.
    --
    -- Schema equality is by comparing the SchemaIds. This is safe
    -- since the only way a schema can be used is through a SchemaId,
    -- and the SchemaId captures the content of all the definitions
    -- that can be referenced by a query (those visible in the NameEnv).
    identicalSchemas =
      [ proc
      | proc <- schemaIndexCurrent index : schemaIndexOlder index
      , fromStoredVersions storedSchema_versions ==
          hashedSchemaAllVersions (procSchemaHashed proc)
      ]

    processStoredSchema =
      case processSchema Map.empty storedSchema_schema of
        Left msg -> throwIO $ ErrorCall msg
        Right resolved -> return resolved

  fromDB <-
    if Map.null storedSchema_versions
      then processStoredSchema
      else case identicalSchemas of
        (one : _) -> return one
        [] -> processStoredSchema

  -- For the "source", we'll use the current schema source. It doesn't
  -- reflect what's in the DB, but it reflects what you can query for.
  mkDbSchema schemaCache validate  (Just (storedSchemaPids storedSchema))
    dbContent fromDB (Just index)

-- | Build a DbSchema from parsed/resolved Schemas. Note that we still need
-- the original source for the schema for 'toStoredSchema' (otherwise we would
-- need to pretty-print the resolved Schemas back into source.
newDbSchema
  :: Maybe (MVar DbSchemaCache)
  -> SchemaIndex
  -> SchemaSelector
  -> DbContent
  -> IO DbSchema
newDbSchema schemaCache index selector dbContent = do
  schema <- case selector of
    SpecificSchemaId id -> case schemaForSchemaId index id of
      Nothing -> throwIO $ ErrorCall $ "schema " <> show id <> " not found"
      Just schema -> return schema
    _otherwise ->
      return (schemaIndexCurrent index)
  mkDbSchema schemaCache AllowChanges Nothing dbContent schema Nothing

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
  :: Maybe (MVar DbSchemaCache)
  -> Maybe (HashMap PredicateRef Pid)
  -> DbContent
  -> ByteString
  -> IO DbSchema
mkDbSchemaFromSource schemaCache knownPids dbContent source = do
  case processSchema Map.empty source of
    Left str -> throwIO $ ErrorCall str
    Right (ProcessedSchema source resolved hashed) ->
      mkDbSchema schemaCache AllowChanges knownPids dbContent
        (ProcessedSchema source resolved hashed)
        Nothing

dbSchemaKey
   :: HashMap PredicateRef Pid
   -> ProcessedSchema
   -> Maybe SchemaIndex
   -> DbSchemaCacheKey
dbSchemaKey pidMap stored maybeIndex = DbSchemaCacheKey $
  hashBinary
    ( [ (showRef ref, pid) | (ref, Pid pid) <- HashMap.toList pidMap ]
    , coerce (hashedSchemaAllVersions (procSchemaHashed stored)) :: IntMap Text
    , flip (maybe []) maybeIndex $ \SchemaIndex{..} ->
        map (coerce . hashedSchemaAllVersions . procSchemaHashed)
          (schemaIndexCurrent : schemaIndexOlder) :: [IntMap Text]
    )

{- |
   Caching DbSchema

   DbSchema is quite expensive to build:
    - we have to typecheck all the derived predicates
    - we have to build RTS.Types for types and predicates
    - we have to compile byte-code for typechecking and traversing facts

   So we'd like to cache these to avoid repeating the work and to
   share as much as possible between open DBs.

   The DbSchema depends on
    - the schema in the DB and the Pids we assigned to predicates
    - the SchemaIndex
    - the content of the DB (which predicates have facts)

   Schemas don't change all that much, so the first two will often be
   the same (or at least, there will be a few combinations in use at
   any given time). For a given schema, the Pids we assign should be
   deterministic.

   The dependency on the DB contents looks problematic. However, most
   of the work of building the DbSchema does *not* depend on the
   contents, only transformations and pruning derived predicates use
   that. So we can cache the plain DbSchema before DbContent is taken
   into account, and then apply the DbContent changes for each DB.
-}

withDbSchemaCache
  :: Maybe (MVar DbSchemaCache)
  -> CheckChanges
  -> Maybe (HashMap PredicateRef Pid)
  -> ProcessedSchema
  -> Maybe SchemaIndex
  -> IO DbSchema
  -> IO DbSchema
withDbSchemaCache maybeCache validate maybePids stored maybeIndex mk =
  case (maybeCache, validate, maybePids) of
    (Just cacheVar, AllowChanges, Just pidMap) -> do
      -- only use the cache when
      --   1. we're not validating (AllowChanges)
      --   2. we have stored Pids
      cache <- readMVar cacheVar
      let key = dbSchemaKey pidMap stored maybeIndex
      case HashMap.lookup key cache of
        Just schema -> do
          addStatValueType "glean.db.schema.cache.hit" 1 Sum
          vlog 2 $ "DbSchema cache hit, stored SchemaId: " <>
            maybe "<unknown>" (unSchemaId . snd)
              (IntMap.lookupMax (hashedSchemaAllVersions
                (procSchemaHashed stored)))
          return schema
        Nothing -> do
          addStatValueType "glean.db.schema.cache.miss" 1 Sum
          schema <- mk
          modifyMVar_ cacheVar $ return . HashMap.insert key schema
          return schema
    _otherwise -> do
      addStatValueType "glean.db.schema.cache.fail" 1 Sum
      mk

mkDbSchema
  :: Maybe (MVar DbSchemaCache)
  -> CheckChanges
  -> Maybe (HashMap PredicateRef Pid)
  -> DbContent
  -> ProcessedSchema
  -> Maybe SchemaIndex
  -> IO DbSchema
mkDbSchema cacheVar validate knownPids dbContent
    procStored@(ProcessedSchema source _ stored) index = do

  -- either fetch a cached DbSchema or build a new one
  schema <-
    withDbSchemaCache cacheVar validate knownPids procStored index
      buildDbSchema

  -- the cached DbSchema cannot depend on the DbContent, so next we add in
  -- any changes to DbSchema that need to depend on DbContent.
  transformations <- failOnLeft $ mkTransformations
    dbContent
    (predicatesById schema)
    procStored
    addedSchemas

  let
      -- to avoid the need to have different prunings for each schemaId
      -- we pass the union of all transformations to the 'prune' function
      -- such that if a predicate can be mapped to something in the db
      -- for any schemaId, then it is kept in the derivation query.
     byId =
       enableSelectiveDerivations dbContent <$>
         prune dbContent allTrans (predicatesById schema)

     intPid = fromIntegral . fromPid . predicatePid
     byPid = IntMap.fromList [ (intPid p, p) | p <- HashMap.elems byId ]

     allTrans = fold $ HashMap.elems transformations

  return $ schema {
    predicatesById = byId,
    predicatesByPid = byPid,
    predicatesTransformations =
      mkQueryTransformations byPid <$> transformations
  }

  where
  addedSchemas = case index of
    Nothing -> []
    Just SchemaIndex{..} -> schemaIndexCurrent : schemaIndexOlder

  buildDbSchema = do
    let
      -- Assign Pids to predicates.
      --  - predicates in the stored schema get Pids from the StoredSchema
      --    (unless this is a new DB, in which case we'll assign fresh Pids)
      --  - predicates in the merged schema get new fresh Pids.
      --
      -- Note that the Pid assignment is deterministic for a given SchemaId,
      -- because HashMap.keys returns the PredicateIds sorted by hash, which
      -- is the low 64 bits of the predicate's Hash.
      storedPids = case knownPids of
        Nothing -> zip (HashMap.keys (hashedPreds stored)) [lowestPid..]
        Just pidMap -> assign first (HashMap.keys (hashedPreds stored))
          where
          first = maybe lowestPid succ $ maximumMay (HashMap.elems pidMap)
          assign _next [] = []
          assign next (id : ids) =
            case HashMap.lookup (predicateIdRef id) pidMap of
              Nothing -> (id, next) : assign (succ next) ids
              Just pid -> (id, pid) : assign next ids

      nextPid = maybe lowestPid succ $ maximumMay (map snd storedPids)

      addedPreds =
        HashMap.unions $ map (hashedPreds . procSchemaHashed) addedSchemas

      addedPids = zip ids [nextPid ..]
        where
        ids = filter (not . isStoredPred) (HashMap.keys addedPreds)
        isStoredPred id = HashMap.member id (hashedPreds stored)

      idToPid = HashMap.fromList (storedPids ++ addedPids)

    mapM_ (checkForChanges validate stored . procSchemaHashed) addedSchemas

    let
      typecheck env (stored, ProcessedSchema _ resolved hashed) =
        typecheckSchema idToPid stored angleVersion hashed env
        where
        angleVersion =  maybe latestAngleVersion resolvedSchemaAngleVersion
          (listToMaybe (schemasResolved resolved))

    -- typecheck all the schemas, producing PredicateDetails / TypeDetails
    tcEnv <- foldM typecheck emptyTcEnv
      ((True, procStored) : map (False,) addedSchemas)

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

    let
        predicates = HashMap.elems (tcEnvPredicates tcEnv)

        intPid = fromIntegral . fromPid . predicatePid

        byPid = IntMap.fromList [ (intPid deets, deets) | deets <- predicates ]

        maxPid = maybe lowestPid (Pid . fromIntegral . fst)
          (IntMap.lookupMax byPid)

        schemaEnvMap =
          Map.unions $
          map (hashedSchemaEnvs . procSchemaHashed) $
          procStored : addedSchemas

        latestSchema =
          case index of
            Just SchemaIndex{..} -> schemaIndexCurrent
            Nothing -> procStored

        legacyAllVersions =
          hashedSchemaAllVersions $ procSchemaHashed latestSchema

        latestSchemaId =
          case IntMap.lookupMax legacyAllVersions of
            Nothing -> error "no \"all\" schema"
            Just (_, id) -> id

        dbSchemaId =
          case IntMap.lookupMax (hashedSchemaAllVersions stored) of
            Nothing -> error "no \"all\" schema in DB"
            Just (_, id) -> id

    vlog 2 $ "DB schema " <> unSchemaId dbSchemaId <> " has " <>
      showt (HashMap.size (hashedTypes stored)) <> " types/" <>
      showt (HashMap.size (hashedPreds stored)) <>
      " predicates, global schema has " <>
      showt (sum (map (HashMap.size . hashedTypes . procSchemaHashed)
        addedSchemas)) <> " types/" <>
      showt (sum (map (HashMap.size . hashedPreds . procSchemaHashed)
        addedSchemas)) <>
      " predicates, final schema has " <>
      showt (HashMap.size (tcEnvTypes tcEnv)) <> " types/" <>
      showt (HashMap.size (tcEnvPredicates tcEnv)) <> " predicates."

    forM_ (IntMap.toList legacyAllVersions) $ \(n, id) ->
      vlog 2 $ "all." <> showt n <> " = " <> unSchemaId id

    return $ DbSchema
      { predicatesById = tcEnvPredicates tcEnv
      , typesById = tcEnvTypes tcEnv
      , schemaEnvs = schemaEnvMap
      , legacyAllVersions = legacyAllVersions
      , predicatesByPid = byPid
      , predicatesTransformations = HashMap.empty
      , schemaInventory = inventory predicates
      , schemaSource = (source, hashedSchemaAllVersions stored)
      , schemaMaxPid = maxPid
      , schemaLatestVersion = latestSchemaId
      }

mkTransformations
  :: DbContent
  -> HashMap PredicateId PredicateDetails
  -> ProcessedSchema   -- ^ stored schema
  -> [ProcessedSchema] -- ^ other schemas
  -> Either Text (HashMap SchemaId (IntMap PredicateTransformation))
mkTransformations content byId stored otherSchemas =
  case content of
    DbWritable -> Right $ HashMap.fromList
      [ (schemaId, mempty)
      | schema <- processed
      , schemaId <- versionsOfAll schema
      ]
    DbReadOnly stats -> do
      evolutions :: [HashMap PredicateId PredicateId] <-
        forM processed $ \schema -> mkEvolutions stats byId [stored, schema]

      let allPredicateTrans :: HashMap (PredicateId, PredicateId) PredicateTransformation
          allPredicateTrans = HashMap.fromList
            [ (pair, trans)
            | pair@(old, new) <- nubOrd $ concatMap HashMap.toList evolutions
            , Just trans <- [mkPredicateTransformation detailsById old new]
            ]

          transformations :: [IntMap PredicateTransformation]
          transformations =
            [ IntMap.fromList
                [ (intPid, trans)
                | pair@(old, _) <- HashMap.toList e
                , Just trans <- [HashMap.lookup pair allPredicateTrans]
                , let intPid = fromIntegral
                        $ fromPid
                        $ predicatePid
                        $ detailsById old
                ]
            | e <- evolutions
            ]

      -- Transformations are per ProcessedSchema. Here we find the
      -- transformations applicable to the union of the stored and a foreign
      -- ProcessedSchema and then we associate them with all 'all' schemas of
      -- the foreign ProcessedSchema.
      return $ HashMap.fromList
        [ (schemaId, trans)
        | (schema, trans) <- zip processed transformations
        , schemaId <- versionsOfAll schema
        ]
  where
  processed :: [ProcessedSchema]
  processed = stored : otherSchemas

  versionsOfAll :: ProcessedSchema -> [SchemaId]
  versionsOfAll = IntMap.elems . hashedSchemaAllVersions . procSchemaHashed

  detailsById id = HashMap.lookupDefault err id byId
    where err = error $ "mkEvolutions: " <> show (displayDefault id)


-- Calculate predicate evolutions for a set of ProcessedSchema
mkEvolutions
  :: HashMap Pid PredicateStats
  -> HashMap PredicateId PredicateDetails
  -> [ProcessedSchema]
  -> Either Text (HashMap PredicateId PredicateId)
mkEvolutions stats byId schemas = do
  manual <- calcSchemaEvolutions hasFacts bySchemaRef <$> directEvolutions schemas
  auto <- calcAutoEvolutions hasFacts byPredRef
  evolutions <- calcEvolutions predicateIdRef byPredRef bySchemaRef manual auto
  validateEvolutions (Just hasFacts) types preds evolutions
  return evolutions
  where
  (types, preds) = definitions schemas

  hasFacts id = fromMaybe False $ do
    details <- HashMap.lookup id byId
    let pid = predicatePid details
    pstats <- HashMap.lookup pid stats
    return $ predicateStats_count pstats > 0

  byPredRef :: Map PredicateRef [PredicateId]
  byPredRef = predicatesByPredicateRef schemas

  bySchemaRef :: Map SchemaRef (VisiblePredicates PredicateId)
  bySchemaRef = predicatesBySchemaRef schemas

predicatesByPredicateRef :: [ProcessedSchema] -> Map PredicateRef [PredicateId]
predicatesByPredicateRef schemas =
  Set.toList <$>
  Map.fromListWith (<>)
  [ (predicateIdRef pred, Set.singleton pred)
  | processed <- schemas
  , pred <- HashMap.keys $ hashedPreds (procSchemaHashed processed)
  ]

predicatesBySchemaRef
  :: [ProcessedSchema]
  -> Map SchemaRef (VisiblePredicates PredicateId)
predicatesBySchemaRef schemas =
  Map.fromListWith (<>)
  [ ( schemaRef resolved, visible)
  | processed <- schemas
  , let env = predRefToId $ schemaRefToIdEnv $ procSchemaHashed processed
        refToId ref =
          HashMap.lookupDefault (error "predicatesBySchemaRef") ref env
  , resolved <- schemasResolved $ procSchemaResolved processed
  , let visible = mapVisible refToId (visiblePredicates resolved)
  ]

-- | Evolves relationships as per source code annotations.
--
-- We only consider evolution annotations in the current schema. This is so
-- that we don't need to wait until schemas are garbage-collected to be able to
-- remove an evolves relationship.
directEvolutions :: [ProcessedSchema] -> Either Text (Map SchemaRef SchemaRef)
directEvolutions schemas = directSchemaEvolutions $
  concatMap (schemasResolved . procSchemaResolved) schemas

-- | Calculate predicate evolutions caused by version-less schema migrations.
calcAutoEvolutions
  :: (PredicateId -> Bool)
  -> Map PredicateRef [PredicateId] -- ^ all SchemaIndex predicates
  -> Either Text (Map PredicateRef PredicateId)
calcAutoEvolutions hasFacts byRef =
  Map.traverseMaybeWithKey choose byRef
  where
  -- from a list of predicates with the same PredicateRef,
  -- choose the one that has facts in the DB.
  choose :: PredicateRef -> [PredicateId] -> Either Text (Maybe PredicateId)
  choose ref xs =
    case filter hasFacts xs of
      []  -> Right Nothing
      [y] -> Right (Just y)
      ys  -> Left $ Text.unwords
        [ "db has facts for multiple instances of", showRef ref, ":"
        , Text.unwords (showRef <$> ys)
        ]

-- | Calculate which schema should effectively evolve which by taking the DB's
-- content into account.
--
-- If A.3 evolves A.2 which evolves A.1, but we only have facts for predicates
-- from A.3, then we end-up with both A.1 and A.2 being directly evolved by
-- A.3.
calcSchemaEvolutions
  :: (PredicateId -> Bool)
  -> Map SchemaRef (VisiblePredicates PredicateId)
  -> Map SchemaRef SchemaRef
  -> Map SchemaRef SchemaRef -- ^ value evolves key
calcSchemaEvolutions hasFacts bySchemaRef direct =
  Map.fromList
    [ (ref, target)
    | ref <- Map.keys bySchemaRef
    , Just target <- [transformationTarget ref]
    ]
  where
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
            newerSchemas = close (`Map.lookup` direct) src
            -- given 'c' produces [b, a]
            olderSchemas = close (`Map.lookup` reverseDirect) src

    reverseDirect :: Map SchemaRef SchemaRef
    reverseDirect =
      Map.fromList $ map swap $ Map.toList direct

    hasFactsInDb :: SchemaRef -> Bool
    hasFactsInDb sref = sref `Set.member` withFacts

    withFacts :: Set SchemaRef
    withFacts = Map.keysSet $
      Map.filter (any hasFacts . visibleDefined) bySchemaRef

close :: (a -> Maybe a) -> a -> [a]
close next x = case next x of
  Just y -> y : close next y
  Nothing -> []

-- | Optimise derivation queries for fetching facts given the particular db's
-- content.
prune
  :: DbContent
  -> IntMap PredicateTransformation
  -> HashMap PredicateId PredicateDetails
  -> HashMap PredicateId PredicateDetails
prune DbWritable _ pmap = pmap
  -- don't change derivations while db is still writable
prune (DbReadOnly stats) transformations pmap = pruneDerivations hasFacts pmap
  where
    -- A predicate has facts in the db if it or the predicate it would be
    -- transformed into has facts in the db.
    hasFacts predId = fromMaybe False $ do
      details <- HashMap.lookup predId pmap
      let pid = predicatePid details
          availablePid = fromMaybe pid $ transformationFor pid
      pstats <- HashMap.lookup availablePid stats
      return $ predicateStats_count pstats > 0

    transformationFor :: Pid -> Maybe Pid
    transformationFor pid = do
      trans <- IntMap.lookup (fromIntegral $ fromPid pid) transformations
      PidRef pid _ <- return $ tAvailable trans
      return pid


-- | Check that predicates with the same name/version have the same definitions.
-- This is done by comparing Ids, which are hashes of the representation.
checkForChanges
  :: CheckChanges
  -> HashedSchema
  -> HashedSchema
  -> IO ()
checkForChanges AllowChanges _ _ = return ()
checkForChanges MustBeEqual
    (HashedSchema typesStored predsStored _ _ _)
    (HashedSchema typesAdded predsAdded _ _ _) = do
  let errors =
        compareSchemaTypes
          (HashMap.keys typesStored)
          (HashMap.keys typesAdded) <>
        compareSchemaPredicates
          (HashMap.keys predsStored)
          (HashMap.keys predsAdded)
  unless (null errors) $
    throwIO $ Thrift.Exception $ Text.intercalate "\n\n" errors

-- | Compare schemas for compatibility. That is, if schema A defines a
-- predicate or type P, then schema B must have an identical
-- definition of P.
--
-- This is used to determine whether we can safely use schema B for a
-- stacked DB when the base DB is using schema A. It's useful to be
-- able to do this, because we might want to add a new derived
-- predicate to B for testing purposes, but it's only safe if we
-- didn't change any predicates defined by A, because those will be
-- mapped to the same Pids in the stacked DB.
--
compareSchemaPredicates :: [PredicateId] -> [PredicateId] -> [Text]
compareSchemaPredicates predsA predsB = errors
  where
  (errors, _) = flip execState ([], HashMap.empty) $
    forM_ (predsA ++ predsB) $ \id -> do
      (errs, pids) <- State.get
      let (err, pids') = compareInsert "predicate" (predicateIdRef id) id pids
      State.put (err <> errs, pids')

compareSchemaTypes :: [TypeId] -> [TypeId] -> [Text]
compareSchemaTypes typesA typesB = errors
  where
  (errors, _) = flip execState ([], HashMap.empty) $
    forM_ (typesA ++ typesB) $ \id -> do
      (errs, tids) <- State.get
      let (err, tids') = compareInsert "type" (typeIdRef id) id tids
      State.put (err <> errs, tids')

compareInsert
  :: (Hashable a, Eq a, Eq b, ShowRef a)
  => Text
  -> a
  -> b
  -> HashMap a b
  -> ([Text], HashMap a b)
compareInsert thing ref id tids
  | Just id' <- HashMap.lookup ref tids =
    if id == id'
      then ([], tids)
      else ([thing <> " " <> showRef ref <> " has changed"], tids)
  | otherwise = ([], HashMap.insert ref id tids)

mkPredicateTransformation
  :: Eq id
  => (id -> PredicateDetails)
  -> id
  -> id
  -> Maybe PredicateTransformation
mkPredicateTransformation detailsFor requestedId availableId
  | requestedId == availableId = Nothing
  | otherwise = Just PredicateTransformation
    { tRequested = pidRef requested
    , tAvailable = pidRef available
    , tTransformFactBack = transformFact available requested
    , transformKeyPattern   = transformPattern (key requested) (key available)
    , transformValuePattern = transformPattern (val requested) (val available)
    , transformKey   = transformBytes (key available) (key requested)
    , transformValue = transformBytes (val available) (val requested)
    }
  where
      key = predicateKeyType
      val = predicateValueType
      available = detailsFor availableId
      requested = detailsFor requestedId

{- Note [Negation in stored predicates]

Facts derived based on the absence of other facts could be invalidated when new
facts are added to an incremental database.

The work required to identify which fact to invalidate is close to that of
re-deriving those predicates all over again.

To keep the overhead on incremental databases as low as possible, stored
derived predicates are not allowed to use negation.
-}

typecheckSchema
  :: HashMap PredicateId Pid
  -> Bool -- ^ stored?
  -> AngleVersion
  -> HashedSchema
  -> TcEnv
  -> IO TcEnv

typecheckSchema idToPid stored angleVersion
    HashedSchema{..} tcEnv = do
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
      | (id, def) <- HashMap.toList hashedTypes ]

    lookupType :: TypeId -> Maybe RTS.Type
    lookupType ref =
      case Lazy.HashMap.lookup ref typedefs of
        Nothing -> Nothing
        Just r -> r

    rtsType = mkRtsType lookupType (`HashMap.lookup` idToPid)

  let
    -- If we have already seen this predicate, no need to
    -- typecheck it again.
    new = HashMap.difference hashedPreds (tcEnvPredicates tcEnv)

    mkPredicateDetails (id,def) =
        case
          ( HashMap.lookup (predicateDefRef def) idToPid
          , rtsType $ predicateDefKeyType def
          , rtsType $ predicateDefValueType def ) of
        (Just pid, Just keyType, Just valueType) -> do
          typecheck <- checkSignature keyType valueType
          traversal <- genTraversal keyType valueType
          return $ Just PredicateDetails
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
        _ -> return Nothing

  newDetails <- catMaybes <$> mapM mkPredicateDetails (HashMap.toList new)

  let
    types = HashMap.fromList
      [ (id, TypeDetails id ty)
      | (id, Just ty) <- HashMap.toList typedefs ]

    insertPred details m = HashMap.insert (predicateId details) details m

    -- Build the environment in which we typecheck predicates from
    -- this schema.
    env = TcEnv
      { tcEnvPredicates = foldr insertPred (tcEnvPredicates tcEnv) newDetails
      , tcEnvTypes = HashMap.union types (tcEnvTypes tcEnv)
      }

    -- Typecheck the derivation for a predicate
    tcDeriving details = do
      drv <- either (throwIO . ErrorCall . Text.unpack) return $ runExcept $
        typecheckDeriving env angleVersion rtsType details
          (predicateDefDeriving (predicateSchema details))
      return details { predicateDeriving = drv }

  -- typecheck the derivations for newly added predicates
  finalDetails <- mapM tcDeriving newDetails
  let finalPreds = foldr insertPred (tcEnvPredicates tcEnv) finalDetails

  -- Check the invariant that stored predicates do not refer to derived
  -- predicates. We have to defer this check until last, because
  -- derivations may have been attached to existing predicates now.
  forM_ finalDetails $ \PredicateDetails{..} -> do
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


-- | Enable or disable a DeriveIfEmpty derivation based on the DB
-- contents.
enableSelectiveDerivations :: DbContent -> PredicateDetails -> PredicateDetails
enableSelectiveDerivations dbContent details =
  let
    disable = details { predicateDeriving = NoDeriving }
  in
  if
    | Derive DeriveIfEmpty _ <- predicateDeriving details ->
      case dbContent of
        DbWritable -> disable
        DbReadOnly stats -> do
          case HashMap.lookup (predicatePid details) stats of
            Nothing -> details
            Just stats
              | predicateStats_count stats == 0 -> details
              | otherwise -> disable
    | otherwise -> details


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
         "stored predicate " <> displayDefault def <>
         " refers to non-stored derived predicate " <> displayDefault ref
      _ -> return ()
    Nothing -> error $ "checkStoredType: " <> Text.unpack (showRef ref)
  go (NamedTy (ExpandedType ref _)) = case HashMap.lookup ref types of
    Just TypeDetails{..} -> go typeType
    Nothing -> error $ "checkStoredType: " <> Text.unpack (showRef ref)
  go _ = return ()


-- | Check that a new schema can be processed and merged with the
-- current schema index. This is used to ensure that a running server
-- will not choke on a new schema before we deploy it.
validateNewSchema :: ServerConfig.Config -> ByteString -> SchemaIndex -> IO ()
validateNewSchema ServerConfig.Config{..} newSrc current = do
  schema <- case processSchema Map.empty newSrc of
    Left msg -> throwIO $ Thrift.Exception $ Text.pack msg
    Right resolved -> return resolved
  let
    -- If use_schema_id is on then we can accept schema changes,
    -- otherwise the new schema must exactly match the old schema
    -- except for added/removed predicates.
    checkChanges
      | config_use_schema_id = AllowChanges
      | otherwise = MustBeEqual

  curDbSchema <- mkDbSchema Nothing AllowChanges Nothing
    readWriteContent schema Nothing
  void $ newMergedDbSchema
    Nothing
    (toStoredSchema curDbSchema)
    current
    checkChanges
    readWriteContent

failOnLeft :: Either Text a -> IO a
failOnLeft = \case
  Right x -> return x
  Left err -> throwIO $ ErrorCall $ Text.unpack err

-- | Check that the current schema in the SchemaIndex is compatible
-- with each of the older schema instances. This is the validity check
-- when adding a new schema instance.
validateNewSchemaInstance :: SchemaIndex -> IO ()
validateNewSchemaInstance (SchemaIndex curr older) = failOnLeft $ do
  -- Prevent recursive predicates. Because this is a constraint we will remove
  -- in the future, for now we only check this when a new schema instance is to
  -- be generated.
  mapM_ checkRecursiveDefinitions (schemasResolved $ procSchemaResolved curr)

  let auto = Map.fromList
        [ (predicateIdRef id, id)
        | id <- HashMap.keys $ hashedPreds $ procSchemaHashed curr ]

  direct <- directEvolutions schemas
  evolutions <- calcEvolutions
    predicateIdRef
    (predicatesByPredicateRef schemas)
    (predicatesBySchemaRef schemas)
    direct
    auto

  if performExhaustiveCheck
    then validateEvolutions Nothing types preds (transitive evolutions)
    else validateEvolutions Nothing types preds evolutions
  where
  schemas = curr : older
  (types, preds) = definitions schemas

  -- I P evolves Q and Q evolves R, we expect that `P evolves R` to work. If Q
  -- removes a field from R and P adds it again with a different type then `P
  -- evolves R` will not hold.
  --
  -- This flag controls whether we should check for compatibility with all
  -- transitively evolved predicates. For now we don't have an efficient way to
  -- perform the check. This is O(n^2), and therefore off by default.
  performExhaustiveCheck = False

  -- add transitive relations explicitly to the map
  --    HashMap.fromList [(1,2),(2,3)]
  -- becomes
  --    HashMap.fromList [(1,2),(2,3),(1,3)]
  transitive :: (Hashable a, Eq a) => HashMap a a -> HashMap a a
  transitive m = HashMap.fromList
    [ (key, val)
    | key <- HashMap.keys m
    , val <- close (`HashMap.lookup` m) key
    ]

-- | Detect recursion and co-recursion in predicate derivations.
-- We don't check default derived predicates because whether they will in fact
-- recurse or not depends on the db's content.
checkRecursiveDefinitions :: ResolvedSchemaRef -> Either Text ()
checkRecursiveDefinitions resolved =
  unless (null recursiveDefinitions) $
  Left $ Text.unlines $
    "found cycles in predicate derivations: " :
    [ Text.intercalate " -> " $ map showRef (x:xs ++ [x])
    | x:xs <- recursiveDefinitions
    ]
  where
    recursiveDefinitions :: [[PredicateRef]]
    recursiveDefinitions =
      cycles (HashMap.keys deps) (\p -> HashMap.lookupDefault [] p deps)

    -- derived predicate to derived predicate dependency map
    deps :: HashMap PredicateRef [PredicateRef]
    deps = fmap (filter hasDerivation .  preds) derivationQueries

    hasDerivation :: PredicateRef -> Bool
    hasDerivation ref = ref `HashMap.member` derivationQueries

    derivationQueries :: HashMap PredicateRef (Query_ PredicateRef TypeRef)
    derivationQueries =
      HashMap.mapMaybe toQuery (resolvedSchemaDeriving resolved)
      where
        toQuery = \case
          NoDeriving -> Nothing
          Derive when query ->
            case when of
              DeriveIfEmpty -> Nothing
              DeriveOnDemand -> Just query
              DerivedAndStored -> Just query

    -- predicates referenced in the query
    preds :: Query_ PredicateRef TypeRef -> [PredicateRef]
    preds query = HashSet.toList $ bifoldMap HashSet.singleton references query

    cycles :: Ord a => [a] -> (a -> [a]) -> [[a]]
    cycles roots children = [ cycle | CyclicSCC cycle <- scc]
      where
      scc = stronglyConnComp [ (node, node, children node) | node <- roots ]

    -- predicates referenced by type
    references :: TypeRef -> HashSet PredicateRef
    references tref = fromMaybe mempty $ do
      TypeDef _ ty <- HashMap.lookup tref (resolvedSchemaTypes resolved)
      return $ bifoldMap HashSet.singleton references ty

definitions
  :: [ProcessedSchema]
  -> (HashMap TypeId TypeDef, HashMap PredicateId PredicateDef)
definitions schemas = (types, preds)
  where
  preds :: HashMap PredicateId PredicateDef
  preds = HashMap.fromList
      [ (id, def)
      | processed <- schemas
      , (id, def) <- HashMap.toList $ hashedPreds $ procSchemaHashed processed
      ]

  types  :: HashMap TypeId TypeDef
  types = HashMap.unions $ hashedTypes . procSchemaHashed <$> schemas


-- | Interrogate the schema associated with a DB
getSchemaInfo :: DbSchema -> SchemaIndex -> GetSchemaInfo -> IO SchemaInfo
getSchemaInfo dbSchema SchemaIndex{..} GetSchemaInfo{..} = do
  let
    SchemaInfo source pids storedVersions = toSchemaInfo dbSchema

    -- Try to find the source for the desired schema. It could be in
    -- our SchemaIndex, or it could be the schema stored in the DB if
    -- we're looking at an old DB.
    findSchemaSource sid =
      case matches of
        (one:_) -> return $ renderSchemaSource one
        [] -> throwIO $ Exception $
          "SchemaId not found: " <> unSchemaId sid
      where
      matches =
        [ procSchemaSource proc
        | proc <- schemaIndexCurrent : schemaIndexOlder
        , sid `Map.member` hashedSchemaEnvs (procSchemaHashed proc)
        ] ++
        [ source
        | (source, versions) <- [ schemaSource dbSchema ]
        , sid `elem` IntMap.elems versions
        ]

    schemaIds
      | SelectSchema_stored{} <- getSchemaInfo_select = storedVersions
      | otherwise = toStoredVersions (legacyAllVersions dbSchema)

  source <- if getSchemaInfo_omit_source
    then return ""
    else case getSchemaInfo_select of
      SelectSchema_stored{} -> return source
      SelectSchema_current{} -> findSchemaSource (schemaLatestVersion dbSchema)
      SelectSchema_schema_id sid -> findSchemaSource sid
      _ -> return source

  return (SchemaInfo source pids schemaIds)

-- The version map in the 'StoredSchema', the 'SchemaIndex' and the
-- 'SchemaInfo' is a @Map Text Version@ whereas the one we use
-- internally is the inverse of this: @IntMap SchemaId@. These two
-- functions convert back and forth.
--
-- TODO: maybe they should be the same.

toStoredVersions :: IntMap SchemaId -> Map Text Version
toStoredVersions versions = Map.fromList
  [ (unSchemaId id, fromIntegral ver) | (ver, id) <- IntMap.toList versions ]

fromStoredVersions :: Map Text Version -> IntMap SchemaId
fromStoredVersions versions = IntMap.fromList
  [ (fromIntegral ver, SchemaId id) | (id, ver) <- Map.toList versions ]

renderSchemaSource :: SourceSchemas -> ByteString
renderSchemaSource parsed =
  Text.encodeUtf8 $ renderStrict $ layoutCompact $ displayDefault parsed

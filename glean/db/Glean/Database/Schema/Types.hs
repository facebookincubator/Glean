{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema.Types
  ( DbSchema(..)
  , DbSchemaCache
  , DbSchemaCacheKey(..)
  , RefTargetId
  , PredicateDetails(..)
  , predicateRef
  , PredicateTransformation(..)
  , Bytes(..)
  , IsPointQuery
  , SchemaSelector(..)
  , schemaNameEnv
  , allSchemaVersion
  , addTmpPredicate
  , lookupSourceRef
  , lookupPredicateSourceRef
  , lookupPredicateId
  , lookupPid
  , lookupTransformation
  , needsTransformation
  , QueryTransformations
  , mkQueryTransformations
  , TypeDetails(..)
  , lookupTypeId
  , dbSchemaRtsType
  , mkRtsType
  , tempPredicateId
  , tempPid
  , pidRef
  , transitiveDeps
  ) where

import Data.Bifoldable (bifoldr')
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))

import Glean.Angle.Hash
import Glean.Angle.Types as Schema hiding (Type, FieldDef)
import qualified Glean.Angle.Types as Schema
import Glean.Bytecode.Types
import Glean.Query.Codegen.Types (Match, Output, TransformAndBind)
import Glean.Database.Schema.ComputeIds
import Glean.Query.Typecheck.Types
import Glean.RTS.Foreign.Bytecode (Subroutine)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Bytecode.Code
import Glean.RTS.Term (Term)
import Glean.RTS.Typecheck
import Glean.RTS.Traverse
import Glean.RTS.Types (Pid(..), Type, PidRef(..), FieldDef, ExpandedType(..))
import Glean.Types as Thrift
import Glean.Schema.Types
import Glean.Schema.Util

-- | The Schema used by a DB
data DbSchema = DbSchema
  { predicatesById :: HashMap PredicateId PredicateDetails
  , derivationDepends :: HashMap PredicateId [PredicateId]
  , typesById :: HashMap TypeId TypeDetails

  , schemaEnvs :: Map SchemaId (NameEnv RefTargetId)
     -- ^ for each SchemaId, the environment of types/predicates

  , legacyAllVersions :: IntMap SchemaId
     -- ^ Maps all.N schema versions to SchemaIds

  , predicatesByPid :: IntMap PredicateDetails

  , predicatesTransformations :: HashMap SchemaId QueryTransformations
    -- ^ Transformations are specific to the union of evolutions in the client
    -- and server schemas. Indexed by client schema id.

  , schemaInventory :: Inventory
  , schemaMaxPid :: Pid
  , schemaLatestVersion :: SchemaId

  , schemaSource :: (SourceSchemas, IntMap SchemaId)
    -- ^ This is for toStoredSchema
  }

-- | Cached DbSchemas, so that we can share them amongst different DBs
type DbSchemaCache = HashMap DbSchemaCacheKey DbSchema

newtype DbSchemaCacheKey = DbSchemaCacheKey Hash
  deriving (Eq, Hashable)

-- | Transformations to be applied to a query. Keyed by the requested predicate
-- (i.e. the one present in the query)
newtype QueryTransformations = QueryTransformations (IntMap TransDetails)

data TransDetails
  = HasTransformation PredicateTransformation
  | DependenciesHaveTransformations -- transitive deps

-- | Data required to transform a predicate that was requested in a query into
-- one that we have available in the database, and then to transform the facts
-- found back into the requested predicate.
data PredicateTransformation = PredicateTransformation
  { tRequested :: PidRef
    -- ^ the predicate that was queried for
  , tAvailable :: PidRef
    -- ^ the predicate we have in the database
  , tTransformFactBack :: Thrift.Fact -> Thrift.Fact
    -- ^ available -> requested
    -- ^ transform a fact of the available predicate into a fact
    -- of the requested predicate.

  , transformKeyPattern :: forall a
    . Maybe ( (Type -> Term (Match TransformAndBind Output) -> Code ())
            -> Term (Match TransformAndBind Output)
            -> (Term (Match TransformAndBind Output) -> Code a)
            -> Code a)
  , transformValuePattern :: forall a
    . Maybe (  (Type -> Term (Match TransformAndBind Output) -> Code ())
            -> Term (Match TransformAndBind Output)
            -> (Term (Match TransformAndBind Output) -> Code a)
            -> Code a)
    -- ^ requested -> available
    -- ^ Transform a pattern into one which can be used to build a prefix for
    -- the available predicate. The resulting pattern cannot be used for
    -- variable binding.
  }

-- | Whether a prefix will match a fact from beginning to end and therefore can
-- only have a single result.
type IsPointQuery = Bool

-- | bytes in a binary output
data Bytes = Bytes
  { b_start :: Register 'DataPtr
  , b_end :: Register 'DataPtr
  }
  deriving (Show)

data TypeDetails = TypeDetails
  { typeId :: TypeId
  , typeType :: Type
  }

data PredicateDetails = PredicateDetails
  { predicatePid :: Pid
  , predicateId :: PredicateId
  , predicateSchema :: PredicateDef
  , predicateKeyType :: Type
  , predicateValueType :: Type
  , predicateTypecheck :: Subroutine CompiledTypecheck
  , predicateTraversal :: Subroutine CompiledTraversal
  , predicateDeriving :: DerivingInfo TypecheckedQuery
  , predicateInStoredSchema :: Bool
    -- ^ True if this prediate is part of the schema stored in the DB.
    -- Only predicates in the stored schema can be written.
  }

predicateRef :: PredicateDetails -> PredicateRef
predicateRef = predicateIdRef . predicateId

data SchemaSelector
  = LatestSchemaAll
  | SpecificSchemaId SchemaId

instance Pretty SchemaSelector where
  pretty LatestSchemaAll = "latest"
  pretty (SpecificSchemaId (SchemaId id)) = "schema-id:" <> pretty id

allSchemaVersion :: DbSchema -> SchemaSelector -> Maybe SchemaId
allSchemaVersion _ (SpecificSchemaId v) = Just v
allSchemaVersion dbSchema LatestSchemaAll = Just (schemaLatestVersion dbSchema)

schemaNameEnv :: DbSchema -> SchemaSelector -> Maybe (NameEnv RefTargetId)
schemaNameEnv dbSchema schemaVer = do
  hash <- allSchemaVersion dbSchema schemaVer
  Map.lookup hash (schemaEnvs dbSchema)

addTmpPredicate :: NameEnv RefTargetId -> NameEnv RefTargetId
addTmpPredicate =
  HashMap.insert sourceRef (Set.singleton (RefPred tempPredicateId))
  where
  PredicateRef name ver = predicateIdRef tempPredicateId
  sourceRef = SourceRef name (Just ver)

lookupSourceRef
  :: SourceRef
  -> SchemaSelector
     -- ^ schema version to use if predicate version is Nothing
  -> DbSchema
  -> LookupResult RefTargetId
lookupSourceRef ref schemaVer dbSchema =
  case schemaNameEnv dbSchema schemaVer of
    Nothing -> OutOfScope
    Just env -> resolveRef env ref

lookupPredicateSourceRef
  :: SourceRef
  -> SchemaSelector
     -- ^ schema version to use if predicate version is Nothing
  -> DbSchema
  -> Either Text PredicateDetails
lookupPredicateSourceRef ref schemaVer dbSchema =
  case lookupResultToEither ref $ lookupSourceRef ref schemaVer dbSchema of
    Right (RefPred pred)
      | Just details <- lookupPredicateId pred dbSchema -> Right details
      | otherwise -> Left $
        "internal error: " <> showRef pred <> " not found"
    Right (RefType _) ->
      Left $ showRef ref <> " is a type, not a predicate"
    Left err -> Left err

lookupPredicateId :: PredicateId -> DbSchema -> Maybe PredicateDetails
lookupPredicateId ref = HashMap.lookup ref . predicatesById

lookupPid :: Pid -> DbSchema -> Maybe PredicateDetails
lookupPid (Pid pid) = IntMap.lookup (fromIntegral pid) . predicatesByPid

lookupTypeId :: TypeId -> DbSchema -> Maybe TypeDetails
lookupTypeId ref  = HashMap.lookup ref . typesById

-- | Either the type or a type transitively referenced by it needs to be
-- transformed.
needsTransformation
  :: QueryTransformations
  -> Pid
  -> Bool
needsTransformation (QueryTransformations tmap) pid =
  isJust $ IntMap.lookup (fromIntegral $ fromPid pid) tmap

lookupTransformation
  :: Pid
  -> QueryTransformations
  -> Maybe PredicateTransformation
lookupTransformation pid (QueryTransformations tmap) = do
  dets <- IntMap.lookup (fromIntegral $ fromPid pid) tmap
  case dets of
    HasTransformation trans -> Just trans
    DependenciesHaveTransformations -> Nothing

transitiveDeps :: (Pid -> PredicateDetails) -> Set Pid -> Pid -> [Pid]
transitiveDeps detailsFor = transitive (predicateDeps . detailsFor)
  where
    transitive :: Ord a => (a -> [a]) -> Set a -> a ->  [a]
    transitive next initial root = go [root] initial
      where
        go [] _ = []
        go (x:xs) visited
          | x `Set.member`visited = go xs visited
          | otherwise = x : go (next x <> xs) (Set.insert x visited)

-- All predicates mentioned in a predicate's type.
-- Does not include predicates from the derivation query.
predicateDeps :: PredicateDetails -> [Pid]
predicateDeps details =
  typeDeps (predicateKeyType details) $
    typeDeps (predicateValueType details) []
  where
    typeDeps ty r = bifoldr' overPidRef overExpanded r ty
    overExpanded (ExpandedType _ ty) r = typeDeps ty r
    overPidRef (PidRef pid _) r = pid : r

tempPid :: DbSchema -> Pid
tempPid = succ . schemaMaxPid

pidRef :: PredicateDetails -> PidRef
pidRef details = PidRef (predicatePid details) (predicateId details)

-- | Convert Schema types to RTS types using a DbSchema
dbSchemaRtsType :: DbSchema -> Schema.Type -> Maybe Type
dbSchemaRtsType dbSchema = mkRtsType lookupType lookupPid
  where
  lookupType ref = typeType <$> lookupTypeId ref dbSchema
  lookupPid ref = case lookupPredicateId ref dbSchema of
    Just pid -> Just $ predicatePid pid
    -- detect temporary predicates that might have been serialized
    -- see Glean.Query.Flatten.captureKey
    _ | tempPredicateId == ref -> Just $ tempPid dbSchema
    _ -> Nothing

-- | Convert from Schema Types to RTS Types. This involves
-- 1. PredicateRef -> PidRef
-- 2. Expand NamedTypes
mkRtsType
  :: (TypeId -> Maybe Type)
  -> (PredicateId -> Maybe Pid)
  -> Schema.Type -> Maybe Type
mkRtsType lookupType lookupPid = rtsType
  where
    rtsType :: Schema.Type -> Maybe Type
    rtsType Schema.ByteTy = return Schema.ByteTy
    rtsType Schema.NatTy = return Schema.NatTy
    rtsType (Schema.ArrayTy elty) = Schema.ArrayTy <$> rtsType elty
    rtsType (Schema.RecordTy fields) = Schema.RecordTy <$> mapM fieldType fields
    rtsType (Schema.SumTy fields) = Schema.SumTy <$> mapM fieldType fields
    rtsType Schema.StringTy = return Schema.StringTy
    rtsType (Schema.PredicateTy ref) = do
      pid <- lookupPid ref
      return (Schema.PredicateTy (PidRef pid ref))
    -- TODO: This will loop if we have recursive typedefs but we don't allow
    -- those at the moment.
    rtsType (Schema.NamedTy ref) =
      Schema.NamedTy . ExpandedType ref <$> lookupType ref
    rtsType (Schema.MaybeTy eltTy) = Schema.MaybeTy <$> rtsType eltTy
    rtsType (Schema.EnumeratedTy names) = return (Schema.EnumeratedTy names)
    rtsType Schema.BooleanTy = return Schema.BooleanTy

    fieldType :: Schema.FieldDef -> Maybe FieldDef
    fieldType (Schema.FieldDef name ty) = Schema.FieldDef name <$> rtsType ty

mkQueryTransformations
  :: IntMap PredicateDetails
  -> IntMap PredicateTransformation
  -> QueryTransformations
mkQueryTransformations pmap tmap = QueryTransformations $
  foldr addSCC mempty (reverse $ Graph.stronglyConnComp edges)
  where
    transFor pid =  IntMap.lookup (intPid pid) tmap
    detailsFor pid = pmap IntMap.! intPid pid
    intPid (Pid pid) = fromIntegral pid
    deps = predicateDeps . detailsFor

    addSCC scc depsTrans = foldr add depsTrans scc
      where
      add pid acc = fromMaybe acc $ do
        trans <- case transFor pid of
          Just trans -> Just $ HasTransformation trans
          Nothing -> defaultForSCC
        return $ IntMap.insert (intPid pid) trans acc

      defaultForSCC =
        if any needsTransformation $ concatMap deps scc
          then Just DependenciesHaveTransformations
          else Nothing
        where needsTransformation pid = intPid pid `IntMap.member` depsTrans

    edges =
      [ (pid, pid, deps pid)
      | pid <- Pid . fromIntegral <$> IntMap.keys pmap
      ]

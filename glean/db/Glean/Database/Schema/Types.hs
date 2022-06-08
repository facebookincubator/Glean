{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema.Types
  ( DbSchema(..)
  , RefTargetId
  , PredicateDetails(..)
  , PredicateTransformation(..)
  , SchemaVersion(..)
  , schemaNameEnv
  , addTmpPredicate
  , lookupSourceRef
  , lookupPredicateSourceRef
  , lookupPredicateId
  , lookupPid
  , TypeDetails(..)
  , lookupTypeId
  , dbSchemaRtsType
  , mkRtsType
  , tempPredicateId
  , tempPid
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Text (Text)

import Glean.Angle.Types as Schema hiding (Type, FieldDef)
import Glean.Angle.Hash
import qualified Glean.Angle.Types as Schema
import Glean.Query.Typecheck.Types
import Glean.RTS.Foreign.Bytecode (Subroutine)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Typecheck
import Glean.RTS.Traverse
import Glean.RTS.Types (Pid(..), Type, PidRef(..), FieldDef, ExpandedType(..))
import Glean.Types as Thrift
import Glean.Schema.Types
import Glean.Schema.Util

type RefTargetId = RefTarget PredicateId TypeId

-- | The Schema used by a DB
data DbSchema = DbSchema
  { predicatesById :: HashMap PredicateId PredicateDetails
  , typesById :: HashMap TypeId TypeDetails

  , schemaEnvs :: IntMap (NameEnv RefTargetId)
     -- ^ for each version of "all", environment of types/predicates

  , predicatesByPid :: IntMap PredicateDetails
  , predicatesTransformations  :: IntMap PredicateTransformation
     -- ^ keyed by predicate requested

  , schemaInventory :: Inventory
  , schemaResolved :: [ResolvedSchemaRef]
  , schemaSource :: SourceSchemas
  , schemaMaxPid :: Pid
  , schemaLatestVersion :: Version
  }

-- | Data required to transform a predicate that was requested in a query into
-- one that we have available in the database, and then to transform the facts
-- found back into the requested predicate.
data PredicateTransformation = PredicateTransformation
  { tRequested :: PredicateDetails
    -- ^ the predicate that was queried for
  , tAvailable :: PredicateDetails
    -- ^ the predicate we have in the database
  , tTransformFactBack :: Thrift.Fact -> Thrift.Fact
    -- ^ available -> requested
    -- ^ transform a fact of the available predicate into a fact
    -- of the requested predicate.
  }

data TypeDetails = TypeDetails
  { typeId :: TypeId
  , typeRef :: TypeRef
  , typeType :: Type
  }

data PredicateDetails = PredicateDetails
  { predicatePid :: Pid
  , predicateId :: PredicateId
  , predicateRef :: PredicateRef
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

data SchemaVersion
  = LatestSchemaAll
  | SpecificSchemaAll Version

allSchemaVersion :: DbSchema -> SchemaVersion -> Version
allSchemaVersion _ (SpecificSchemaAll v) = v
allSchemaVersion dbSchema LatestSchemaAll = schemaLatestVersion dbSchema

schemaNameEnv :: DbSchema -> SchemaVersion -> Maybe (NameEnv RefTargetId)
schemaNameEnv dbSchema schemaVer =
  IntMap.lookup (fromIntegral (allSchemaVersion dbSchema schemaVer))
     (schemaEnvs dbSchema)

addTmpPredicate :: NameEnv RefTargetId -> NameEnv RefTargetId
addTmpPredicate =
  HashMap.insert tmp (Set.singleton (RefPred tempPredicateId))
  where tmp = SourceRef (predicateIdName tempPredicateId) (Just 0)

lookupSourceRef
  :: SourceRef
  -> SchemaVersion
     -- ^ schema version to use if predicate version is Nothing
  -> DbSchema
  -> LookupResult RefTargetId
lookupSourceRef ref schemaVer dbSchema =
  case schemaNameEnv dbSchema schemaVer of
    Nothing -> OutOfScope
    Just env -> resolveRef env ref

lookupPredicateSourceRef
  :: SourceRef
  -> SchemaVersion
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

tempPredicateId :: PredicateId
tempPredicateId = PredicateId "_tmp_" hash0

tempPid :: DbSchema -> Pid
tempPid = succ . schemaMaxPid

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

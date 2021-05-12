module Glean.Database.Schema.Types
  ( DbSchema(..)
  , PredicateDetails(..)
  , SchemaVersion(..)
  , lookupPredicate
  , lookupPredicateRef
  , lookupPid
  , TypeDetails(..)
  , lookupType
  , lookupTypeRef
  , dbSchemaRtsType
  , mkRtsType
  , tempPredicateRef
  , tempPid
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Glean.Angle.Types as Schema hiding (Type, FieldDef)
import qualified Glean.Angle.Types as Schema
import Glean.Query.Typecheck.Types
import Glean.RTS.Foreign.Bytecode (Subroutine)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Typecheck
import Glean.RTS.Traverse
import Glean.RTS.Types (Pid(..), Type, PidRef(..), FieldDef, ExpandedType(..))
import Glean.Schema.Resolve

-- | The Schema used by a DB
data DbSchema = DbSchema
  { predicatesByRef :: HashMap PredicateRef PredicateDetails
  , predicatesByName :: IntMap (HashMap Name PredicateDetails)
     -- ^ points to the predicate for each name in schema "all"
  , predicatesById :: IntMap PredicateDetails
  , schemaTypesByRef :: HashMap TypeRef TypeDetails
  , schemaTypesByName :: IntMap (HashMap Name TypeDetails)
     -- ^ points to the type for each name in schema "all"
  , schemaInventory :: Inventory
  , schemaSpec :: Schemas
  , schemaSource :: SourceSchemas
  , schemaMaxPid :: Pid
  , schemaLatestVersion :: Version
  }

data TypeDetails = TypeDetails
  { typeRef :: TypeRef
  , typeType :: Type
  }

data PredicateDetails = PredicateDetails
  { predicatePid :: Pid
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

lookupPredicate
  :: SourceRef
  -> SchemaVersion
     -- ^ schema version to use if predicate version is Nothing
  -> DbSchema
  -> Maybe PredicateDetails
lookupPredicate (SourceRef name (Just ver)) _ dbSchema =
  lookupPredicateRef (PredicateRef name ver) dbSchema
lookupPredicate (SourceRef name Nothing) schemaVer dbSchema =
  HashMap.lookup name =<<
    IntMap.lookup (fromIntegral (allSchemaVersion dbSchema schemaVer))
      (predicatesByName dbSchema)

lookupPredicateRef :: PredicateRef -> DbSchema -> Maybe PredicateDetails
lookupPredicateRef ref = HashMap.lookup ref . predicatesByRef

lookupPid :: Pid -> DbSchema -> Maybe PredicateDetails
lookupPid (Pid pid) = IntMap.lookup (fromIntegral pid) . predicatesById

lookupType :: SourceRef -> SchemaVersion -> DbSchema -> Maybe TypeDetails
lookupType (SourceRef name (Just v)) _ dbSchema =
  lookupTypeRef (TypeRef name v) dbSchema
lookupType (SourceRef name Nothing) schemaVer dbSchema =
  HashMap.lookup name =<<
    IntMap.lookup (fromIntegral (allSchemaVersion dbSchema schemaVer))
      (schemaTypesByName dbSchema)

lookupTypeRef :: TypeRef -> DbSchema -> Maybe TypeDetails
lookupTypeRef ref  = HashMap.lookup ref . schemaTypesByRef

tempPredicateRef :: PredicateRef
tempPredicateRef = PredicateRef "_tmp_" 0

tempPid :: DbSchema -> Pid
tempPid = succ . schemaMaxPid

-- | Convert Schema types to RTS types using a DbSchema
dbSchemaRtsType :: DbSchema -> Schema.Type -> Maybe Type
dbSchemaRtsType dbSchema = mkRtsType lookupType lookupPid
  where
  lookupType ref = typeType <$> lookupTypeRef ref dbSchema
  lookupPid ref = case lookupPredicateRef ref dbSchema of
    Just pid -> Just $ predicatePid pid
    -- detect temporary predicates that might have been serialized
    -- see Glean.Query.Flatten.captureKey
    _ | tempPredicateRef == ref -> Just $ tempPid dbSchema
    _ -> Nothing

-- | Convert from Schema Types to RTS Types. This involves
-- 1. PredicateRef -> PidRef
-- 2. Expand NamedTypes
mkRtsType
  :: (TypeRef -> Maybe Type)
  -> (PredicateRef -> Maybe Pid)
  -> Schema.Type -> Maybe Type
mkRtsType lookupType lookupPid = rtsType
  where
    rtsType :: Schema.Type -> Maybe Type
    rtsType Schema.Byte = return Schema.Byte
    rtsType Schema.Nat = return Schema.Nat
    rtsType (Schema.Array elty) = Schema.Array <$> rtsType elty
    rtsType (Schema.Record fields) = Schema.Record <$> mapM fieldType fields
    rtsType (Schema.Sum fields) = Schema.Sum <$> mapM fieldType fields
    rtsType Schema.String = return Schema.String
    rtsType (Schema.Predicate ref) = do
      pid <- lookupPid ref
      return (Schema.Predicate (PidRef pid ref))
    -- TODO: This will loop if we have recursive typedefs but we don't allow
    -- those at the moment.
    rtsType (Schema.NamedType ref) =
      Schema.NamedType . ExpandedType ref <$> lookupType ref
    rtsType (Schema.Maybe eltTy) = Schema.Maybe <$> rtsType eltTy
    rtsType (Schema.Enumerated names) = return (Schema.Enumerated names)
    rtsType Schema.Boolean = return Schema.Boolean

    fieldType :: Schema.FieldDef -> Maybe FieldDef
    fieldType (Schema.FieldDef name ty) = Schema.FieldDef name <$> rtsType ty

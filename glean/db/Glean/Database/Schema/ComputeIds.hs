{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Schema.ComputeIds
  ( computeIds
  , RefToIdEnv(..)
  , emptyRefToIdEnv
  , ResolvedSchemaId
  , refsToIds
  , HashedSchema(..)
  , emptyHashedSchema
  , RefTargetId
  ) where

import Control.Monad
import Control.Monad.State as State
import Data.Bifunctor
import Data.Bifoldable
import Data.Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Glean.Angle.Hash
import Glean.Angle.Types as Schema
import Glean.Schema.Util (showRef)
import Glean.Schema.Types
import Glean.Types

-- | The schema with all predicate/type references replaced with
-- PredicateId/TypeId.
data HashedSchema = HashedSchema
  { hashedTypes :: HashMap TypeId TypeDef
  , hashedPreds :: HashMap PredicateId PredicateDef
  , schemaRefToIdEnv :: RefToIdEnv
  , hashedSchemaEnv :: NameEnv RefTargetId
  , hashedSchemaAllVersion :: Version
  , hashedSchemaId :: SchemaId
  }

type RefTargetId = RefTarget PredicateId TypeId

emptyHashedSchema :: HashedSchema
emptyHashedSchema = HashedSchema
  { hashedTypes = HashMap.empty
  , hashedPreds =  HashMap.empty
  , schemaRefToIdEnv = emptyRefToIdEnv
  , hashedSchemaEnv = HashMap.empty
  , hashedSchemaAllVersion = 0
  , hashedSchemaId = SchemaId ""
  }

type Def_ p t = RefTarget p t

type DefResolved = Def_ ResolvedPredicateDef ResolvedTypeDef
type Def = Def_ PredicateDef TypeDef

data RefToIdEnv = RefToIdEnv
  { typeRefToId :: HashMap TypeRef TypeId
  , predRefToId :: HashMap PredicateRef PredicateId
  }

emptyRefToIdEnv :: RefToIdEnv
emptyRefToIdEnv = RefToIdEnv HashMap.empty HashMap.empty

type ResolvedSchemaId = ResolvedSchema PredicateId TypeId

refsToIds
  :: Bifunctor f
  => RefToIdEnv
  -> f PredicateRef TypeRef
  -> f PredicateId TypeId
refsToIds (RefToIdEnv tids pids) =
  bimap (lookupPredicateId pids) (lookupTypeId tids)
  where
    lookupPredicateId m p = HashMap.findWithDefault err p m
      where
        err = error $ "lookupPredicateId: " <> Text.unpack (showRef p)
    lookupTypeId m p = HashMap.findWithDefault err p m
      where
        err = error $ "lookupTypeId: " <> Text.unpack (showRef p)

attachDerivations
  :: [ResolvedSchemaRef]
  -> [HashMap PredicateRef ResolvedPredicateDef]
attachDerivations schemas =
  map (HashMap.mapWithKey attach . resolvedSchemaPredicates) schemas
  where
  allDerivings = HashMap.unions (map resolvedSchemaDeriving schemas)

  attach ref def = case HashMap.lookup ref allDerivings of
    Just drv | not (isDefaultDeriving drv) -> def { predicateDefDeriving = drv }
      -- see Note [overriding default deriving]
    _ -> def

isDefaultDeriving :: DerivingInfo q -> Bool
isDefaultDeriving (Derive DeriveIfEmpty _) = True
isDefaultDeriving _ = False

-- | Compute the PredicateId / TypeId for each definition, and substitute for
-- PredicateRef/TypeRef with PredicateId/TypeId inside all the definitions

computeIds
  :: (forall k v . HashMap k v -> [(k,v)])
  -> [ResolvedSchemaRef]
  -> Maybe (SchemaId, Version)
  -> HashedSchema
computeIds toList schemas versions = flip evalState emptyRefToIdEnv $ do
  let
    preds = attachDerivations schemas

    -- first topologically sort the predicates and types, so that we can
    -- process them in dependency order.
    edges :: [(DefResolved, RefResolved, [RefResolved])]
    edges =
      [ (RefPred def, RefPred (predicateDefRef def), predicateDefRefs def)
      | m <- preds
      , (_, def) <- toList m
      ] ++
      [ (RefType def, RefType (typeDefRef def), typeDefRefs def)
      | m <- map resolvedSchemaTypes schemas
      , (_, def) <- toList m
      ]

    collectRefs = bifoldMap ((:[]) . RefPred) ((:[]) . RefType)

    predicateDefRefs def =
      collectRefs (predicateDefKeyType def) <>
      collectRefs (predicateDefValueType def) <>
      foldMap collectRefs (predicateDefDeriving def)

    typeDefRefs def = collectRefs (typeDefType def)

  defs <- fmap concat $ forM (stronglyConnComp edges) $ \comp ->
    case comp of
      AcyclicSCC def -> do
        resolved <- resolveDef def
        let newdef = updateDefWithHash (fingerprintDef resolved) resolved
        State.modify (extend newdef)
        return [newdef]
      CyclicSCC defs -> do
        -- sort the definitions in the cycle by ref, so that the next
        -- steps produce deterministic hashes.
        let sorted = sortOn (bimap predicateDefRef typeDefRef) defs
        -- first map every ref in the cycle to the Id hash0
        defs1 <-
          withState (extends (zip defs (repeat hash0))) $
            mapM resolveDef sorted
        -- Compute a hash for each def
        let hashes = map fingerprintDef defs1
        -- Next make a hash of the whole cycle.
        let cycleHash = hashBinary hashes
        -- Note that it's possible to have two predicates with the
        -- same name (different versions) in the cycle, and we
        -- definitely want them to end up with different hashes
        -- (unless they have identical representations) so the hash of
        -- each declaration in the cycle is the hash of the
        -- declaration plus the hash of the whole cycle.
        let hashes2 =
              [ (def, hashBinary (hash1, cycleHash))
              | (def, hash1) <- zip sorted hashes ]
        State.modify (extends hashes2)
        -- now resolve the defs again with the correct Ids
        forM hashes2 $ \(def, hash) ->
          updateDefWithHash hash <$> resolveDef def

  env <- State.get

  let preds = HashMap.fromList
        [ (predicateDefRef def, def) | RefPred def <- defs]

      -- see Note [overriding default deriving]
      attachDefaultDerivings preds = foldr attach preds
        [ (id, drv)
        | schema <- schemas
        , (ref, drv) <- toList (resolvedSchemaDeriving schema)
        , isDefaultDeriving drv
        , Just id <- [HashMap.lookup ref (predRefToId env)]
        ]
        where
        attach (id,drv) = HashMap.adjust f id
          where
          f (PredicateDef id key val _ s) =
            PredicateDef id key val (fmap (refsToIds env) drv) s

      (allVersion, schemaId, schemaEnv) = makeSchemaEnv schemas versions env

  return HashedSchema {
      hashedTypes = HashMap.fromList
        [ (typeDefRef def, def) | RefType def <- defs],
      hashedPreds = attachDefaultDerivings preds,
      schemaRefToIdEnv = env,
      hashedSchemaEnv = schemaEnv,
      hashedSchemaAllVersion = allVersion,
      hashedSchemaId = schemaId
    }

  where
    updateDefWithHash :: Hash -> Def -> Def
    updateDefWithHash hash def = case def of
        RefType (TypeDef (TypeId ref _) ty) ->
          RefType (TypeDef (TypeId ref hash) ty)
        RefPred (PredicateDef (PredicateId ref _) key val drv s) ->
          RefPred (PredicateDef (PredicateId ref hash) key val drv s)

    extend :: Def -> RefToIdEnv -> RefToIdEnv
    extend def (RefToIdEnv tids pids) =
      case def of
        RefType (TypeDef id _) ->
          RefToIdEnv (HashMap.insert (typeIdRef id) id tids) pids
        RefPred (PredicateDef id _ _ _ _) ->
          RefToIdEnv tids (HashMap.insert (predicateIdRef id) id pids)

    extends :: [(DefResolved, Hash)] -> RefToIdEnv -> RefToIdEnv
    extends list env = foldr one env list
      where
      one (def, hash) (RefToIdEnv tids pids) =
        case def of
          RefType (TypeDef ref _) ->
            let id = TypeId ref hash in
            RefToIdEnv (HashMap.insert ref id tids) pids
          RefPred (PredicateDef ref _ _ _ _) ->
            let id = PredicateId ref hash in
            RefToIdEnv tids (HashMap.insert ref id pids)

    resolveDef :: DefResolved -> State RefToIdEnv Def
    resolveDef (RefType (TypeDef ref ty)) = do
      env <- State.get
      let
        newTy = refsToIds env ty
        newId = TypeId {
          typeIdRef = ref,
          typeIdHash = hash0 }
        def = RefType (TypeDef newId newTy)
      return def
    resolveDef (RefPred (PredicateDef ref key val drv s)) = do
      env <- State.get
      let
        newKey = refsToIds env key
        newVal = refsToIds env val
        newDrv = fmap (resolveQuery env) drv
        newId = PredicateId {
          predicateIdRef = ref,
          predicateIdHash = hash0 }
        def = RefPred (PredicateDef newId newKey newVal newDrv s)
      return def

resolveQuery
  :: RefToIdEnv
  -> Query_ PredicateRef TypeRef
  -> Query_ PredicateId TypeId
resolveQuery env (SourceQuery head stmts ord) =
  SourceQuery (refsToIds env <$> head) (refsToIds env <$> stmts) ord

-- Serialize a definition to produce its hash.  Note that the hash
-- includes the PredicateRef/TypeRef: two predicates or types are the
-- same only if they the same name, same version, and same
-- representation.
fingerprintDef :: Def -> Hash
fingerprintDef (RefType (TypeDef id ty)) =
  hashBinary (showRef ref, ty)
  where ref = typeIdRef id
fingerprintDef (RefPred (PredicateDef id key val drv _)) =
  hashBinary (showRef ref, key, val, fmap rmLocQuery drv)
  where ref = predicateIdRef id

{- Note [overriding default deriving]

If we add a "deriving P default" declaration for a predicate P to the
schema, after a DB was created with P, we would like this to work: the
new P should get the same Pid, so queries still work.

But, if the derivation is included in the hash, the new P with the
deriving decl will get a different hash. It's difficult to resolve
this later, because we only want the derivation to take effect if the
two Ps are really the same (they have the same types).

So the hack to solve this for now is to ignore the "derive default"
derivations when computing hashes, and add them back in
afterwards. The old and the new Ps will get the same hashes, if they
really only differ in their derivations.

-}


makeSchemaEnv
  :: [ResolvedSchemaRef]
  -> Maybe (SchemaId, Version)
  -> RefToIdEnv
  -> (Version, SchemaId, NameEnv RefTargetId)
makeSchemaEnv resolved versions refToIdEnv
  | Just (schemaId, ver) <- versions,
    Just schema <- Map.lookup ver resolvedAlls =
    (ver, schemaId, mkEnv schema)
  | Just (ver, schema) <- Map.lookupMax resolvedAlls =
    let
      env = mkEnv schema
      schemaId = SchemaId $ Text.pack $ show $ hashNameEnv env
    in
    (fromIntegral ver, schemaId, env)
  | otherwise =
    error "no \"all\" schema"
  where
    mkEnv ResolvedSchema{..} =
       HashMap.union
         (mapNameEnv (Just . refsToIds refToIdEnv)
           resolvedSchemaQualScope)
         versionedNameEnv

    resolvedAlls = Map.fromList
      [ (resolvedSchemaVersion schema, schema)
      | schema <- resolved
      , resolvedSchemaName schema == "all" ]

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

-- | How to take a NameEnv and compute the schema hash from it. This
-- hash uniquely identifies a particular NameEnv that can be used to
-- resolve a type or predicate name, or in general a query.
hashNameEnv :: NameEnv (RefTarget PredicateId TypeId) -> Hash
hashNameEnv env =
  hashBinary (sort (HashMap.toList env))

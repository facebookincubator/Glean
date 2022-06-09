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
  ) where

import Control.Monad
import Control.Monad.State as State
import Data.Bifunctor
import Data.Bifoldable
import Data.Binary as Binary
import Data.Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.Text as Text

import Glean.Angle.Hash
import Glean.Angle.Types as Schema
import Glean.Schema.Util (showRef)
import Glean.Schema.Types

-- | The schema with all predicate/type references replaced with
-- PredicateId/TypeId.
data HashedSchema = HashedSchema
  { hashedTypes :: HashMap TypeId TypeDef
  , hashedPreds :: HashMap PredicateId PredicateDef
  , schemaRefToIdEnv :: RefToIdEnv
  }

type Def_ p t = RefTarget p t

type DefResolved = Def_ ResolvedPredicateDef ResolvedTypeDef
type Def = Def_ PredicateDef TypeDef

data RefToIdEnv = RefToIdEnv
  { typeRefToId :: HashMap TypeRef TypeId
  , predRefToId :: HashMap PredicateRef PredicateId
  }

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

emptyRefToIdEnv :: RefToIdEnv
emptyRefToIdEnv = RefToIdEnv HashMap.empty HashMap.empty

attachDerivations
  :: [ResolvedSchemaRef]
  -> [HashMap PredicateRef ResolvedPredicateDef]
attachDerivations schemas =
  map (HashMap.mapWithKey attach . resolvedSchemaPredicates) schemas
  where
  allDerivings = HashMap.unions (map resolvedSchemaDeriving schemas)

  attach ref def = case HashMap.lookup ref allDerivings of
    Just drv -> def { predicateDefDeriving = drv }
    Nothing -> def

-- | Compute the PredicateId / TypeId for each definition, and substitute for
-- PredicateRef/TypeRef with PredicateId/TypeId inside all the definitions

computeIds :: [ResolvedSchemaRef] -> HashedSchema
computeIds schemas = flip evalState emptyRefToIdEnv $ do
  let
    preds = attachDerivations schemas

    -- first topologically sort the predicates and types, so that we can
    -- process them in dependency order.
    edges :: [(DefResolved, RefResolved, [RefResolved])]
    edges =
      [ (RefPred def, RefPred (predicateDefRef def), predicateDefRefs def)
      | m <- preds
      , def <- HashMap.elems m
      ] ++
      [ (RefType def, RefType (typeDefRef def), typeDefRefs def)
      | m <- map resolvedSchemaTypes schemas
      , def <- HashMap.elems m
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
        let cycleHash = hashByteString (Binary.encode hashes)
        -- Note that it's possible to have two predicates with the
        -- same name (different versions) in the cycle, and we
        -- definitely want them to end up with different hashes
        -- (unless they have identical representations) so the hash of
        -- each declaration in the cycle is the hash of the
        -- declaration plus the hash of the whole cycle.
        let hashes2 =
              [ (def, hashByteString (Binary.encode (hash1, cycleHash)))
              | (def, hash1) <- zip sorted hashes ]
        State.modify (extends hashes2)
        -- now resolve the defs again with the correct Ids
        forM hashes2 $ \(def, hash) ->
          updateDefWithHash hash <$> resolveDef def

  env <- State.get
  return HashedSchema {
      hashedTypes = HashMap.fromList
        [ (typeDefRef def, def) | RefType def <- defs],
      hashedPreds = HashMap.fromList
        [ (predicateDefRef def, def) | RefPred def <- defs],
      schemaRefToIdEnv = env
    }

  where
    updateDefWithHash :: Hash -> Def -> Def
    updateDefWithHash hash def = case def of
        RefType (TypeDef (TypeId ref _) ty) ->
          RefType (TypeDef (TypeId ref hash) ty)
        RefPred (PredicateDef (PredicateId ref _) key val drv) ->
          RefPred (PredicateDef (PredicateId ref hash) key val drv)

    extend :: Def -> RefToIdEnv -> RefToIdEnv
    extend def (RefToIdEnv tids pids) =
      case def of
        RefType (TypeDef id _) ->
          RefToIdEnv (HashMap.insert (typeIdRef id) id tids) pids
        RefPred (PredicateDef id _ _ _) ->
          RefToIdEnv tids (HashMap.insert (predicateIdRef id) id pids)

    extends :: [(DefResolved, Hash)] -> RefToIdEnv -> RefToIdEnv
    extends list env = foldr one env list
      where
      one (def, hash) (RefToIdEnv tids pids) =
        case def of
          RefType (TypeDef ref _) ->
            let id = TypeId ref hash in
            RefToIdEnv (HashMap.insert ref id tids) pids
          RefPred (PredicateDef ref _ _ _) ->
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
    resolveDef (RefPred (PredicateDef ref key val drv)) = do
      env <- State.get
      let
        newKey = refsToIds env key
        newVal = refsToIds env val
        newDrv = fmap (resolveQuery env) drv
        newId = PredicateId {
          predicateIdRef = ref,
          predicateIdHash = hash0 }
        def = RefPred (PredicateDef newId newKey newVal newDrv)
      return def

resolveQuery
  :: RefToIdEnv
  -> Query_ PredicateRef TypeRef
  -> Query_ PredicateId TypeId
resolveQuery env (SourceQuery head stmts) =
  SourceQuery (refsToIds env <$> head) (refsToIds env <$> stmts)

-- Serialize a definition to produce its hash.
-- TODO: we're including the Ref in the serialization here
-- because we need to distinguish e.g. cxx1.Name.4 from
-- cxx1.Name.5 because those will have different Pids, even though
-- they have the same representation. When we start using
-- PredicateIds on the client side too, and assign Pids per
-- PredicateId, then we can remove showRef below.
fingerprintDef :: Def -> Hash
fingerprintDef (RefType (TypeDef id ty)) =
  hashByteString (Binary.encode (showRef ref, ty))
  where ref = typeIdRef id
fingerprintDef (RefPred (PredicateDef id key val drv)) =
  hashByteString (Binary.encode (showRef ref, key, val, fmap rmLocQuery drv))
  where ref = predicateIdRef id

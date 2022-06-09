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
  { hashedTypes :: HashMap TypeId (TypeRef, TypeDef)
  , hashedPreds :: HashMap PredicateId (PredicateRef, PredicateDef)
  , schemaRefToIdEnv :: RefToIdEnv
  }

type Def_ p t = RefTarget (PredicateRef, p) (TypeRef, t)

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
      [ (RefPred (ref, def),
          RefPred (predicateDefRef def), predicateDefRefs def)
      | m <- preds
      , (ref, def) <- HashMap.toList m
      ] ++
      [ (RefType (ref, def),
          RefType (typeDefRef def), typeDefRefs def)
      | m <- map resolvedSchemaTypes schemas
      , (ref, def) <- HashMap.toList m
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
        newdef <- resolveDef fingerprintDef def
        State.modify (extend newdef)
        return [newdef]
      CyclicSCC defs -> do
        -- sort the definitions in the cycle by ref, so that the next
        -- steps produce deterministic hashes.
        let sorted = sortOn (bimap fst fst) defs
        -- first map every ref in the cycle to the Id hash0
        defs1 <-
          withState (extends (zip defs (repeat hash0))) $
            mapM (resolveDef (const hash0)) sorted
        -- Compute a hash for each def
        let hashes = map fingerprintDef defs1
        -- Next make a hash of the whole cycle.
        let cycleHash = hashString (concatMap show hashes)
        -- Note that it's possible to have two predicates with the
        -- same name (different versions) in the cycle, and we
        -- definitely want them to end up with different hashes
        -- (unless they have identical representations) so the hash of
        -- each declaration in the cycle is the hash of the
        -- declaration plus the hash of the whole cycle.
        let hashes2 =
              [ (def, hashString (show hash1 <> show cycleHash))
              | (def, hash1) <- zip sorted hashes ]
        State.modify (extends hashes2)
        -- now resolve the defs again with the correct Ids
        forM hashes2 $ \(def, hash) -> resolveDef (const hash) def

  env <- State.get
  return HashedSchema {
      hashedTypes = HashMap.fromList
        [ (typeDefRef def, (ref, def))
        | RefType (ref, def) <- defs],
      hashedPreds = HashMap.fromList
        [ (predicateDefRef def, (ref, def))
        | RefPred (ref, def) <- defs],
      schemaRefToIdEnv = env
    }

  where
    extend :: Def -> RefToIdEnv -> RefToIdEnv
    extend def (RefToIdEnv tids pids) =
      case def of
        RefType (ref, TypeDef id _) ->
          RefToIdEnv (HashMap.insert ref id tids) pids
        RefPred (ref, PredicateDef id _ _ _) ->
          RefToIdEnv tids (HashMap.insert ref id pids)

    extends :: [(DefResolved, Hash)] -> RefToIdEnv -> RefToIdEnv
    extends list env = foldr one env list
      where
      one (def, hash) (RefToIdEnv tids pids) =
        case def of
          RefType (ref, _) ->
            let id = TypeId (typeRef_name ref) hash in
            RefToIdEnv (HashMap.insert ref id tids) pids
          RefPred (ref, _) ->
            let id = PredicateId (predicateRef_name ref) hash in
            RefToIdEnv tids (HashMap.insert ref id pids)

    resolveDef :: (Def -> Hash) -> DefResolved -> State RefToIdEnv Def
    resolveDef hashDef (RefType (ref, TypeDef _ ty)) = do
      env <- State.get
      let
        newTy = refsToIds env ty
        newId = TypeId {
          typeIdName = typeRef_name ref,
          typeIdHash = hashDef def }
          -- NB. hashDef shouldn't look at the Id, or it will loop
        def = RefType (ref, TypeDef newId newTy)
      return def
    resolveDef hashDef (RefPred (ref, PredicateDef _ key val drv)) = do
      env <- State.get
      let
        newKey = refsToIds env key
        newVal = refsToIds env val
        newDrv = fmap (resolveQuery env) drv
        newId = PredicateId {
          predicateIdName = predicateRef_name ref,
          predicateIdHash = hashDef def }
        def = RefPred (ref, PredicateDef newId newKey newVal newDrv)
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
fingerprintDef (RefType (ref, TypeDef _ ty)) =
  hashByteString (Binary.encode (showRef ref, ty))
fingerprintDef (RefPred (ref, PredicateDef _ key val drv)) =
  hashByteString (Binary.encode (showRef ref, key, val, fmap rmLocQuery drv))

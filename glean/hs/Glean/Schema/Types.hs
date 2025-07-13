{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


{-# LANGUAGE DeriveFunctor #-}
module Glean.Schema.Types (
  -- * Schema references
  SchemaRef(..), showSchemaRef,

  -- * Name targets
  RefTarget(..), RefResolved,

  -- * Resolved abstract syntax
  ResolvedPat,
  ResolvedType,
  ResolvedFieldDef,
  ResolvedTypeDef,
  ResolvedPredicateDef,
  ResolvedDerivingDef,
  ResolvedQuery,
  ResolvedStatement,
  ResolvedDeriving,

  ResolvedPat',
  ResolvedStatement',
  ResolvedQuery',
  ResolvedDeriving',
  ResolvedTypeDef',
  ResolvedType',
  ResolvedFieldDef',

  -- * Name environments and resolution
  NameEnv,
  mapNameEnv,
  LookupResult(..),
  resolveRef,
  resolveRefFiltered,
  lookupResultToEither,

  -- * Resolved schemas
  ResolvedSchemas(..),
  ResolvedSchema(..),
  ResolvedSchemaRef,
  schemaRef,
) where

import Data.Bifunctor
import Data.Binary
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Glean.Angle.Types
import Glean.Schema.Util

-- | Identify a schema
data SchemaRef = SchemaRef Name Version
  deriving (Eq, Ord, Show, Generic)

showSchemaRef :: SchemaRef -> Text
showSchemaRef (SchemaRef name version) =
  name <> "." <> Text.pack (show version)

instance Hashable SchemaRef

-- | The target of a reference
data RefTarget p t = RefPred p | RefType t
  deriving (Eq, Ord, Show, Generic, Functor)

instance (Binary p, Binary t) => Binary (RefTarget p t)

instance Bifunctor RefTarget where
  bimap f _ (RefPred p) = RefPred (f p)
  bimap _ g (RefType t) = RefType (g t)

type RefResolved = RefTarget PredicateRef TypeRef

showRefTarget :: (ShowRef t, ShowRef p) => RefTarget p t -> Text
showRefTarget (RefType t) = showRef t
showRefTarget (RefPred p) = showRef p

-- | An environment that describes which names are in scope.
type NameEnv t = HashMap SourceRef (Set t)

mapNameEnv :: Ord b => (a -> Maybe b) -> NameEnv a -> NameEnv b
mapNameEnv f = fmap g
  where
  g set = Set.fromList [ b | a <- Set.toList set, Just b <- [f a] ]

data LookupResult t
  = OutOfScope
  | Ambiguous [t]
  | ResolvesTo t

lookupResultToEither
  :: (ShowRef t, ShowRef p)
  => SourceRef
  -> LookupResult (RefTarget p t)
  -> Either Text (RefTarget p t)
lookupResultToEither ref OutOfScope =
  Left $ "not in scope: " <> showRef ref
lookupResultToEither ref (Ambiguous targets) =
  Left $ showRef ref <> " is ambiguous. It could refer to: " <>
      Text.intercalate ", " (map showRefTarget targets)
lookupResultToEither _ (ResolvesTo target) = Right target

-- | Resolve a name (@SourceRef@) with respect to a scope (@NameEnv@)
resolveRef :: NameEnv t -> SourceRef -> LookupResult t
resolveRef scope ref = resolveRefFiltered scope (const True) ref

resolveRefFiltered :: NameEnv t -> (t -> Bool) -> SourceRef -> LookupResult t
resolveRefFiltered scope p ref =
  case HashMap.lookup ref scope of
    Nothing -> OutOfScope
    Just set -> case filter p $ Set.toList set of
      [] -> OutOfScope
      [one] -> ResolvesTo one
      many -> Ambiguous many

-- Resolved abstract syntax

type ResolvedType = Type_ SrcSpan PredicateRef TypeRef
type ResolvedFieldDef = FieldDef_ SrcSpan PredicateRef TypeRef
type ResolvedTypeDef = TypeDef_ SrcSpan PredicateRef TypeRef
type ResolvedPredicateDef = PredicateDef_ SrcSpan SrcSpan PredicateRef TypeRef
type ResolvedDerivingDef = DerivingDef_ SrcSpan SrcSpan PredicateRef TypeRef
type ResolvedPat = SourcePat_ SrcSpan SrcSpan PredicateRef TypeRef
type ResolvedStatement = SourceStatement_ SrcSpan SrcSpan PredicateRef TypeRef
type ResolvedQuery = SourceQuery_ SrcSpan SrcSpan PredicateRef TypeRef
type ResolvedDeriving = DerivingInfo ResolvedQuery

-- Versions of the above types abstracted over the source spans
type ResolvedPat' s = SourcePat_ s s PredicateRef TypeRef
type ResolvedStatement' s = SourceStatement_ s s PredicateRef TypeRef
type ResolvedQuery' s = SourceQuery_ s s PredicateRef TypeRef
type ResolvedDeriving' s = DerivingInfo (ResolvedQuery' s)
type ResolvedTypeDef' s = TypeDef_ s PredicateRef TypeRef
type ResolvedType' s = Type_ s PredicateRef TypeRef
type ResolvedFieldDef' s = FieldDef_ s PredicateRef TypeRef
-- | A 'ResolvedSchema' is used during schema resolution to resolve
-- schemas that import or inherit from this schema.
data ResolvedSchema p t = ResolvedSchema
  { resolvedSchemaName :: Name
  , resolvedSchemaVersion :: Version
  , resolvedSchemaAngleVersion :: AngleVersion
  , resolvedSchemaTypes :: HashMap TypeRef (TypeDef_ SrcSpan p t )
    -- ^ types that are defined by this schema
  , resolvedSchemaReExportedTypes :: HashMap TypeRef (TypeDef_ SrcSpan p t )
    -- ^ types that are inherited and re-exported by this schema
  , resolvedSchemaPredicates ::
      HashMap PredicateRef (PredicateDef_ SrcSpan SrcSpan p t)
    -- ^ predicates that are defined by this schema
  , resolvedSchemaReExportedPredicates ::
      HashMap PredicateRef (PredicateDef_ SrcSpan SrcSpan p t)
    -- ^ predicates that are inherited and re-exported by this schema
  , resolvedSchemaUnqualScope :: NameEnv (RefTarget p t)
    -- ^ The scope exposed by this schema, unqualified. This will be
    -- used when the schema is inherited.
  , resolvedSchemaQualScope :: NameEnv (RefTarget p t)
    -- ^ The scope exposed by this schema, qualified. This will be
    -- used when the schema is inherited or imported.
  , resolvedSchemaDeriving ::
      HashMap PredicateRef (DerivingDef_ SrcSpan SrcSpan p t)
    -- ^ deriving declarations, for predicates defined in this schema
    -- or an inherited schema.
  , resolvedSchemaEvolves :: Set SchemaRef
    -- ^ schemas evolves by this schema.
  }

type ResolvedSchemaRef = ResolvedSchema PredicateRef TypeRef

schemaRef :: ResolvedSchema p t -> SchemaRef
schemaRef ResolvedSchema{..} =
  SchemaRef resolvedSchemaName resolvedSchemaVersion

-- | A set of schemas after name resolution
data ResolvedSchemas = ResolvedSchemas
  { schemasHighestVersion :: Maybe Version
  , schemasResolved :: [ResolvedSchemaRef]
    -- ^ Resolved schemas in dependency order
  }

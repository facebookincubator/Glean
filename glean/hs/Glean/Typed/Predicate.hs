-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}

-- | Support for working with Thrift-generated predicate types
module Glean.Typed.Predicate
  ( -- * class Predicate
    Predicate(..)
  , justId
  , predicateRef
    -- * Lookup PidOf
  , HasPredicates(..)
    -- ** exception
  , MissingPredicate(..), throwMissingPredicate
    -- * Vector Predicates
  , Predicates, SchemaPredicates, makePredicates
    -- ** Sum types
  , SumBranches(..)
  ) where

import Control.Exception
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Proxy
import Data.Text (Text)
import Data.Vector.Primitive (Vector)
import qualified Data.Vector.Primitive as Vector
import TextShow

import Glean.RTS.Types (invalidPid)
import Glean.Types (PredicateRef(..), SchemaInfo(..))
import Glean.Typed.Binary
import Glean.Typed.Id

-- -----------------------------------------------------------------------------

-- | 'Predicate' provides information about the schema's data structures,
-- and convenience functions.
--
-- * Types
--     * @'KeyType' Foo@ : the generated type (often @Foo_key*) that
--       stores the predicate's fields.
--     * @'ValueType' Foo@ : the generated values type, defaults to @()@.
-- * Methods
--     * @'getName' Foo@ : the @glean.thrift@ derived 'PredicateRef' holding
--       the Text name and Int version of the predicate.
--     * @'getIndex' Foo@ : internal use, index into all schema predicate list.
--       Used to efficiently lookup @'T.PidOf' p@ for a particular repo.
--
-- Superclass constraint of 'BT.Type' ensures this can be serialized into
-- the underlying binary format.

class (Type (KeyType p), Type (ValueType p)) => Predicate p where
  type KeyType p
  type ValueType p
  type ValueType p = ()
  getName :: proxy p -> PredicateRef
  getIndex :: proxy p -> PredicateIndex
  mkFact :: IdOf p -> Maybe (KeyType p) -> Maybe (ValueType p) -> p
  getId :: p -> IdOf p
  getFactKey :: p -> Maybe (KeyType p)
  getFactValue :: p -> Maybe (ValueType p)

justId :: Predicate p => IdOf p -> p
justId x = mkFact x Nothing Nothing

newtype MissingPredicate = MissingPredicate PredicateRef deriving(Show)
instance Exception MissingPredicate

-- | Useful error for 'getPid' in 'HasPredicates'
throwMissingPredicate :: forall p. Predicate p => PidOf p
throwMissingPredicate = throw $ MissingPredicate $ getName (Proxy :: Proxy p)

-- | An index into the Predicates vector. Used when writing facts only.
type PredicateIndex = Int

newtype Predicates = Predicates (Vector Pid)
  -- invalidPid for predicates missing from the schema

-- | Class of values 'a' that have definitions for any predicate 'p'
class HasPredicates a where
  getPid :: Predicate p => a -> PidOf p

instance HasPredicates Predicates where
  getPid = get
    where
      get :: forall p. Predicate p => Predicates -> PidOf p
      get (Predicates pids)
        | pid /= invalidPid = PidOf pid
        | otherwise = throw $ MissingPredicate $ getName (Proxy :: Proxy p)
        where
          pid = pids Vector.! getIndex (Proxy :: Proxy p)

-- | The type of @allPredicates@ generated for each schema. To get this,
-- import the generated module for the schema, e.g. @Glean.Schema.Src@.
type SchemaPredicates = [(PredicateRef, PredicateIndex)]

makePredicates :: [SchemaPredicates] -> SchemaInfo -> Predicates
makePredicates schemas info = Predicates $
  (Vector.//)
    (Vector.replicate (maxIx+1) invalidPid)
    [ (ix, HashMap.lookupDefault invalidPid ref ref_ids)
    | (ref, ix) <- concat schemas
    ]
  where
    maxIx = maximum $ map snd $ concat schemas
    ref_ids = HashMap.fromList
      [(ref, Pid id) | (id,ref) <- Map.toList $ schemaInfo_predicateIds info]

-- | Convert a predicate type to a versioned predicate name. This is
-- useful for building Angle queries that refer to specific versions.
predicateRef :: forall p proxy . Predicate p => proxy p -> Text
predicateRef _ = name <> "." <> showt version
  where !(PredicateRef name version) = getName (Proxy @p)

-- -----------------------------------------------------------------------------

-- | When the schema has a @sum([..])@ type, and all the branches have
-- distinct types, then 'SumBranches' lets us get the data constructor
-- for each branch or set the query field in a type-driven way.
--
-- 'injectBranch' : This is the constructor for this branch of the 'parent'

class SumBranches child parent where
  injectBranch :: child -> parent
  projectBranch :: parent -> Maybe child

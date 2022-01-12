{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Support for Thrift-generated query types.
--
module Glean.Typed.Query (
    QueryResult
  , QueryOf
  , PredicateQuery(..)
  , SumQuery(..)
  , ToQuery(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Glean.Typed.Id
import Glean.Typed.Predicate
import Glean.Types

-- | map a query type to its result type. For example,
-- 'Glean.Schema.Query.Src.File' maps to 'Glean.Schema.Src.File'.
--
-- Must be inverse of 'QueryOf'
type family QueryResult q = r | r -> q

-- | map a result type to its query type. For example,
-- 'Glean.Schema.Src.File' maps to 'Glean.Schema.Query.Src.File'.
--
-- Must be inverse of 'QueryResult'
type family QueryOf r = q | q -> r

-- | A predicate type that has a corresponding query type.
--
--     * @'toQueryId' x@ : Make a query that matches precisely the @x@ id. See
--       also 'injectQuery' which can take the @'QueryResult' p@.
--     * @'toQueryKey' x@ : Make a query that matches precisely the given key
--
class Predicate p => PredicateQuery p where
  toQueryId :: IdOf p -> QueryOf p
  toQueryKey :: KeyType p -> QueryOf p

-- | 'injectQuery' : Set this branch of the default 'parent' query. See
-- also 'toQueryId' which can make the @'QueryOf' child@.
class SumQuery child parent where
  injectQuery :: child -> parent

-- | Convert a result type to a query type. This is currently provided
-- for sum types and predicates, but not for record types or arrays.
class ToQuery r where
  toQuery :: r -> QueryOf r

  default toQuery :: PredicateQuery r => r -> QueryOf r
  toQuery p = case getFactKey p of
    Nothing -> toQueryId (getId p)
    Just k -> toQueryKey k

-- -----------------------------------------------------------------------------
-- Instances for base types

type instance QueryOf Nat = Nat

instance ToQuery Nat where
  toQuery = id

type instance QueryResult Byte = Byte

type instance QueryOf Byte = Byte

instance ToQuery Byte where
  toQuery = id

type instance QueryResult Bool = Bool

type instance QueryOf Bool = Bool

instance ToQuery Bool where
  toQuery n = n

type instance QueryResult Text = Text

type instance QueryOf Text = Text

instance ToQuery Text where
  toQuery t = t

type instance QueryResult ByteString = ByteString

type instance QueryOf ByteString = ByteString

instance ToQuery ByteString where
  toQuery t = t

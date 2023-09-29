{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
-- | A tagged intmap to keep the type of keys clear.  Mainly intended for
-- Glean predicates.
module Glean.Util.PredMap
  ( PredMap(..), makePredMap, fromList, fromDistinctAscList, fromListWith
  , lookup, findWithDefault, mapMaybe, alter, empty, insert, insertWith, elems
  , union
  , unionsWith
  , keys
  , toList, toAscList
  , size
  , member, notMember
  ) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Maybe (mapMaybe)
import Prelude hiding (lookup)

import Glean.Typed.Predicate
import Glean.Typed.Id

-- -----------------------------------------------------------------------------
-- Helpers, to make most of this module into one liners

one :: IdOf p -> Int
one = fromIntegral . fromFid . idOf

withKey :: (IdOf p, v) -> (Int, v)
withKey (p, v) = (one p, v)

-- -----------------------------------------------------------------------------

-- | I want some more type safety around IntMap. The 'p' will usually
-- be a Predicate type, e.g. (IdOf p) as the logical key.
newtype PredMap p v = PredMap { predMap :: IntMap v }
  deriving (Show, Semigroup, Monoid, Functor, Foldable, Traversable, NFData)

empty :: PredMap p v
empty = PredMap IntMap.empty

-- | Simplest way to make a 'PredMap' but not the only way.  Any 'p' without
-- a key are silently dropped.
makePredMap :: Predicate p => [p] -> PredMap p (KeyType p)
makePredMap = PredMap . IntMap.fromList . Data.Maybe.mapMaybe getWithKey
  where getWithKey p = (one (getId p),) <$> getFactKey p

fromList :: [(IdOf p, v)] -> PredMap p v
fromList = PredMap . IntMap.fromList . map withKey

fromDistinctAscList :: [(IdOf p, v)] -> PredMap p v
fromDistinctAscList = PredMap . IntMap.fromDistinctAscList . map withKey

fromListWith :: (v -> v -> v) -> [(IdOf p, v)] -> PredMap p v
fromListWith f = PredMap . IntMap.fromListWith f . map withKey

lookup :: Predicate p => IdOf p -> PredMap p v -> Maybe v
lookup p pm = IntMap.lookup (one p) (predMap pm)

findWithDefault :: Predicate p => v -> IdOf p -> PredMap p v -> v
findWithDefault d p = IntMap.findWithDefault d (one p) . predMap

mapMaybe :: Predicate p => (a -> Maybe b) -> PredMap p a -> PredMap p b
mapMaybe f = PredMap . IntMap.mapMaybe f . predMap

alter :: (Maybe v -> Maybe v) -> IdOf p -> PredMap p v -> PredMap p v
alter f p = PredMap . IntMap.alter f (one p) . predMap

insert :: IdOf p -> v -> PredMap p v -> PredMap p v
insert p v = PredMap . IntMap.insert (one p) v . predMap

insertWith :: (v -> v -> v) -> IdOf p -> v -> PredMap p v -> PredMap p v
insertWith f p v = PredMap . IntMap.insertWith f (one p) v . predMap

union :: PredMap p v -> PredMap p v -> PredMap p v
union a b = PredMap $ IntMap.union (predMap a) (predMap b)

unionsWith :: (v -> v -> v) -> [PredMap p v] -> PredMap p v
unionsWith f = PredMap . IntMap.unionsWith f . map predMap

keys :: PredMap p v -> [IdOf p]
keys = map (\k -> IdOf (Fid (fromIntegral k))) . IntMap.keys . predMap

elems :: PredMap p v -> [v]
elems = IntMap.elems . predMap

-- | Synonym of 'toAscList'
toList :: PredMap p v -> [(IdOf p, v)]
toList = toAscList

toAscList :: PredMap p v -> [(IdOf p, v)]
toAscList = map (\ (k, v) -> (IdOf (Fid (fromIntegral k)), v))
       . IntMap.toAscList . predMap

size :: PredMap p v -> Int
size = IntMap.size . predMap

member :: IdOf p -> PredMap p v -> Bool
member p pm = IntMap.member (one p) (predMap pm)

notMember :: IdOf p -> PredMap p v -> Bool
notMember p pm = IntMap.notMember (one p) (predMap pm)

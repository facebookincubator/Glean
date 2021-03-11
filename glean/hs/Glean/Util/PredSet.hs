{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A tagged intset to keep the type of keys clear.  Mainly intended for
-- Glean predicates.
module Glean.Util.PredSet
  ( PredSet(..), fromList, fromDistinctAscList
  , empty, insert, toList, toAscList, size
  , member, notMember
  , singleton, union, difference
    -- * PredSet to/from PredMap
  , keysSet, fromSet, fromSetM
  ) where

import Control.DeepSeq (NFData)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Prelude hiding (lookup)

import Glean.Typed.Id
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap

-- -----------------------------------------------------------------------------

{-# INLINE one #-}
one :: IdOf p -> Int
one = fromIntegral . fromFid . idOf

-- -----------------------------------------------------------------------------

-- | I want some more type safety around IntSet. The 'p' will usually
-- be a Predicate type, e.g. (IdOf p) as the logical key.
newtype PredSet p = PredSet { predSet :: IntSet }
  deriving (Show, Semigroup, Monoid, NFData)

fromList :: [IdOf p] -> PredSet p
fromList = PredSet . IntSet.fromList . map one

singleton :: IdOf p -> PredSet p
singleton = PredSet . IntSet.singleton . one

fromDistinctAscList :: [IdOf p] -> PredSet p
fromDistinctAscList = PredSet . IntSet.fromDistinctAscList . map one

empty :: PredSet p
empty = PredSet IntSet.empty

insert :: IdOf p -> PredSet p -> PredSet p
insert p = PredSet . IntSet.insert (one p) . predSet

-- | Synonym for 'toAscList'
toList :: PredSet p -> [IdOf p]
toList = toAscList

toAscList :: PredSet p -> [IdOf p]
toAscList = map (IdOf . Fid . fromIntegral) . IntSet.toAscList . predSet

size :: PredSet p -> Int
size = IntSet.size . predSet

member :: IdOf p -> PredSet p -> Bool
member k pm = IntSet.member (one k) (predSet pm)

notMember :: IdOf p -> PredSet p -> Bool
notMember k pm = IntSet.notMember (one k) (predSet pm)

union :: PredSet p -> PredSet p -> PredSet p
union x y = PredSet $ IntSet.union (predSet x) (predSet y)

difference :: PredSet p -> PredSet p -> PredSet p
difference x y = PredSet $ IntSet.difference (predSet x) (predSet y)

-- -----------------------------------------------------------------------------

keysSet :: PredMap p v -> PredSet p
keysSet = PredSet . IntMap.keysSet . PredMap.predMap

fromSet :: (IdOf p -> v) -> PredSet p -> PredMap p v
fromSet f =
  PredMap.PredMap . IntMap.fromSet (f . IdOf . Fid . fromIntegral) . predSet

fromSetM :: Monad m => (IdOf p -> m v) -> PredSet p -> m (PredMap p v)
fromSetM f ps = do
  let ks = toAscList ps
  kvs <- mapM (\ k -> (k,) <$> f k) ks
  return (PredMap.fromDistinctAscList kvs)

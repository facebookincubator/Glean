-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Library of re-usable tools for looking at the buck dependency  graph
module Glean.Util.BuckDeps
  ( -- * Reverse Buck Dependency
    -- ** via Haxl
    locatorReverseDepsH
    -- ** in memory
  , RevDepInfo(..), getRevDepInfo, locatorReverseDeps
    -- * Universe
  , LocatorUniverse(..), toLocatorUniverse, inLocatorUniverse
  ) where

import Data.Default
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.List.Extra (nubOrdOn)
import Data.Maybe
import Data.Text (Text)

import Glean
import qualified Glean.Schema.Buck.Types as Buck
import qualified Glean.Schema.Query.Buck.Types as Q.Buck
import Glean.Util.Buck
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap

-- -----------------------------------------------------------------------------
-- Search reverse deps in Backend via Haxl
--
-- Breadth-first search, tracking seen nodes with @IntSet@.  This gives
-- detailed enough data to reconstruct the full graph.
--
-- Each level of the tree has type  @Level a = [[(a, Bool)]]@ where @Bool@
-- is @True@ for new nodes and @False for ones that have been seen earlier.
-- The items in the outer list are for separate parent nodes.  Each of the
-- items in the outer list are @[(a, Bool)]@ and share a single parent node.
--
-- Begin: 1st level has one entry, the root list with the @Bool@
-- labels (some entries might have a repeated key and be @False@).
--
-- Following: In these levels the non-zero length is the same as
-- the number of @True@ values in the previous level.
--
-- An empty root list means one @Level a@ equal to @[[]]@.
--
-- A non-empty root lists guarantees at least one @True@ value
-- (the first root) thus the second level exists.

-- | One level of the bfs traversal
type Level a = [[(a, Bool)]]

-- | Accumulate levels: list of levels, where a level is @[a]@.
type AccL a = [[a]] -> [[a]]

bfsHaxl
  :: forall a w. (a -> Int) -- ^ Unique key for each @a@ to detect duplicates
  -> (a -> Haxl w [a]) -- ^ Get descendants of @a@ (duplicate keys allowed)
  -> [a] -- ^ Roots (duplicate keys allowed)
  -> Haxl w [[a]]  -- ^ Resulting levels of tree (duplicates removed)
bfsHaxl key childrenOf roots = go id (markListList mempty [roots])
  where
    -- True on first seeing a key value, False if key value already seen
    markOne :: IntSet -> a -> (IntSet, (a, Bool))
    markOne seen a =
      let k = key a
          new = k `IntSet.notMember` seen
          seen' | new = IntSet.insert k seen
                | otherwise = seen
      in (seen', (a, new))

    -- mark duplicates from one call of childrenOf
    markList :: IntSet -> [a] -> (IntSet, [(a, Bool)])
    markList = mapAccumL markOne

    -- mark duplicates in one level (many calls of childrenOf)
    markListList :: IntSet -> [[a]] -> (IntSet, Level a)
    markListList = mapAccumL markList

    -- length of input and output lists match
    makeNextLayer :: IntSet -> [a] -> Haxl w (IntSet, Level a)
    makeNextLayer seen allNew = markListList seen <$> mapM childrenOf allNew

    -- Remove the repeated nodes and flatten each level to @[a]@
    go :: AccL a -> (IntSet, [[(a, Bool)]]) -> Haxl w [[a]]
    go !acc (_seen, []) = return (acc [[]]) -- base case
    go !acc (seen, xss) = do
      let flatNew :: [a]
          flatNew = map fst (filter snd (concat xss))
          acc' = seq flatNew (acc . (flatNew :))
      makeNextLayer seen flatNew >>= go acc'

-- | Glean traversal of the buck reverse dependency information
locatorReverseDepsH
  :: [IdOf Buck.Locator] -- ^ Roots (duplicates allows)
  -> Haxl w [[IdOf Buck.Locator]] -- ^ bfs levels (de-duplicated)
locatorReverseDepsH = bfsHaxl key childrenOf
  where
    key = fromIntegral . fromFid . idOf
    childrenOf locIn = fmap kidsOf $
      search_ $ query $ Q.Buck.LocatorReverseDeps_with_key def
        { Q.Buck.locatorReverseDeps_key_locator = Just $ toQueryId locIn }
      where
        kidsOf lrds =
          [ getId loc
          | lrdk <- mapMaybe Buck.locatorReverseDeps_key lrds
          , loc <- Buck.locatorReverseDeps_key_rdeps lrdk ]

-- -----------------------------------------------------------------------------
-- Cache reverse deps in memory and search that

-- | Cache loaded with 'getRDeps' for use in 'locatorReverseDeps'
data RevDepInfo = RevDepInfo
  { rdi_locs :: !(PredMap Buck.Locator Text) -- ^ @Text@ form of 'Buck.Locator'
  , rdi_rdeps :: !(PredMap Buck.Locator [IdOf Buck.Locator]) }

-- | The Int is the non-negative depth of the dependency in the bfs
newtype LocatorUniverse = LocatorUniverse
  { locatorUniverse :: PredMap Buck.Locator Int }
  deriving (Semigroup, Monoid)

toLocatorUniverse :: [[IdOf Buck.Locator]] -> LocatorUniverse
toLocatorUniverse xss = LocatorUniverse
  { locatorUniverse = PredMap.fromList
      [ (x, level)
      | (level, xs) <- zip [0..] xss
      , x <- xs ] }

inLocatorUniverse
  :: LocatorUniverse -> [IdOf Buck.Locator] -> [(IdOf Buck.Locator, Int)]
inLocatorUniverse lu = mapMaybe go
  where
    go i = fmap (i,) . (`PredMap.lookup` locatorUniverse lu) $ i

-- | Cache the locator information into memory for 'locatorReverseDeps'
getRevDepInfo :: Cell -> Haxl w RevDepInfo
getRevDepInfo defaultCell =
  let getLocs =
        let toPair loc = case Buck.locator_key loc of
              Nothing -> error "impossible"
              Just lk -> (getId loc, buckLocToText defaultCell lk)
            makeLocs = PredMap.fromList . map toPair
        in fmap makeLocs $ search_ $ query $
            Q.Buck.Locator_with_get def
      getRDeps =
        let toPair lrd = case Buck.locatorReverseDeps_key lrd of
              Nothing -> error "impossible"
              Just lrdk ->
                ( getId (Buck.locatorReverseDeps_key_locator lrdk)
                , map getId (Buck.locatorReverseDeps_key_rdeps lrdk) )
            makeRevDeps = PredMap.fromList . map toPair
        in fmap makeRevDeps $ search_ $ query $
            Q.Buck.LocatorReverseDeps_with_get def
  in RevDepInfo <$> getLocs <*> getRDeps

-- | Pure Breadth-first search, tracking seen nodes with IntSet
bfsOf
  :: (a -> Int) -- ^ Get unique key for each @a@
  -> (a -> [a]) -- ^ Get descendants of @a@ (duplicates allowed)
  -> [a] -- ^ Roots (duplicates allowed)
  -> [[a]] -- ^ de-duplicated bfs levels starting with roots
bfsOf key childrenOf = go mempty
  where
    go seen toConsider =
      let (ids, use)
            = unzip
            . filter ((`IntSet.notMember` seen) . fst)
            . nubOrdOn fst
            . map (\a -> (key a, a))
            $ toConsider
      in if null use then []
         else let seen' = IntSet.union seen (IntSet.fromList ids)
              in use : go seen' (concatMap childrenOf use)

-- | In-memory traversal of the buck reverse dependency information
locatorReverseDeps
  :: PredMap Buck.Locator [IdOf Buck.Locator] -- ^ From 'getRevDepInfo'
  -> [IdOf Buck.Locator] -- ^ Roots (duplicates allowed)
  -> [[IdOf Buck.Locator]] -- ^ bfs levels (de-duplicated) starting with roots
locatorReverseDeps rdeps = bfsOf key childrenOf
  where
    key = fromIntegral . fromFid . idOf
    childrenOf = fromMaybe [] . (`PredMap.lookup` rdeps)

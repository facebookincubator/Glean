{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module Derive.Common
  ( getFileXRefs
  , getIndirectTargets
  , resolve
  , Indirects
  , XRefs
  ) where

import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Glean
import Glean.Angle
import Glean.Typed (PidOf)
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap

import Derive.Types

-- | Keys are xRefIndirectTarget_id, values are xRefIndirectTarget_key_target
-- Note: XRefTarget_indirect constructor does not appear in these values
type Indirects = PredMap Cxx.XRefIndirectTarget Cxx.XRefTarget

-- Keys are fileXRefMap_id, values are sets of targets for each external target
-- in the map.
type XRefs = PredMap Cxx.FileXRefMap (V.Vector (HashSet Cxx.XRefTarget))

getIndirectTargets
  :: Backend e => e -> Config -> PidOf Cxx.XRefIndirectTarget -> IO Indirects
getIndirectTargets e cfg _pid = do

  let q = maybe id limit (cfgMaxQueryFacts cfg) $
        limitBytes (cfgMaxQuerySize cfg) allFacts

  -- Build indirect map that is 1-level deep.  Chains of indirections not
  -- yet resolved so the values may be XRefTarget_indirect.
  targets <- runQueryEach e (cfgRepo cfg) q mempty $ \targets fact -> do
    return $ PredMap.insert
      (getId fact)
      (Cxx.xRefIndirectTarget_key_target $ fromJust $ getFactKey fact)
      targets

  let -- Use 'targets' to resolve chains of indirections to resolve
      -- (or remove) all XRefTarget_indirect constructors.
      squash (Cxx.XRefTarget_indirect x)
        | Just next <- PredMap.lookup (getId x) targets = squash next
        | otherwise = Nothing -- shouldn't happen
      squash x = Just x

  return $ PredMap.mapMaybe squash targets

-- | The result never has XRefTarget_indirect constructor
resolve :: Indirects -> Cxx.XRefTarget -> Maybe Cxx.XRefTarget
resolve indirects (Cxx.XRefTarget_indirect x) =
  PredMap.lookup (getId x) indirects
resolve _ x = Just x

getFileXRefs :: Backend e => e -> Config -> IO XRefs
getFileXRefs e cfg = do
  -- Collect all of the `XRefTargets` facts first rather than expanding them.
  -- Since there's a lot of sharing of these facts, the cache hit rate is high.
  targetsMap <- do
    let q :: Query Cxx.XRefTargets
        q = maybe id limit (cfgMaxQueryFacts cfg) $
            limitBytes (cfgMaxQuerySize cfg) allFacts
    runQueryEach e (cfgRepo cfg) q mempty
        $ \targetsMap (Cxx.XRefTargets i k) -> do
      key <- case k of
        Just k -> return k
        Nothing -> throwIO $ ErrorCall "internal error: getFileXRefs"
      return $ PredMap.insert (IdOf $ Fid i) key targetsMap

  let
    q :: Query (Cxx.FileXRefMap, [Cxx.XRefTargets])
    q = maybe id limit (cfgMaxQueryFacts cfg) $
        limitBytes (cfgMaxQuerySize cfg) $
        query $ vars $ \(xmap :: Angle Cxx.FileXRefMap)
                        (targets :: Angle [Cxx.XRefTargets]) ->
           tuple (xmap, targets) `where_` [
             wild .= predicate @Cxx.FileXRefs (
               rec $
                 field @"xmap" (asPredicate xmap) $
                 field @"targets" targets
               end
             )
           ]
  xrefs <- runQueryEach e (cfgRepo cfg) q mempty
      $ \xrefs (xmap, targetss) -> do
    let targets =
          [ HashSet.fromList $ PredMap.findWithDefault
              (error "missing XRefTargets") (getId targets) targetsMap
          | targets <- targetss ]
        id = getId xmap
    case PredMap.lookup id xrefs of
      Just xs -> do
        forM_ (zip [0 .. VM.length xs - 1] targets) $ \(i, ts) -> do
          x <- VM.unsafeRead xs i
          VM.unsafeWrite xs i $! HashSet.union ts x
        return xrefs
      Nothing -> do
        xs <- VM.new (length targets)
        forM_ (zip [0..] targets) $ \(i, ts) ->
          VM.unsafeWrite xs i $! ts
        return $ PredMap.insert id xs xrefs
  traverse V.unsafeFreeze xrefs

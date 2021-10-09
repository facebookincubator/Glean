-- Copyright (c) Facebook, Inc. and its affiliates.


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
  let
    q :: Query Cxx.FileXRefs
    q = maybe id limit (cfgMaxQueryFacts cfg) $
        limitBytes (cfgMaxQuerySize cfg) allFacts

  xrefs <- runQueryEach e (cfgRepo cfg) q mempty
      $ \xrefs (Cxx.FileXRefs _ k) -> do
    key <- case k of
      Just k -> return k
      Nothing -> throwIO $ ErrorCall "internal error: getFileXRefs"
    let exts = Cxx.fileXRefs_key_externals key
        id = getId (Cxx.fileXRefs_key_xmap key)
    case PredMap.lookup id xrefs of
      Just xs -> do
        forM_ (zip [0 .. VM.length xs - 1] exts) $ \(i,ext) -> do
          x <- VM.unsafeRead xs i
          VM.unsafeWrite xs i $! HashSet.insert ext x
        return xrefs
      Nothing -> do
        xs <- VM.new (length exts)
        forM_ (zip [0..] exts) $ \(i,ext) ->
          VM.unsafeWrite xs i $! HashSet.singleton ext
        return $ PredMap.insert id xs xrefs
  traverse V.unsafeFreeze xrefs

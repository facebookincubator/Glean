{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Derive.CxxTargetUses
  ( deriveUses
  ) where

import Control.Exception
import Control.Monad
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Vector as V

import Control.Concurrent.Stream (streamWithState)
import Util.Log (logInfo)

import Glean
import Glean.Angle
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Util.PredMap as PredMap
import qualified Glean.Util.PredSet as PredSet
import Glean.Util.Range

import Derive.Common
import Derive.Types

data Ranges = Ranges
  { ranges     :: !(Set.Set ByteRange)
  , expansions :: !(Set.Set ByteRange)
  , spellings  :: !(Set.Set ByteRange)
  }

instance Semigroup Ranges where
  (Ranges r e s) <> (Ranges r' e' s') = Ranges (r <> r') (e <> e') (s <> s')

instance Monoid Ranges where
  mempty = Ranges Set.empty Set.empty Set.empty
  mappend = (<>)

type Uses = HashMap.HashMap Cxx.XRefTarget Ranges

addUses :: Indirects -> (Cxx.XRefTarget, Cxx.From) -> Uses -> Uses
addUses indirects (target, Cxx.From{..})
  | Just direct <- resolve indirects target = HashMap.insertWith
      (<>)
      direct
      mempty
        { ranges = spansToRanges from_spans
        , expansions = spansToRanges from_expansions
        , spellings = spansToRanges from_spellings
        }
  | otherwise = id
  where spansToRanges = Set.fromList . packedByteSpansToRanges

deriveUses
  :: Backend e
  => e
  -> Config
  -> (Int -> ([Writer] -> IO ()) -> IO ())
  -> IO ()

deriveUses e cfg withWriters = do
  logInfo $ "deriveUses" <> if cfgIncremental cfg then " (incremental)" else ""
  indirects <- getIndirectTargets e cfg
    -- There are usually not many indirect targets, so we can get away
    -- without doing anything special in incremental mode for
    -- getIndirectTargets.
  logInfo $ "loaded " ++ show (PredMap.size indirects) ++ " indirect targets"

  let generateUses
        :: Writer
        -> Maybe Src.File
        -> [IdOf Cxx.FileXRefs]
        -> Uses
        -> IO ()
      generateUses writer (Just file) deps uses = do
        when (not $ cfgDryRun cfg) $ writeFacts writer $ do
          facts <- forM (HashMap.toList uses) $ \(target, Ranges{..}) -> do
            let rangesToSpans = rangesToPackedByteSpans . Set.toList
            makeFact @Cxx.TargetUses Cxx.TargetUses_key
              { targetUses_key_target = target
              , targetUses_key_file = file
              , targetUses_key_from = Cxx.From (rangesToSpans ranges)
                                               (rangesToSpans expansions)
                                               (rangesToSpans spellings)
              }
          when (not (null deps)) $ derivedFrom (coerce deps) facts

      generateUses _ _ _ _ = return ()

      ifIncremental f = if cfgIncremental cfg then f else id

      -- NOTE: We rely on the ordering property of the `allFacts` query.
      --       Specifically, we expect to encounter `FileXRefMap`s for
      --       a single file in a single sequence.
      -- NOTE (incremental):
      --   what's the rationale for only processing the new FileXRefMaps?
      --   Provided we did the fanout correctly, this will include all
      --   FileXRefMaps corresponding to files that have changed.
      q :: Query Cxx.FileXRefMap
      q = maybe id limit (cfgMaxQueryFacts cfg) $
          limitBytes (cfgMaxQuerySize cfg) $ query $ ifIncremental new $
            predicate @Cxx.FileXRefMap wild

      -- The dependencies we record for each cxx.TargetUses fact are
      -- the set of cxx.FileXRefs used to generate it. The other
      -- facts that we touch are reachable from the FileXRefs:
      --   - cxx.FileXRefMap is pointed to by FileXRefs
      --   - cxx.XRefIndirectTarget is pointed to by FileXRefMap
      --
      -- Note that this is not strictly accurate. Each TargetUses fact
      -- is actually derived from some subset of the FileXRefs facts
      -- that we saw, since not all FileXRefs have the same
      -- targets. But since we will never hide just a subset of the
      -- FileXRef facts (if we fully index the fanout) widening the
      -- ownership like this is safe and more efficient because we
      -- generate fewer ownership sets.

  let
    producer yield = do
      let item = batchingYield (cfgBatchSize cfg) yield
      (file, final_facts, batch) <-
        runQueryEach e (cfgRepo cfg) q (Nothing, [], emptyBatch)
          $ \(last_file, facts, batch) fact@(Cxx.FileXRefMap _ k) -> do
            key <- case k of
              Just k -> return k
              Nothing -> throwIO $ ErrorCall "internal error: deriveUses"
            let file = Just $ Cxx.fileXRefMap_key_file key
            if last_file == file
              then return (file, fact:facts, batch)
              else do
                batch <- item (length facts) (last_file, facts) batch
                return (file, [fact], batch)
      yield ([(file, final_facts) | isJust file] <> snd batch)

    worker writer files = do
      fileXRefs <- getFileXRefsFor e (map getId (concatMap snd files)) cfg
      forM_ files $ \(file, facts) -> do
        let
          (deps, uses) = foldr f (mempty, HashMap.empty) facts
          f (Cxx.FileXRefMap i (Just key)) (deps, uses) =
            (new_deps, new_uses)
            where
              !new_deps = PredSet.toList these_deps <> deps
              (these_deps, map_targets) =
                case PredMap.lookup (IdOf $ Fid i) fileXRefs of
                  Nothing -> (PredSet.empty, mempty)
                  Just x -> x
              !new_uses = foldr (addUses indirects) uses $
                [ (target, from)
                | Cxx.FixedXRef target from <- Cxx.fileXRefMap_key_fixed key ]
                ++
                [ (target, from)
                | (targets, from) <-
                    zip (V.toList map_targets) (Cxx.fileXRefMap_key_froms key)
                , target <- HashSet.toList targets ]
          f _ x = x
        generateUses writer file deps uses

  withWriters (cfgWorkerThreads cfg) $ \writers ->
    streamWithState producer writers worker

  completePredicates e (cfgRepo cfg) $
    CompletePredicates_derived $ CompleteDerivedPredicate $
      getName (Proxy @Cxx.TargetUses)

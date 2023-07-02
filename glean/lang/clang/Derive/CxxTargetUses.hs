{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, PartialTypeSignatures, TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Derive.CxxTargetUses
  ( deriveUses
  ) where

import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Vector as V

import Util.Log (logInfo)

import Glean
import Glean.Typed (getPid)
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Util.PredMap as PredMap
import Glean.Util.Range

import Derive.Common
import Derive.Types

data Ranges = Ranges
  { ranges     :: Set.Set ByteRange
  , expansions :: Set.Set ByteRange
  , spellings  :: Set.Set ByteRange
  }

instance Semigroup Ranges where
  (Ranges r e s) <> (Ranges r' e' s') = Ranges (r <> r') (e <> e') (s <> s')

instance Monoid Ranges where
  mempty = Ranges Set.empty Set.empty Set.empty
  mappend = (<>)

type Uses = HashMap.HashMap Cxx.XRefTarget Ranges

addUses :: Indirects -> Cxx.BoundXRef -> Uses -> Uses
addUses indirects (Cxx.BoundXRef target uses)
  | Just direct <- resolve indirects target = HashMap.insertWith
      (<>)
      direct
      mempty
        { ranges = spansToRanges uses_key_spans
        , expansions = spansToRanges uses_key_expansions
        , spellings = spansToRanges uses_key_spellings
        }
  | otherwise = id
  where Cxx.Uses{uses_key = Just Cxx.Uses_key{..}} = uses
        spansToRanges = Set.fromList . packedByteSpansToRanges

deriveUses :: Backend e => e -> Config -> Writer -> IO ()
deriveUses e cfg writer = do
  logInfo "deriveUses"
  indirects <- getIndirectTargets e cfg (getPid writer)
  logInfo $ "loaded " ++ show (PredMap.size indirects) ++ " indirect targets"
  xrefs <- getFileXRefs e cfg
  logInfo $ "loaded " ++ show (PredMap.size xrefs) ++ " file xrefs"

  let generateUses :: Maybe Src.File -> Uses -> IO ()
      generateUses (Just file) uses =
        when (not $ cfgDryRun cfg)
        $ writeFacts writer
        $ forM_ (HashMap.toList uses) $ \(target, Ranges{..}) -> do
            let rangesToSpans = rangesToPackedByteSpans . Set.toList
            makeFact_ @Cxx.TargetUses Cxx.TargetUses_key
              { targetUses_key_target = target
              , targetUses_key_file = file
              , targetUses_key_uses = rangesToRelSpans $
                  Set.toList (ranges <> expansions <> spellings)
              }
            uses <- makeFact @Cxx.Uses Cxx.Uses_key
              { uses_key_spans = rangesToSpans ranges
              , uses_key_expansions = rangesToSpans expansions
              , uses_key_spellings = rangesToSpans spellings
              }
            makeFact_ @Cxx.XRefTargetUses Cxx.XRefTargetUses_key
              { xRefTargetUses_key_target = target
              , xRefTargetUses_key_file = file
              , xRefTargetUses_key_uses = uses
              }
      generateUses _ _ = return ()

      -- NOTE: We rely on the ordering property of the `allFacts` query.
      --       Specifically, we expect to encounter `FileXRefMap`s for
      --       a single file in a single sequence.
      q :: Query Cxx.FileXRefMap
      q = maybe id limit (cfgMaxQueryFacts cfg) $
          limitBytes (cfgMaxQuerySize cfg) $
          expanding @Cxx.Uses allFacts

  (uses, last_file) <- runQueryEach e (cfgRepo cfg) q (mempty, Nothing)
    $ \(uses, last_file) (Cxx.FileXRefMap i k) -> do
      key <- case k of
        Just k -> return k
        Nothing -> throwIO $ ErrorCall "internal error: deriveUses"
      let file = Just $ Cxx.fileXRefMap_key_file key
      let bound = Cxx.fileXRefMap_key_bound key
      let free = Cxx.fileXRefMap_key_free key
      uses <- if last_file == file
        then return uses
        else do
          generateUses last_file uses
          return HashMap.empty
      let new_uses = foldr (addUses indirects) uses $
            bound ++
            [ Cxx.BoundXRef target uses
            | (targets, uses) <- zip
                (V.toList $ PredMap.findWithDefault mempty (IdOf $ Fid i) xrefs)
                free
            , target <- HashSet.toList targets ]
      return (new_uses, file)

  generateUses last_file uses

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
              , targetUses_key_from = Cxx.From (rangesToSpans ranges)
                                               (rangesToSpans expansions)
                                               (rangesToSpans spellings)
              }
      generateUses _ _ = return ()

      -- NOTE: We rely on the ordering property of the `allFacts` query.
      --       Specifically, we expect to encounter `FileXRefMap`s for
      --       a single file in a single sequence.
      q :: Query Cxx.FileXRefMap
      q = maybe id limit (cfgMaxQueryFacts cfg) $
          limitBytes (cfgMaxQuerySize cfg) allFacts

  (uses, last_file) <- runQueryEach e (cfgRepo cfg) q (mempty, Nothing)
    $ \(uses, last_file) (Cxx.FileXRefMap i k) -> do
      key <- case k of
        Just k -> return k
        Nothing -> throwIO $ ErrorCall "internal error: deriveUses"
      let file = Just $ Cxx.fileXRefMap_key_file key
      uses <- if last_file == file
        then return uses
        else do
          generateUses last_file uses
          return HashMap.empty
      let new_uses = foldr (addUses indirects) uses $
            [ (target, from)
            | Cxx.FixedXRef target from <- Cxx.fileXRefMap_key_fixed key ]
            ++
            [ (target, from)
            | (targets, from) <- zip
                (V.toList $ PredMap.findWithDefault mempty (IdOf $ Fid i) xrefs)
                (Cxx.fileXRefMap_key_froms key)
            , target <- HashSet.toList targets ]
      return (new_uses, file)

  generateUses last_file uses

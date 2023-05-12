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

type Uses = HashMap.HashMap Cxx.XRefTarget (Set.Set ByteRange)

addUses :: Indirects -> Cxx.FixedXRef -> Uses -> Uses
addUses indirects (Cxx.FixedXRef target spans)
  | Just direct <- resolve indirects target = HashMap.insertWith
      (<>)
      direct
      (Set.fromList $ relByteSpansToRanges spans)
  | otherwise = id

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
        $ forM_ (HashMap.toList uses) $ \(target, ranges) -> do
            makeFact_ @Cxx.TargetUses Cxx.TargetUses_key
              { targetUses_key_target = target
              , targetUses_key_file = file
              , targetUses_key_uses = rangesToRelSpans $ Set.toList ranges
              }
            uses <- makeFact @Cxx.Uses Cxx.Uses_key
              {
                uses_key_spans = rangesToPackedByteSpans $ Set.toList ranges
              }
            makeFact_ @Cxx.XRefTargetUses Cxx.XRefTargetUses_key
              { xRefTargetUses_key_target = target
              , xRefTargetUses_key_file = file
              , xRefTargetUses_key_uses = uses
              }
      generateUses _ _ = return ()

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
      let new_uses = foldr (addUses indirects) uses
            $ Cxx.fileXRefMap_key_fixed key
            ++ [ Cxx.FixedXRef target offsets
                | (targets, offsets) <- zip
                    (V.toList $ PredMap.findWithDefault
                      mempty
                      (IdOf $ Fid i)
                      xrefs)
                    (Cxx.fileXRefMap_key_variable key)
                , target <- HashSet.toList targets ]
      return (new_uses, file)

  generateUses last_file uses

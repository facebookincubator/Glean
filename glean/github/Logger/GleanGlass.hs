{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Logger.GleanGlass (module Logger.GleanGlass) where

import Data.Text

import Logger.IO

data GleanGlassLogger = GleanGlassLogger

instance Semigroup GleanGlassLogger where
  _ <> _ = GleanGlassLogger

instance Monoid GleanGlassLogger where
  mempty = GleanGlassLogger

runLog :: Logger -> GleanGlassLogger -> IO ()
runLog _ _ = return ()

setSuccess :: Bool -> GleanGlassLogger
setSuccess _ = GleanGlassLogger

setError :: Text -> GleanGlassLogger
setError _ = GleanGlassLogger

setTimeElapsedUs :: Int -> GleanGlassLogger
setTimeElapsedUs _ = GleanGlassLogger

setAllocatedBytes :: Int -> GleanGlassLogger
setAllocatedBytes _ = GleanGlassLogger

setFilepath :: Text -> GleanGlassLogger
setFilepath _ = GleanGlassLogger

setRepo :: Text -> GleanGlassLogger
setRepo _ = GleanGlassLogger

setRepoOther :: [Text] -> GleanGlassLogger
setRepoOther _ = GleanGlassLogger

setSymbol :: Text -> GleanGlassLogger
setSymbol _ = GleanGlassLogger

setItemCount :: Int -> GleanGlassLogger
setItemCount _ = GleanGlassLogger

setRepoName :: Text -> GleanGlassLogger
setRepoName _ = GleanGlassLogger

setRepoHash :: Text -> GleanGlassLogger
setRepoHash _ = GleanGlassLogger

setMethod :: Text -> GleanGlassLogger
setMethod _ = GleanGlassLogger

setRevision :: Text -> GleanGlassLogger
setRevision _ = GleanGlassLogger

setExactRevision :: Bool -> GleanGlassLogger
setExactRevision _ = GleanGlassLogger

setNearestRevision :: Bool -> GleanGlassLogger
setNearestRevision _ = GleanGlassLogger

setMatchingRevision :: Bool -> GleanGlassLogger
setMatchingRevision _ = GleanGlassLogger

setRevisionUsed :: Text -> GleanGlassLogger
setRevisionUsed _ = GleanGlassLogger

setDbUsedName :: Text -> GleanGlassLogger
setDbUsedName _ = GleanGlassLogger

setDbUsedInstance :: Text -> GleanGlassLogger
setDbUsedInstance _ = GleanGlassLogger

setSnapshot :: Text -> GleanGlassLogger
setSnapshot _ = GleanGlassLogger

setTruncated :: Bool -> GleanGlassLogger
setTruncated _ = GleanGlassLogger

setLimit :: Int -> GleanGlassLogger
setLimit _ = GleanGlassLogger

setIncludeXlangXrefs :: Bool -> GleanGlassLogger
setIncludeXlangXrefs _ = GleanGlassLogger

setContentCheck :: Bool -> GleanGlassLogger
setContentCheck _ = GleanGlassLogger

setContentMatch :: Bool -> GleanGlassLogger
setContentMatch _ = GleanGlassLogger

setInternalError :: Text -> GleanGlassLogger
setInternalError _ = GleanGlassLogger

setInternalErrorType :: Text -> GleanGlassLogger
setInternalErrorType _ = GleanGlassLogger

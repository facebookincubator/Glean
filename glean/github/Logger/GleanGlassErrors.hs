{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Logger.GleanGlassErrors (module Logger.GleanGlassErrors) where

import Data.Monoid
import Data.Semigroup
import Data.Text

import Logger.IO

data GleanGlassErrorsLogger = GleanGlassErrorsLogger

instance Semigroup GleanGlassErrorsLogger where
  _ <> _ = GleanGlassErrorsLogger

instance Monoid GleanGlassErrorsLogger where
  mempty = GleanGlassErrorsLogger

runLog :: Logger -> GleanGlassErrorsLogger -> IO ()
runLog _ _ = return ()

setError :: Text -> GleanGlassErrorsLogger
setError _ = GleanGlassErrorsLogger

setErrorType :: Text -> GleanGlassErrorsLogger
setErrorType _ = GleanGlassErrorsLogger

setRepoName :: Text -> GleanGlassErrorsLogger
setRepoName _ = GleanGlassErrorsLogger

setRepoHash :: Text -> GleanGlassErrorsLogger
setRepoHash _ = GleanGlassErrorsLogger

setFilepath :: Text -> GleanGlassErrorsLogger
setFilepath _ = GleanGlassErrorsLogger

setRepo :: Text -> GleanGlassErrorsLogger
setRepo _ = GleanGlassErrorsLogger

setSymbol :: Text -> GleanGlassErrorsLogger
setSymbol _ = GleanGlassErrorsLogger

setMethod :: Text -> GleanGlassErrorsLogger
setMethod _ = GleanGlassErrorsLogger
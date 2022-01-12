{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Logger (
  runLogCmd,
  runLogRepo,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.RateLimiterMap
import Logger.GleanServer (GleanServerLogger)
import qualified Logger.GleanServer as Logger
import Util.Logger

import Glean.Database.Types
import Glean.Types


instance ActionLog GleanServerLogger where
  successLog = Logger.setSuccess True
  failureLog ex = mconcat
    [ Logger.setSuccess False
    , Logger.setError (Text.pack (show ex))
    ]
  timeLog = Logger.setTimeElapsed
  allocLog = Logger.setAllocatedBytes . fromIntegral

runLogCmd :: Text -> Env -> GleanServerLogger -> IO ()
runLogCmd cmd env log =
  whenAllowed (envLoggerRateLimit env) cmd $ \weight ->
    Logger.runLog (envLogger env) $
      log <> Logger.setMethod cmd <> Logger.setWeight weight

runLogRepo :: Text -> Env -> Repo -> GleanServerLogger -> IO ()
runLogRepo cmd env Repo{..} log =
  runLogCmd cmd env $
    log <> Logger.setRepoName repo_name <> Logger.setRepoHash repo_hash

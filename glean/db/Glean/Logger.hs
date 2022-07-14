{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Logger (
  runLogCmd,
  runLogRepo,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.RateLimiterMap
import Glean.Logger.Server as Logger
import Util.Logger

import Glean.Database.Types
import Glean.Types
import Glean.Util.Some


instance ActionLog GleanServerLog where
  successLog = Logger.SetSuccess True
  failureLog ex = mconcat
    [ Logger.SetSuccess False
    , Logger.SetError (Text.pack (show ex))
    ]
  timeLog = Logger.SetTimeElapsed
  allocLog = Logger.SetAllocatedBytes . fromIntegral

runLogCmd :: Text -> Env -> GleanServerLog -> IO ()
runLogCmd cmd env log =
  whenAllowed (envLoggerRateLimit env) cmd $ \weight ->
    case envServerLogger env of
      Some logger ->
        runLog logger $
          log <> Logger.SetMethod cmd <> Logger.SetWeight weight

runLogRepo :: Text -> Env -> Repo -> GleanServerLog -> IO ()
runLogRepo cmd env Repo{..} log =
  runLogCmd cmd env $
    log <> Logger.SetRepoName repo_name <> Logger.SetRepoHash repo_hash

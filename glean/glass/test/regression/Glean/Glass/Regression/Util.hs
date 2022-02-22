{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Util (withTestEnv) where

import Util.EventBase
import Configerator
import ConfigeratorTestUtils
import Facebook.Fb303 ( withFb303 )
import Logger.IO (withLogger)

import Glean
import Glean.Util.Some
import Glean.Util.Time

import Glean.Glass.Env as Glass
import Glean.Glass.Repos

withTestEnv :: Some Backend -> (Glass.Env -> IO a) -> IO a
withTestEnv backend f =
  withEventBaseDataplane $ \evp ->
  -- don't use the real configerator in tests
  withFakeConf $ \_fakeConf ->
  withConfigeratorAPI defaultConfigeratorOptions $ \cfgapi ->
  withLogger cfgapi $ \logger ->
  withFb303 "glass-test" $ \fb303 ->
  withLatestRepos backend (hours 1) $ \latestGleanRepos ->
    f Glass.Env
      { gleanBackend = Some backend
      , gleanIndexBackend = IndexBackend Nothing
      , ..
      }

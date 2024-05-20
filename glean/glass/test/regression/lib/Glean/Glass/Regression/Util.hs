{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Util (withTestEnv, withTestEnvScm) where

import qualified Haxl.Core as Haxl
import Util.EventBase
import Glean.Util.ConfigProvider
import Glean.Impl.TestConfigProvider () -- don't use the real ConfigProvider in tests
import Facebook.Fb303 ( withFb303 )
import Logger.IO (withLogger)

import Glean
import Glean.Util.Some
import Glean.Util.Time

import Glean.Glass.Env as Glass
import Glean.Glass.Repos
import Glean.Glass.SnapshotBackend as SB
import Glean.Glass.RepoMapping
import Glean.Glass.SourceControl

withTestEnv :: Backend b => b -> (Glass.Env -> IO a) -> IO a
withTestEnv b = withTestEnvScm b (Some NilSourceControl)

withTestEnvScm
  :: Backend b
  => b
  -> Some SourceControl
  -> (Glass.Env -> IO a)
  -> IO a
withTestEnvScm backend scm f =
  withEventBaseDataplane $ \evp ->
  withConfigProvider defaultConfigOptions $ \cfgapi ->
  withLogger cfgapi $ \logger ->
  withFb303 "glass-test" $ \fb303 ->
  withLatestRepos backend scm Nothing Nothing (hours 1) $
    \latestGleanRepos ->
      f Glass.Env
        { gleanBackend = Some backend
        , gleanIndexBackend = IndexBackend Nothing
        , snapshotBackend = Some SB.NilSnapshotBackend
        , gleanDB = Nothing
        , repoMapping = fixedRepoMapping
        , sourceControl = scm
        , tracer = mempty
        , haxlState = Haxl.stateEmpty
        , ..
        }

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Derive
  ( main
  ) where

import Control.Concurrent (setNumCapabilities)

import Util.EventBase (withEventBaseDataplane)

import Glean.Init ( withOptions )
import qualified Glean
import Glean.Util.ConfigProvider
import Glean.Impl.ConfigProvider

import Derive.Types
import Derive.All

withNumCapabilities :: Maybe Int -> IO a -> IO a
withNumCapabilities Nothing act = act
withNumCapabilities (Just n) act = setNumCapabilities n >> act

main :: IO ()
main = withOptions options $ \(cfg, service) ->
  withNumCapabilities (cfgNumCapabilities cfg) $
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $ \cfgAPI ->
  Glean.withRemoteBackend evb (cfgAPI::ConfigAPI) service Nothing $ \be ->
  Derive.All.derive be cfg

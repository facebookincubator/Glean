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

import Configerator
import Facebook.Init (withFacebookOptions)
import Util.EventBase (withEventBaseDataplane)

import qualified Glean
import Glean.Impl.ConfigProvider ()

import Derive.Types
import Derive.All

withNumCapabilities :: Maybe Int -> IO a -> IO a
withNumCapabilities Nothing act = act
withNumCapabilities (Just n) act = setNumCapabilities n >> act

main :: IO ()
main = withFacebookOptions options $ \(cfg, service) ->
  withNumCapabilities (cfgNumCapabilities cfg) $
  withEventBaseDataplane $ \evb ->
  withConfigeratorAPI defaultConfigeratorOptions $ \cfgAPI ->
  Glean.withRemoteBackend evb cfgAPI service $ \be ->
  Derive.All.derive be cfg

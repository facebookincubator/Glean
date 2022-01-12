{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Derive (main) where

import Control.Concurrent (setNumCapabilities)
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Options.Applicative as O

import Util.EventBase (withEventBaseDataplane)

import Glean.Util.ConfigProvider

import Derive.Env
import Derive.Lib
import Derive.Types
import qualified Glean.Backend as Backend

withNumCapabilities :: Maybe Int -> IO a -> IO a
withNumCapabilities Nothing act = act
withNumCapabilities (Just n) act = setNumCapabilities n >> act

main :: IO ()
main = withConfigOptions allOptions $ \((cfg, passes), cfgOpts) ->
  withNumCapabilities (cfgNumCapabilities cfg) $
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  Backend.withBackendWithDefaultOptions evb cfgAPI (cfgService cfg) $ \be ->
  withEnv cfg allPredicates be $
    forM_ (Set.toList passes) . dispatchDerive
  where
  allOptions :: O.ParserInfo (Config, Set DerivePass)
  allOptions = O.info (O.helper <*> parser) O.fullDesc
    where parser = (,) <$> options <*> optionsPasses

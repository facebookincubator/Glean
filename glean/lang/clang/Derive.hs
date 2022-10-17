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

import qualified Glean
import qualified Glean.LocalOrRemote as Backend
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider

import Derive.Env
import Derive.Lib
import Derive.Types

withNumCapabilities :: Maybe Int -> IO a -> IO a
withNumCapabilities Nothing act = act
withNumCapabilities (Just n) act = setNumCapabilities n >> act

runDerive
  :: Glean.Backend be => Config -> Set DerivePass -> be -> IO ()
runDerive cfg passes be =
  withEnv cfg allPredicates be $
    forM_ (Set.toList passes) . dispatchDerive

main :: IO ()
main = withConfigOptions allOptions $ \((cfg, passes), cfgOpts) ->
  withNumCapabilities (cfgNumCapabilities cfg) $
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
  Backend.withBackendWithDefaultOptions evb cfgAPI (cfgService cfg)
    (Just schema_id) $ \be ->
  runDerive cfg passes be

  where
    allOptions :: O.ParserInfo (Config, Set DerivePass)
    allOptions = O.info (O.helper <*> parser) O.fullDesc
      where parser = (,) <$> options <*> optionsPasses

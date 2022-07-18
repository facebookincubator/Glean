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
import Glean.Schema.Builtin.Types (schema_id)

import Derive.Types
import Derive.All

import Data.Aeson as J
import Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as BS

withNumCapabilities :: Maybe Int -> IO a -> IO a
withNumCapabilities Nothing act = act
withNumCapabilities (Just n) act = setNumCapabilities n >> act

main :: IO ()
main = withOptions options $ \cmd ->
  case cmd of
    SchemaId ->
      putStrLn $ BS.unpack $ J.encodePretty $
        J.object ["schema_id" J..= J.toJSON schema_id]
    Derive (cfg, service) ->
      withNumCapabilities (cfgNumCapabilities cfg) $
      withEventBaseDataplane $ \evb ->
      withConfigProvider defaultConfigOptions $ \cfgAPI ->
      Glean.withRemoteBackend evb (cfgAPI::ConfigAPI) service Nothing $ \be ->
      Derive.All.derive be cfg

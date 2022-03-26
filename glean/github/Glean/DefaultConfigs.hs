{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.DefaultConfigs (
    defaultClientConfigSource,
    defaultRecipesConfigSource,
    serverConfigPath,
    schemaConfigPath,
  ) where

import Data.Text (Text)

import Glean.Util.ThriftSource
import Glean.ClientConfig.Types
import qualified Glean.Recipes.Types as Recipes

defaultClientConfigSource :: ThriftSource ClientConfig
defaultClientConfigSource = configDefault "client"

defaultRecipesConfigSource :: ThriftSource Recipes.Config
defaultRecipesConfigSource = configDefault "recipes"

-- | Path in configerator where the server configs live
serverConfigPath :: String
serverConfigPath = "server"

-- | config path to the schema definition
schemaConfigPath :: Text
schemaConfigPath = "schema"

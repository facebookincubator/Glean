{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.DefaultConfigs (
    defaultSchemaLocation,
    defaultClientConfigSource,
    serverConfigPath,
    schemaConfigPath,
  ) where

import Data.Text (Text)

import Glean.Util.ThriftSource
import Glean.ClientConfig.Types
import Glean.ServerConfig.Types

-- | Where do we find the schema by default?
defaultSchemaLocation :: SchemaLocation
defaultSchemaLocation = SchemaLocation_dir "$datadir/glean/schema/source"

-- | Path under ~/.config/glean where the default client config lives
defaultClientConfigSource :: ThriftSource ClientConfig
defaultClientConfigSource = configDefault "client"

-- | Path under ~/.config/glean where the server config lives
serverConfigPath :: String
serverConfigPath = "server"

-- | Path under ~/.config/glean where the schema lives if we use `--schema indexconfig`
schemaConfigPath :: Text
schemaConfigPath = "schema-index"

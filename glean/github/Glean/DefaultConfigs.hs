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
defaultClientConfigSource = config "client"

defaultRecipesConfigSource :: ThriftSource Recipes.Config
defaultRecipesConfigSource = config "recipes"

-- | Path in configerator where the server configs live
serverConfigPath :: String
serverConfigPath = "server"

-- | config path to the schema definition
schemaConfigPath :: Text
schemaConfigPath = "schema"

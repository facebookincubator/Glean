module Glean.DefaultConfigs (
    defaultClientConfigSource,
    defaultRecipesConfigSource,
    serverConfigPath,
    schemaConfigPath,
  ) where

import Data.Default
import Data.Text (Text)

import Glean.Util.ThriftSource
import Glean.ClientConfig.Types
import qualified Glean.Recipes.Types as Recipes

defaultClientConfigSource :: ThriftSource ClientConfig
defaultClientConfigSource = value def

defaultRecipesConfigSource :: ThriftSource Recipes.Config
defaultRecipesConfigSource = value def

-- | Path in configerator where the server configs live
serverConfigPath :: String
serverConfigPath = "server"

-- | config path to the schema definition
schemaConfigPath :: Text
schemaConfigPath = "schema"

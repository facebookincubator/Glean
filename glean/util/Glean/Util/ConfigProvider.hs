{-# LANGUAGE AllowAmbiguousTypes, TypeFamilyDependencies #-}
module Glean.Util.ConfigProvider
  ( ConfigProvider(..)
  , ConfigOptions
  , Deserializer
  , ConfigPath
  , TestConfigProvider(..)
  , withConfigOptions
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable
import Options.Applicative (Parser, ParserInfo(..))

import Glean.Init

type Deserializer a = ByteString -> Either String a

type ConfigPath = Text

type family ConfigOptions cfg = opts | opts -> cfg

-- | An interface for a provider of configuration values that might
-- change over time.
class ConfigProvider cfg where

  -- | Allow the user to set the config options
  configOptions :: Parser (ConfigOptions cfg)

  -- | A default set of options
  defaultConfigOptions :: ConfigOptions cfg

  -- | Create a ConfigProvider
  withConfigProvider :: ConfigOptions cfg -> (cfg -> IO a) -> IO a

  type Subscription cfg :: * -> *

  -- | subscribe to a configuration value. The callback should be
  -- invoked whenever the underlying value changes.
  subscribe
    :: Typeable a
    => cfg
    -> ConfigPath
    -> (a -> IO ())
    -> Deserializer a
    -> IO (Subscription cfg a)

  -- | Stop subscribing to a config
  cancel :: cfg -> Subscription cfg a -> IO ()

  -- | Fetch a configuration value and deserialize it.
  get :: Typeable a => cfg -> ConfigPath -> Deserializer a -> IO a

  -- | Identifies exceptions thrown by 'get' or 'subscribe'
  isConfigFailure :: cfg -> SomeException -> Bool


-- | A ConfigProvider in which we can programmatically set values, for testing.
class ConfigProvider cfg => TestConfigProvider cfg where
  setTestConfig :: cfg -> ConfigPath -> ByteString -> IO ()


-- | Variant of 'withOptions' that adds in the ConfigProvider options
withConfigOptions
  :: ConfigProvider cfg
  => ParserInfo a
  -> ((a, ConfigOptions cfg) -> IO b)
  -> IO b
withConfigOptions parserInfo fn =
  Glean.Init.withOptions
    parserInfo {
      infoParser = (,) <$> infoParser parserInfo <*> configOptions
    }
    fn

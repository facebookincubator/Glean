-- | A ConfigProvider for testing. It uses a temporary directory on the
-- filesystem, and supports setting configs programmatically.

module Glean.Impl.TestConfigProvider (
    TestConfigAPI,
    realConfigAPI,
  ) where

import qualified Data.ByteString as ByteString
import Data.Maybe
import qualified Data.Text as Text
import System.FilePath
import System.IO.Temp

import Glean.Util.ConfigProvider
import Glean.Impl.ConfigProvider

newtype TestConfigAPI = TestConfigAPI ConfigAPI

data TestConfigOptions = TestConfigOptions

type instance ConfigOptions TestConfigAPI = TestConfigOptions

instance ConfigProvider TestConfigAPI where

  configOptions = pure TestConfigOptions
  defaultConfigOptions = TestConfigOptions

  withConfigProvider TestConfigOptions f =
    withSystemTempDirectory "glean-config" $ \dir ->
      withConfigProvider (LocalConfigOptions (Just dir)) $ \cfg ->
        f (TestConfigAPI cfg)

  type Subscription TestConfigAPI = LocalSubscription
  subscribe (TestConfigAPI cfg) = subscribe cfg
  cancel (TestConfigAPI cfg) = cancel cfg
  get (TestConfigAPI cfg) = get cfg
  isConfigFailure (TestConfigAPI cfg) = isConfigFailure cfg

realConfigAPI :: TestConfigAPI -> ConfigAPI
realConfigAPI (TestConfigAPI cfg) = cfg

instance TestConfigProvider TestConfigAPI where
  setTestConfig (TestConfigAPI ConfigAPI{..}) path contents = do
    let file = fromMaybe "" (configDir opts) </> Text.unpack path
    ByteString.writeFile file contents

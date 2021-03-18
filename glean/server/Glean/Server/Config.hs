module Glean.Server.Config (
  Config(..), options
) where

import qualified Glean.Database.Config as DBConfig

import qualified Options.Applicative as O

data Config = Config
  { cfgPort :: Maybe Int
  , cfgDBConfig :: DBConfig.Config
  , cfgSetShards :: Bool
  , cfgWritePort :: Maybe FilePath
  , cfgHandler :: String
  }

options :: O.Parser Config
options = Config
  <$> O.optional (O.option O.auto (O.long "port" <> O.short 'p'))
  <*> DBConfig.options
  <*> O.switch (O.long "set-shards")
  <*> O.optional (O.strOption
      (O.long "write-port"
        <> O.metavar "FILE"
        <> O.help "write port number to file once server is alive"))
#if FACEBOOK
  <*> O.strOption
      (O.long "handler"
        <> O.value "glean"
        <> O.metavar "(glean | search)"
        <> O.help "which handler to run")
#else
  <*> pure "glean"
#endif

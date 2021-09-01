-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Server.Config (
  Config(..), options
) where

import qualified Glean.Database.Config as DBConfig

import qualified Options.Applicative as O

data Config = Config
  { cfgPort :: Maybe Int
  , cfgDBConfig :: DBConfig.Config
  , cfgSetShards :: Bool
  , cfgEnableIndexing :: Bool
  , cfgWritePort :: Maybe FilePath
  , cfgHandler :: String -- ^ deprecated, ignored.
  }

options :: O.Parser Config
options = Config
  <$> O.optional (O.option O.auto (O.long "port" <> O.short 'p'))
  <*> DBConfig.options
  <*> O.switch (O.long "set-shards")
  <*> O.switch (O.long "enable-indexing")
  <*> O.optional (O.strOption
      (O.long "write-port"
        <> O.metavar "FILE"
        <> O.help "write port number to file once server is alive"))
  <*> pure "glean"

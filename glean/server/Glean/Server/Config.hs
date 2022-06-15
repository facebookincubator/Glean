{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Server.Config (
  Config(..), options
) where

import qualified Glean.Database.Config as DBConfig

import Control.Applicative
import qualified Options.Applicative as O

data Config = Config
  { cfgPort :: Maybe Int
  , cfgDBConfig :: DBConfig.Config
  , cfgPublishShards :: Bool
  , cfgEnableIndexing :: Bool
  , cfgWritePort :: Maybe FilePath
  , cfgHandler :: String -- ^ deprecated, ignored.
  }

options :: O.Parser Config
options = Config
  <$> O.optional (O.option O.auto (O.long "port" <> O.short 'p'))
  <*> DBConfig.options
  <*> ( O.switch (O.long "publish-shards" <>
                  O.help "publish db hashes via SMC shards")
        -- obsolete spelling still used in TW scripts
        <|> O.switch (O.long "set-shards" <> O.hidden)
  )
  <*> O.switch (O.long "enable-indexing")
  <*> O.optional (O.strOption
      (O.long "write-port"
        <> O.metavar "FILE"
        <> O.help "write port number to file once server is alive"))
  <*> pure "glean"

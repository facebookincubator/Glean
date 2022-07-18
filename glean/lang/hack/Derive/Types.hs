{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Derive.Types
  ( Config(..)
  , options
  , defaultConfig
  , Command(..)
  ) where

import Data.Default
import qualified Options.Applicative as O

import qualified Glean
import qualified Glean.Types as Thrift (Repo)-- gen
import Glean.Write.Async (SendQueueSettings(..), WriterSettings(..))
import Glean.Write.Options (sendQueueOptions, writerOptions)

-- | Configuration options for "Derive" and its passes
data Config = Config
  { cfgRepo :: Thrift.Repo
  , cfgNumCapabilities :: Maybe Int  -- ^ override @+RTS -Nx -RTS@
  , cfgMaxQueryFacts :: Maybe Int
  , cfgMaxQuerySize :: Int
  , cfgSendQueue :: SendQueueSettings
  , cfgWriter :: WriterSettings
  , cfgChunksPerCapability :: Int  -- ^ units of work per worker
  , cfgMaxQueueSize :: Int
  }

defaultConfig :: Thrift.Repo -> Config
defaultConfig repo = Config
  { cfgRepo = repo
  , cfgNumCapabilities =  Nothing
  , cfgMaxQueryFacts = Nothing
  , cfgMaxQuerySize = defaultMaxQuerySize
  , cfgSendQueue = def
  , cfgWriter = def
  , cfgChunksPerCapability = defaultChunksPerCapability
  , cfgMaxQueueSize = defaultMaxQueueSize
  }

defaultMaxQuerySize  :: Int
defaultMaxQuerySize = 30000000

defaultChunksPerCapability :: Int
defaultChunksPerCapability = 5

defaultMaxQueueSize :: Int
defaultMaxQueueSize = 500

data Command
    = Derive (Config, Glean.ThriftSource Glean.ClientConfig)
    | SchemaId

-- | Command-line argument parser for "Derive" to get 'Config'
options :: O.ParserInfo Command
options = O.info (O.helper <*> (parserDerive O.<|> parserSchemaId)) O.fullDesc
  where
    parserSchemaId :: O.Parser Command
    parserSchemaId =
        O.flag' SchemaId (O.long "schema-id" <> O.help "display schema-id")

    parserDerive :: O.Parser Command
    parserDerive = Derive <$> ((,) <$> config <*> Glean.options)

    config  :: O.Parser Config
    config = do
      cfgRepo <- Glean.readRepo "/" <$> O.strOption
        (  O.long "repo"
        <> O.metavar "REPO"
        <> O.help "repo DB (default: use latest complete DB)" )
      cfgNumCapabilities <- O.optional $ O.option O.auto $
        O.long "num-capabilities"
        <> O.metavar "N"
        <> O.help "optionally override +RTS -Nx -RTS"
      cfgMaxQueryFacts <- O.optional $ O.option O.auto $
        O.long "max-query-facts"
        <> O.metavar "N"
        <> O.help "maximum number of facts to query in one step"
      cfgMaxQuerySize <- O.option O.auto $
        O.long "max-query-size"
        <> O.metavar "N"
        <> O.value defaultMaxQuerySize
        <> O.help "maximum number of bytes to query in one step"
      cfgSendQueue <- sendQueueOptions
      cfgWriter <- writerOptions
      cfgChunksPerCapability <- O.option O.auto $
        O.long "chunks-per-capability"
        <> O.metavar "N"
        <> O.value defaultChunksPerCapability
        <> O.help "units of work per worker"
      cfgMaxQueueSize <- O.option O.auto $
        O.long "max-queue-size"
        <> O.metavar "N"
        <> O.value defaultMaxQueueSize
        <> O.help "maximum chunks stored in TBQueue"
      return Config{..}

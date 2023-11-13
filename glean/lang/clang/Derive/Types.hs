{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
-- | Supporting types for "Derive" so that some passes
-- can compile separately.
module Derive.Types
  ( -- * types
    Config(..), MatchAlgorithm(..)
    -- * parsing options
  , options
    -- * testing
  , testConfig
  ) where

import Control.Applicative
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Options.Applicative as O

import Glean
import qualified Glean.LocalOrRemote as Glean

-- | Options for 'DeriveFunctionCalls' pass for benchmarking algorithms
data MatchAlgorithm = Naive | SweepingLine | SourceVector

instance Default MatchAlgorithm where def = SourceVector

-- | Configuration options for "Derive" and its passes
data Config = Config
  { cfgService :: Glean.Service
  , cfgDefaultCell :: Text
  , cfgRepo :: Repo
  , cfgDryRun :: Bool
  , cfgNumCapabilities :: Maybe Int  -- ^ override @+RTS -Nx -RTS@
  , cfgWorkerThreads :: Int  -- ^ number of threads with a Writer
  , cfgSendQueue :: SendQueueSettings
  , cfgWriter :: WriterSettings
  , cfgMaxQueryFacts :: Maybe Int
  , cfgMaxQuerySize :: Int
  , cfgMaxStoreQuerySize :: Int
    -- storing takes a lot longer than just querying, so we set a much
    -- lower size limit when storing to avoid timeouts.
  , cfgBenchmark :: Bool  -- ^ function-calls pass
  , cfgMatchAlgorithm :: MatchAlgorithm  -- ^ function-calls pass
  , cfgDebugPrintReferences :: Bool  -- ^ function-calls pass
  , cfgMaxQueueSize :: Int  -- ^ function-calls pass
  , cfgIncremental :: Bool  -- ^ derive incrementally
  }

-- | Only used for regression testing derived passes
testConfig :: Repo -> Config
testConfig repo = Config
  { cfgService = error "testConfig.cfgService"
  , cfgDefaultCell = repo_name repo
  , cfgRepo = repo
  , cfgDryRun = False
  , cfgNumCapabilities = Nothing
  , cfgWorkerThreads = 1
  , cfgSendQueue = def
    { sendQueueThreads = 1
    , sendQueueMaxMemory = 2000000000
    , sendQueueMaxBatches = 1024 }
  , cfgWriter = def{ writerMaxSize = 30000000 }
  , cfgMaxQueryFacts = Nothing
  , cfgMaxQuerySize = 30000000
  , cfgMaxStoreQuerySize = 10000000
  , cfgBenchmark = False
  , cfgMatchAlgorithm = def
  , cfgDebugPrintReferences = False
  , cfgMaxQueueSize = 100000
  , cfgIncremental = False
  }

-- | Command-line argument parser for "Derive" to get 'Config'
--
-- See 'Runner.Server.callDerived'
options :: O.Parser Config
options = do
  cfgService <- Glean.options
  cfgDefaultCell <- O.option (Text.pack <$> O.str) $
    O.long "default-cell"
    <> O.metavar "CELL"
    <> O.help "Default buck cell name for buck.Locator{subdir=nothing}"
    <> O.value "fbsource"
  cfgRepo <- readRepo "/" <$> O.strOption
    (  O.long "repo"
    <> O.metavar "NAME/HASH"
    <> O.help "repo DB (default: use latest complete DB)" )
  cfgDryRun <- O.switch $ O.long "dry-run"
  cfgNumCapabilities <- O.optional $ O.option O.auto $
    O.long "num-capabilities"
    <> O.metavar "N"
    <> O.help "optionally override +RTS -Nx -RTS"
  cfgWorkerThreads <- O.option O.auto $
    O.long "worker-threads"
    <> O.metavar "N"
    <> O.value 1
    <> O.help "number of concurrent writer threads, default 1"
  cfgSendQueue <- sendQueueOptions
  cfgWriter <- writerOptions
  cfgMaxQueryFacts <- O.optional $ O.option O.auto $
    O.long "max-query-facts"
    <> O.metavar "N"
    <> O.help "maximum number of facts to query in one step"
  cfgMaxQuerySize <- O.option O.auto $
    O.long "max-query-size"
    <> O.metavar "N"
    <> O.value 30000000
    <> O.help "maximum number of bytes to query in one step"
  cfgMaxStoreQuerySize <- O.option O.auto $
    O.long "max-store-query-size"
    <> O.metavar "N"
    <> O.value 1000000
    <> O.help "maximum number of bytes to query in one step when storing"
  cfgBenchmark <- O.switch $ O.long "benchmark"
  cfgMatchAlgorithm <- O.flag' Naive (O.long "match-naive") <|>
        O.flag' SweepingLine (O.long "match-sweeping-line") <|>
        O.flag' SourceVector (O.long "match-source-vector") <|>
        pure def
  cfgDebugPrintReferences <- O.switch $ O.long "debug-print"
  cfgMaxQueueSize <- O.option O.auto $
    O.long "max-queue-size"
    <> O.metavar "N"
    <> O.value 10000
    <> O.help "function-calls: maximum queue size for cxx.FileXRefMap"
  cfgIncremental <- O.switch $
    O.long "incremental"
  return Config{..}

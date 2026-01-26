{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Benchmark
  ( BenchmarkRunner
  , benchmarkMain
  , GleanBenchConfig(..)
  ) where

import Criterion.Main
import Criterion.Main.Options
import Data.Default
import Options.Applicative

import Glean.Init

type BenchmarkRunner = [Benchmark] -> IO ()

benchmarkMain :: (GleanBenchConfig -> BenchmarkRunner -> IO ()) -> IO ()
benchmarkMain exec =
  withOptions
    (describeWith ((,) <$> opts <*> parseWith defaultConfig)) $ \(conf, crit) ->
    exec conf (runMode crit)

data GleanBenchConfig = GleanBenchConfig {
    useLMDB :: Bool
  }

instance Default GleanBenchConfig where
  def = GleanBenchConfig { useLMDB = False }

opts :: Parser GleanBenchConfig
opts = GleanBenchConfig
  <$> switch (long "lmdb" <> help "use LMDB (otherwise use RocksDB)")

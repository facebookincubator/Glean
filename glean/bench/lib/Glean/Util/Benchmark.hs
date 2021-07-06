-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Util.Benchmark
  ( BenchmarkRunner
  , benchmarkMain
  ) where

import Criterion.Main
import Criterion.Main.Options

import Glean.Init

type BenchmarkRunner = [Benchmark] -> IO ()

benchmarkMain :: (BenchmarkRunner -> IO ()) -> IO ()
benchmarkMain exec = withOptions (describe defaultConfig) $
  exec . runMode

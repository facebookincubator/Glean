{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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

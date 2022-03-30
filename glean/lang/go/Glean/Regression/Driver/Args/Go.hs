{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.Args.Go ( args ) where

-- | How to run the go (lsif-go) external indexer from regression tests
args :: FilePath -> [String]
args path =
    ["--binary", "lsif-go"
    ,"--lsif"
    ,"--root", path
    ]

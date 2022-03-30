{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.Args.TypeScript ( args ) where

-- | How to run the typescript (lsif-tsc) external indexer from regression tests
args :: FilePath -> [String]
args path =
    ["--binary", "lsif-tsc"
    ,"--lsif"
    ,"--root", path
    ]

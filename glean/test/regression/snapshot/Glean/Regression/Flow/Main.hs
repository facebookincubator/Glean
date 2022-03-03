{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
module Glean.Regression.Flow.Main ( main ) where

import System.Environment ( withArgs )

import qualified Glean.Regression.Driver.External as Driver ( main )
import qualified Glean.Regression.Driver.Args.Flow as Flow

main :: IO ()
main = withArgs (Flow.args path) Driver.main
  where
      path = "glean/lang/codemarkup/tests/flow/"

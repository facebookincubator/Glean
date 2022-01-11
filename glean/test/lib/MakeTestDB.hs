{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module MakeTestDB (main) where

import System.Environment

import Glean.Database.Test
import TestDB

main :: IO ()
main = do
  [tmpdir] <- getArgs
  withTestDB [setRoot tmpdir] $ \_ _ -> return ()

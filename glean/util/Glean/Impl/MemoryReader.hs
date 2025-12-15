{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Impl.MemoryReader
  (totalMemCapacityKB
  ) where

-- template for open source build
-- not implemented yet
totalMemCapacityKB :: IO (Maybe Int)
totalMemCapacityKB = return Nothing

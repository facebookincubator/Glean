{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Foreign.Stats
  ( marshalPredicateStats
  ) where

import Control.Monad (forM)
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Util.FFI

import Glean.FFI
import Glean.RTS.Types (Pid(..))
import Glean.Types

marshalPredicateStats
  :: (Ptr CSize
      -> Ptr (Ptr Int64)
      -> Ptr (Ptr Word64)
      -> Ptr (Ptr Word64)
      -> IO CString)
  -> IO [(Pid, PredicateStats)]
marshalPredicateStats get = do
  (count, pids, counts, sizes) <- invoke get
  usingMalloced pids $
    usingMalloced counts $
    usingMalloced sizes $
    forM [0 .. fromIntegral count - 1] $ \i -> do
      pid <- peekElemOff pids i
      count <- peekElemOff counts i
      size <- peekElemOff sizes i
      return (Pid pid, PredicateStats (fromIntegral count) (fromIntegral size))

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module ServiceData.GlobalStats (module ServiceData.GlobalStats) where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap

import ServiceData.Types

setCounter :: ByteString -> Int -> IO ()
setCounter _ _ = return ()

clearCounter :: ByteString -> IO ()
clearCounter _ = return ()

getCounters :: IO (HashMap.HashMap ByteString Int)
getCounters = return HashMap.empty

addStatValue :: ByteString -> Int -> IO ()
addStatValue _ _ = return ()

addStatValueType :: ByteString -> Int -> ExportType -> IO ()
addStatValueType _ _ _ = return ()

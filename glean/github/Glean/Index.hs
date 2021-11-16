{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Index
  ( index
  )
  where

import qualified Glean.Types as Thrift
import qualified Glean.Index.Types as Thrift
import Control.Exception (throwIO)

type Port = Int

index
  :: IO Port
  -> a
  -> Thrift.IndexRequest
  -> IO Thrift.IndexResponse
index _ _ _ = throwIO $ Thrift.Exception "not implemented"

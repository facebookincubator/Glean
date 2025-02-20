{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Server.DownloadBatch
  ( downloadBatch
  ) where

import Glean.Types as Thrift
import Control.Exception

downloadBatch :: Thrift.BatchDescriptor -> IO Thrift.Batch
downloadBatch _ = throwIO $
  Thrift.Exception "couldn't download batch from location"

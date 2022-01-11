{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Datasource.Scribe.Write
  ( scribeWriteBatches
  ) where

import Control.Exception

scribeWriteBatches
  :: writeFromScribe
  -> bucket
  -> [jsonFactBatch]
  -> Bool
  -> IO ()
scribeWriteBatches _ _ _ _ = do
  throwIO $ ErrorCall $ "writing via Scribe is not supported in this build"

-- Copyright (c) Facebook, Inc. and its affiliates.

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

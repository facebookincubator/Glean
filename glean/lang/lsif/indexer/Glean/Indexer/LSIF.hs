{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.LSIF ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.LSIF.Driver as LSIF

data LSIF = LSIF
  -- no options currently

options :: Parser LSIF
options = pure LSIF

-- | An indexer that just slurps an existing LSIF file. Usage:
--
--   cli:    glean index lsif <file> --repo name/hash
--   shell:  :index lsif <file>
--
indexer :: Indexer LSIF
indexer = Indexer {
  indexerShortName = "lsif",
  indexerDescription = "Index an LSIF file",
  indexerOptParser = options,
  indexerRun = \LSIF backend repo IndexerParams{..} -> do
    val <- LSIF.processLSIF indexerRoot
    sendJsonBatches backend repo "lsif" val
  }

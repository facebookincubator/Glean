{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.SCIP (
    indexer,
  ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External
import Glean.SCIP.Driver as SCIP

data SCIP = SCIP
  -- no options currently

options :: Parser SCIP
options = pure SCIP

-- | An indexer that just slurps an existing SCIP file. Usage:
--
--   cli:    glean index scip file> --repo name/hash
--   shell:  :index scip <file>
--
indexer :: Indexer SCIP
indexer = Indexer {
  indexerShortName = "scip",
  indexerDescription = "Index a SCIP file",
  indexerOptParser = options,
  indexerRun = \SCIP backend repo IndexerParams{..} -> do
    val <- SCIP.processSCIP indexerRoot
    sendJsonBatches backend repo "scip" val
  }

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Flow ( indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

data Flow = Flow
  { flowBinary :: FilePath
  , flowWriteRoot :: String
  }

options :: Parser Flow
options = do
  flowBinary <- strOption $
    long "flow" <>
    value "flow" <>
    help "path to the flow binary"
  flowWriteRoot <- strOption $ long "write-root" <> value "test"
  return Flow{..}

indexer :: Indexer Flow
indexer = Indexer {
  indexerShortName = "flow",
  indexerDescription = "Index JS/Flow code",
  indexerOptParser = options,
  indexerRun = \Flow{..} -> do
    let ext = Ext {
          extRunScript = flowBinary,
          extFlavour = Json,
          extArgs =
            [ "glean"
            , "${TEST_ROOT}"
            , "--output-dir"
            , "${JSON_BATCH_DIR}"
            , "--write-root"
            , flowWriteRoot
            ],
          extDerivePredicates =
            [ "flow.StringToFileModule"
            , "flow.FileXRef"
            , "flow.FileDeclaration"
            , "flow.FlowEntityImportUses"
            , "flow.FlowTypeEntityImportUses"
            , "flow.NameLowerCase"
            ]
        }
    indexerRun externalIndexer ext
  }

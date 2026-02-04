{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Indexer.Angle (indexer) where

import Glean.Indexer
import Glean.Indexer.External
import Options.Applicative


data Angle = Angle
  { binaryPath :: FilePath
  , repoRoot :: FilePath
  }

options :: Parser Angle
options =
  Angle
    <$> strOption
      ( long "indexer-binary"
          <> metavar "PATH"
          <> showDefault
          <> help "path to the indexer binary"
      )
    <*> strOption
      ( long "repo-root"
          <> metavar "PATH"
          <> showDefault
          <> help "path to the repo root"
      )

indexer :: Indexer Angle
indexer =
  Indexer
    { indexerShortName = "angle"
    , indexerDescription = "Index Angle code"
    , indexerOptParser = options
    , indexerRun = \Angle {..} -> do
        let ext =
              Ext
                { extRunScript = binaryPath
                , extFlavour = Server
                , extArgs =
                    [ "index",
                      "--service"
                    , "${GLEAN_SERVER}"
                    , "--db"
                    , "${TEST_REPO_NAME}/${TEST_REPO_HASH}"
                    , "--repo-path"
                    , repoRoot
                    , "--dir"
                    ,"${TEST_ROOT}"
                    ]
                , extDerivePredicates =
                  ["anglelang.TargetUses"
                  , "search.anglelang.NameLowerCase"]
                , extAllowNonZeroExit = False
                }
        indexerRun externalIndexer ext
    }

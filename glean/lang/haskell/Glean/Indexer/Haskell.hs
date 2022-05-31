{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.Haskell (
  indexer,
) where

import Glean.Indexer
import Glean.Indexer.External
import Options.Applicative

data Haskell = Haskell
  { haskellBinary :: FilePath
  , haskellRepoRoot :: FilePath
  }

options :: Parser Haskell
options =
  Haskell
    <$> strOption
      ( long "hiedb-indexer"
          <> value "hiedb-indexer"
          <> metavar "PATH"
          <> showDefault
          <> help "path to the hiedb indexer binary"
      )
    <*> strOption
      ( long "source-root"
          <> metavar "DIR"
          <> value "."
          <> showDefault
          <> help
            ( unwords
                [ "Path containing the source files." ]
            )
      )

indexer :: Indexer Haskell
indexer =
  Indexer
    { indexerShortName = "haskell-hie"
    , indexerDescription = "Index Haskell code"
    , indexerOptParser = options
    , indexerRun = \Haskell {..} -> do
        let ext =
              Ext
                { extRunScript = haskellBinary
                , extFlavour = Server
                , extArgs =
                    [ "--service"
                    , "${GLEAN_SERVER}"
                    , "--repo-name"
                    , "${TEST_REPO_NAME}"
                    , "--repo-hash"
                    , "${TEST_REPO_HASH}"
                    , "--repo-path"
                    , haskellRepoRoot
                    , "--dont-create-db"
                    , "${TEST_ROOT}"
                    , ""
                    ]
                , extDerivePredicates = []
                }
        indexerRun externalIndexer ext
    }

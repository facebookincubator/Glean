{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Haskell (
  indexer,
) where

import Options.Applicative
import System.Directory
import System.FilePath
import System.Process

import Glean.Indexer
import Glean.Indexer.External
import Util.IO

data Haskell = Haskell
  { haskellBinary :: FilePath
  , haskellGhc :: Maybe FilePath
  , haskellSrcs :: [FilePath]
  , haskellArgs :: [String]
  }

options :: Parser Haskell
options = do
  haskellBinary <-strOption
    ( long "hie-indexer"
        <> value "hie-indexer"
        <> metavar "PATH"
        <> showDefault
        <> help "path to the hie-indexer binary"
    )
  haskellGhc <- optional $ strOption
    ( long "with-ghc"
        <> metavar "PATH"
        <> help ("GHC to invoke to generate .hie files " <>
             " (otherwise read existing .hie files)")
    )
  haskellSrcs <- many (strOption (long "src"))
  haskellArgs <- many (strOption (
    long "arg" <> metavar "ARG" <>
    help "argument to pass to hie-indexer"))
  return Haskell{..}

indexer :: Indexer Haskell
indexer =
  Indexer
    { indexerShortName = "haskell-hie"
    , indexerDescription = "Index Haskell code"
    , indexerOptParser = options
    , indexerRun = \Haskell {..} backend repo params -> do
        hieDir <- case haskellGhc of
          Just ghc -> do
            let isHs = (== ".hs") . takeExtension
            root <- makeRelativeToCurrentDirectory (indexerRoot params)
            srcs <- filter isHs <$> listDirectoryRecursive root
            let out = indexerOutput params </> "ghc-out"
            callProcess ghc $ [
              "--make",
              "-c",
              "-fwrite-ide-info",
              "-outputdir", out
              ] <> srcs
            return out
          Nothing -> return (indexerRoot params)
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
                    , hieDir
                    ] <> [ "--src=" <> f | f <- haskellSrcs ] <> haskellArgs
                , extDerivePredicates = [
                    "hs.OccNameLowerCase",
                    "hs.SourceModule"
                  ],
                  extAllowNonZeroExit = False
                }
        indexerRun externalIndexer ext backend repo params
    }

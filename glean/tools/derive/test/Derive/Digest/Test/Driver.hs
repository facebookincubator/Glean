{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Derive.Digest.Test.Driver
  ( main
  ) where

import Control.Monad (void)
import System.FilePath ((</>), splitFileName, joinPath, splitPath)

import Util.Timing

import Glean.Indexer
import Glean.Indexer.List (cmdLineParser)
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot
import Glean.Derive.Digest.Lib ( derive, Config(..) )
import Data.Hashable (hash)
import Options.Applicative
import TextShow (showt)

data Options = Options
  { runIndexerOptions :: RunIndexer
  , deriveConfig :: Config
  }

optionsParser :: Parser Options
optionsParser = do
  runIndexerOptions <- cmdLineParser
  hashDigests <- switch (long "hash-digests")
  stripDepth <- option auto (long "strip" <> help "strip depth" <> value 0)
  return Options{
    runIndexerOptions = runIndexerOptions,
    deriveConfig = Config{
      hashFunction = if hashDigests then showt . hash else id,
      pathAdaptor = stripPath stripDepth
      }
  }

-- | stripPath 1 "www/take/me/home.php"  = "take/me/home.php"
stripPath :: Int -> FilePath -> FilePath
stripPath 0 path = path
stripPath depth path = stripped_body </> file
  where
    (body,file) = splitFileName path
    stripped_body = joinPath $ drop depth $ splitPath body

driver :: Driver Options
driver = driverFromIndexer $
  Indexer {
    indexerShortName = "multiple",
    indexerDescription = "Choose an indexer to run",
    indexerOptParser = optionsParser ,
    indexerRun = runIndexerOptions <> deriveDigests
    }
  where
    deriveDigests opts backend repo params =
      void $ reportAndShowTime "derive digests" $ derive backend repo
        (deriveConfig opts)
          { pathAdaptor =
            (indexerRoot params </>) . pathAdaptor (deriveConfig opts)
          }

main :: IO ()
main = testMain driver

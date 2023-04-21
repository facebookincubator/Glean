{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
module Derive.Digest.Test.Driver
  ( main
  ) where

import Control.Monad (void)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (pack)
import System.FilePath ((</>), splitFileName, joinPath, splitPath)

import Util.OptParse (commandParser)
import Util.Timing

import qualified Glean.Clang.Test as Clang
import Glean.Indexer
import Glean.Indexer.List (cmdLineParser)
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot
import Glean.Derive.Digest.Lib ( derive, Config(..), replaceName )
import Data.Hashable (hash)
import Options.Applicative
import TextShow (showt)
import qualified Glean.Glass.Types as Glass

data Options = Options
  { runIndexerOrCpp :: Either RunIndexer Clang.Options
  , deriveConfig :: Config
  }

optionsParser :: Parser Options
optionsParser = do
  runIndexerOrCpp <-
    (Right <$> commandParser "cpp"
                (progDesc "Use the Clang indexer")
                (indexerOptParser $ driverIndexer Clang.driver))
      <|>
    (Left <$> cmdLineParser)
  hashDigests <- switch (long "hash-digests")
  stripDepth <- option auto (long "strip" <> help "strip depth" <> value 0)
  indexOnly <- many (pack <$> strOption(long "index-only"))
  return Options{
    runIndexerOrCpp = runIndexerOrCpp,
    deriveConfig = Config{
      hashFunction = \name ->
        (if hashDigests then showt . hash else id) .
          replaceName name (Glass.Name "SELF"),
      pathAdaptor = stripPath stripDepth,
      indexOnly = nonEmpty indexOnly
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
driver = (driverFromIndexer indexer)
  { driverGroups = \opts -> case runIndexerOrCpp opts of
      Right clangOpts -> driverGroups Clang.driver clangOpts
      Left _ -> []
  }
  where
    indexer = Indexer
      { indexerShortName = "multiple",
        indexerDescription = "Choose an indexer to run",
        indexerOptParser = optionsParser ,
        indexerRun = \opts -> case runIndexerOrCpp opts of
          Right clangOpts ->
            indexerRun (driverIndexer Clang.driver) clangOpts <>
            deriveDigests (deriveConfig opts)
          Left runIndexerOptions ->
            runIndexerOptions <> deriveDigests (deriveConfig opts)
      }
    deriveDigests config backend repo params =
      void $ reportAndShowTime "derive digests" $ derive backend repo
        config
          { pathAdaptor =
            (indexerRoot params </>) . pathAdaptor config
          }

main :: IO ()
main = testMain driver

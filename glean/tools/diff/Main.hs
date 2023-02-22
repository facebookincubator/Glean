{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Options.Applicative
import Numeric.Natural (Natural)

import Util.EventBase

import Diff (diff, Result(..), DiffOptions(..))
import qualified Glean
import Glean (Repo(..))
import qualified Glean.Database.Config as Database
import qualified Glean.Database.Env as Database
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.Util.ConfigProvider

data Config = Config
  { cfgDB :: Database.Config
  , cfgOriginal :: Repo
  , cfgNew :: Repo
  , cfgLogAdded :: Bool
  , cfgBatchSize :: Maybe Natural
  }

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Compare two databases")
  where
    parser :: Parser Config
    parser = do
      cfgDB <- Database.options
      cfgLogAdded <- switch $
        long "log-added" <>
        help "Log facts added in second DB"
      cfgBatchSize <- optional $ option auto $
        long "batch-size" <>
        metavar "N" <>
        help "How many facts to deduplicate together"
      cfgOriginal <- argument (maybeReader Glean.parseRepo) $
        metavar "NAME/HASH"
      cfgNew <- argument (maybeReader Glean.parseRepo) $
        metavar "NAME/HASH"
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
  Database.withDatabases evb cfgDB cfgAPI $ \env -> do
  let opts = DiffOptions
        { opt_logAdded = cfgLogAdded
        , opt_batchSize = fromMaybe (opt_batchSize def) cfgBatchSize
        }
  Result kept added removed <- diff env opts cfgOriginal cfgNew
  putStrLn $ unlines
    [ "kept: " <> show kept
    , "added: " <> show added
    , "removed: " <> show removed ]

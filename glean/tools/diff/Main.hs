{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import Data.Default (def)
import Options.Applicative

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
  }

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Compare two databases")
  where
    parser :: Parser Config
    parser = do
      cfgDB <- Database.options
      cfgOriginal <- argument (maybeReader Glean.parseRepo)
        (  metavar "NAME/HASH"
        )
      cfgNew <- argument (maybeReader Glean.parseRepo)
        (  metavar "NAME/HASH"
        )
      cfgLogAdded <-
        switch (long "log-added" <> help "log facts added in second DB")
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
  Database.withDatabases evb cfgDB cfgAPI $ \env -> do
  let opts = def { opt_logAdded = cfgLogAdded }
  Result kept added removed <- diff env opts cfgOriginal cfgNew
  putStrLn $ unlines
    [ "kept: " <> show kept
    , "added: " <> show added
    , "removed: " <> show removed ]

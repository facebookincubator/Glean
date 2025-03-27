{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Indexer.Angle
  (main
  ) where


import Data.Default
import Options.Applicative
import Util.EventBase

import Glean
import Glean.LocalOrRemote
import Glean.Util.ConfigProvider
import Glean.Impl.ConfigProvider
import Glean.Schema.Builtin.Types (schema_id)

data Options = Options
  { optDir ::  String
  , optDb :: Glean.Repo
  , optService :: Service
  }

opts :: ParserInfo Options
opts = info (helper <*> parser) fullDesc
  where
    parser :: Parser Options
    parser = do
      optDir <- strOption
        (long "dir"
        <> metavar "DIR"
        <> help "dir of schemas to index")
      optDb <- option (maybeReader Glean.parseRepo)
        (  long "db"
        <> metavar "NAME"
        <> help "database to write facts to" )
      optService <- options
      return Options{..}

buildFacts :: FactBuilder
buildFacts = return () -- Not implemented yet

send :: Backend be => be -> Repo -> IO ()
send be repo = do
  withSender be repo refs def $ \sender ->
    withWriter sender def $ \writer ->
      writeFacts writer buildFacts

    where
      -- TODO: add predicates sent, e.g., [ Anglelang.allPredicates, Src., ..]
      refs = []

main :: IO ()
main = do
  withConfigOptions opts $ \(opts, cfgOpts) ->
    withEventBaseDataplane $ \evb ->
    withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
    withBackendWithDefaultOptions evb cfgAPI (optService opts) (Just schema_id)
      $ \backend -> do
        send backend $ optDb opts

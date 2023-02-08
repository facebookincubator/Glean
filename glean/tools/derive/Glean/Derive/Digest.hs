{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

-- | An external deriver for generating codemarkup.LocationDigest facts
module Glean.Derive.Digest (
  main,
) where

import Data.Hashable (hash)
import Options.Applicative (
  ParserInfo,
  fullDesc,
  help,
  helper,
  info,
  long,
  maybeReader,
  metavar,
  option,
  (<**>), progDesc
 )
import TextShow (showt)

import Util.EventBase (withEventBaseDataplane)

import Glean (
  Repo,
  parseRepo,
 )
import Glean.Derive.Digest.Lib (derive)
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.LocalOrRemote (withBackend)
import qualified Glean.LocalOrRemote as Glean
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider (
  ConfigProvider (withConfigProvider),
  withConfigOptions,
 )

main :: IO ()
main = withConfigOptions options $ \(Options {..}, cfg) ->
  withEventBaseDataplane $ \evb ->
    withConfigProvider cfg $ \(cfgAPI :: ConfigAPI) ->
      withBackend evb cfgAPI service (Just schema_id) id $ \backend -> do
        derive backend repo "." (showt . hash)

data Options = Options
  { service :: Glean.Service
  , repo :: Glean.Repo
  }

description :: String
description = unwords
  [ "Language agnostic external deriver to generate Digests for various"
  , "entities."
  , "Digests are produced by hashing source ranges."
  , "The deriver expects to find the source code in the file system,"
  , "and relative paths in src.File facts are resolved from"
  , "the current working directory."
  ]

options :: ParserInfo Options
options = info (parser <**> helper) (fullDesc <> progDesc description)
  where
    parser = do
      service <- Glean.options
      repo <-
        option
          (maybeReader Glean.parseRepo)
          ( long "db"
              <> metavar "NAME/INSTANCE"
              <> help "database to extend"
          )
      return Options {..}

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
import Data.List.NonEmpty (nonEmpty, NonEmpty)
import Data.Text (pack)
import Options.Applicative (
  ParserInfo,
  auto,
  fullDesc,
  help,
  helper,
  info,
  long,
  many,
  maybeReader,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strArgument,
  value,
  (<**>),
 )
import System.FilePath (joinPath, splitFileName, splitPath, (</>))
import TextShow (showt)

import Util.EventBase (withEventBaseDataplane)

import Glean (
  Repo,
  parseRepo,
 )
import Glean.Derive.Digest.Lib (Config (..), FileFact, derive, replaceName)
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.LocalOrRemote (withBackend)
import qualified Glean.LocalOrRemote as Glean
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider (
  ConfigProvider (withConfigProvider),
  withConfigOptions,
 )
import Glean.Util.ShowSchemaId
import qualified Glean.Glass.Types as Glass

main :: IO ()
main = withConfigOptions options $ \(Options {..}, cfg) ->
  withEventBaseDataplane $ \evb ->
    withConfigProvider cfg $ \(cfgAPI :: ConfigAPI) ->
      withBackend evb cfgAPI service (Just schema_id) id $ \backend -> do
        let deriveConfig =
              Config
                { pathAdaptor = stripPath stripDepth
                , hashFunction = \name code ->
                    showt $ hash $
                    -- just hash the unrenamed code in case of failure
                    either (const code) id $
                    replaceName name (Glass.Name "") code
                , indexOnly = indexOnly
                }
        derive backend repo deriveConfig

-- | stripPath 1 "www/take/me/home.php"  = "take/me/home.php"
stripPath :: Int -> FilePath -> FilePath
stripPath depth path = stripped_body </> file
  where
    (body, file) = splitFileName path
    stripped_body = joinPath $ drop depth $ splitPath body

data Options = Options
  { service :: Glean.Service
  , repo :: Glean.Repo
  , stripDepth :: Int
  , indexOnly :: Maybe (NonEmpty FileFact)
  }

description :: String
description =
  unwords
    [ "Language agnostic external deriver to generate Digests for various"
    , "entities."
    , "Digests are produced by hashing source ranges."
    , "The deriver expects to find the source code in the file system,"
    , "and relative paths in src.File facts are resolved from"
    , "the current working directory."
    ]

options :: ParserInfo Options
options = info
  (parser <**> helper <**> showSchemaId)
  (fullDesc <> progDesc description)
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
      stripDepth <-
        option
          auto
          ( long "strip"
              <> short 'p'
              <> metavar "NUM"
              <> help "Strip NUM leading components from file names."
              <> value 0
              <> showDefault
          )
      indexOnly <- nonEmpty <$>
        many
          (pack <$> strArgument (help "index-only"
          <> metavar "FILE"
          <> help "Restrict indexing to these files"))

      return Options {..}

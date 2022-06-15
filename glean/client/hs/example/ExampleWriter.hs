{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}

-- Example Glean client to demonstrate creating a DB and writing some
-- facts to it.

module ExampleWriter (main) where

import Control.Exception
import Control.Monad
import qualified Data.Text as Text
import Options.Applicative

import Util.EventBase

import qualified Glean
import Glean.BuildInfo
import Glean.Impl.ConfigProvider
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Schema.Src as Src
import Glean.Schema.Src.Types as Src
import Glean.Schema.Cxx1 as Cxx
import Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.ConfigProvider

newtype Config = Config
  { cfgService :: Glean.ThriftSource Glean.ClientConfig
  }

options :: ParserInfo Config
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(cfg, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
  Glean.withRemoteBackend evb cfgAPI (cfgService cfg) (Just schema_id)
    create


create :: Glean.Backend b => b -> IO ()
create backend =
  Glean.fillDatabase backend Nothing repo handle ifexists write
  where
  -- the handle can be anything, but we'll use some details
  -- about the binary since that might be useful for debugging
  handle = buildRule <> "@" <> buildRevision

  -- name/hash of the repo to create
  repo = Glean.Repo "example" "0"

  ifexists = throwIO $ ErrorCall "database exists"

  write = do
    let predicates = [ Src.allPredicates, Cxx.allPredicates ]
    Glean.basicWriter backend repo predicates $ do
      Glean.makeFact_ @Src.File "Foo.hs"
      forM_ [ [a,b] | a <- ['a'..'z'], b <- ['0'..'9'] ] $ \str -> do
        name <- Glean.makeFact @Cxx.Name (Text.pack str)
        Glean.makeFact_ @Cxx.FunctionName (Cxx.FunctionName_key_name name)

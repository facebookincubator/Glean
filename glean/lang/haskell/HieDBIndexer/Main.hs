{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module HieDBIndexer.Main where

import qualified Glean.LocalOrRemote as Glean
import Glean.Impl.ConfigProvider (ConfigAPI)
import Glean.Init (withOptions)
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Util.ConfigProvider
import HieDBIndexer.DefaultMain
import qualified HieDBIndexer.Options as HieDB
import HieDBIndexer.Trace (vlogTextTracer)
import Options.Applicative
import Util.EventBase

main :: IO ()
main = do
  let tracer = vlogTextTracer 0
      parser =
        (,) <$> infoParser HieDB.options <*> Glean.options
      opts = info (helper <*> parser) fullDesc
  withOptions opts $ \((cfg, mode), svc) ->
    withEventBaseDataplane $ \evb ->
      withConfigProvider defaultConfigOptions $ \(cfgAPI :: ConfigAPI) ->
        Glean.withBackendWithDefaultOptions
          evb
          cfgAPI
          svc
          (Just schema_id)
          $ case mode of
              HieDB.BinaryMode{..} ->
                outputMain tracer cfg outputPath
              HieDB.WriteMode{..} ->
                defaultMain tracer cfg repo dontCreateDb

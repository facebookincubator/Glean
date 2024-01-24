{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module GleanCLI.Types
  ( Plugin(..)
  ) where

import Control.Exception
import Options.Applicative

import Util.EventBase
import Glean.Database.Env (withDatabases)
import Glean.LocalOrRemote as Glean
    ( Service(..), LocalOrRemote, withBackendWithDefaultOptions )
import Glean.Impl.ConfigProvider
import qualified Glean.ServerConfig.Types as Server
import Glean.Util.ConfigProvider

#if GLEAN_FACEBOOK
import Glean.Database.Config
import Glean.Facebook.Logger.Database
import Glean.Util.Some
import Logger.IO (withLogger)
#endif

class Plugin c where
  parseCommand :: Parser c

  -- | Allows for transforming the command-line args that get passed
  -- to Gflags. This can be used to add --minloglevel=N for example.
  argTransform :: c -> [String] -> [String]
  argTransform _ = id

  -- | Allows transforming the server config to e.g. disable the janitor.
  --   Only relevant when using a local DB root and setting a server config
  serverConfigTransform :: c -> (Server.Config -> Server.Config)
  serverConfigTransform _ = id

  runCommand
    :: (Glean.LocalOrRemote backend, ConfigProvider cfg)
    => EventBaseDataplane
    -> cfg
    -> backend
    -> c
    -> IO ()
  runCommand _ _ _ _ =
    throwIO $ ErrorCall "runCommand implementation missing"

  -- | override this instead of runCommand if you need to do your own
  -- withBackend stuff.
  withService
    :: EventBaseDataplane
    -> ConfigAPI
    -> Glean.Service
    -> c
    -> IO ()
  withService evb cfgAPI svc c = case svc of
    Glean.Remote{} ->
      Glean.withBackendWithDefaultOptions evb cfgAPI svc Nothing $ \b ->
        runCommand evb cfgAPI b c
    Glean.Local cfg _ ->
#if GLEAN_FACEBOOK
      withLogger cfgAPI $ \logger -> do
        cfg <- pure cfg{cfgDatabaseLogger =
                          Some (GleanDatabaseFacebookLogger logger)
                      }
#endif
        withDatabases evb cfg cfgAPI $ \env ->
          runCommand evb cfgAPI env c

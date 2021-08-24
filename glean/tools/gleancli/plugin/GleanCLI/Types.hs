-- Copyright (c) Facebook, Inc. and its affiliates.

module GleanCLI.Types
  ( Plugin(..)
  ) where

import Control.Exception
import Options.Applicative

import Util.EventBase
import qualified Glean.LocalOrRemote as Glean
import Glean.Impl.ConfigProvider
import Glean.Util.ConfigProvider

class Plugin c where
  parseCommand :: Parser c

  -- | Allows for transforming the command-line args that get passed
  -- to Gflags. This can be used to add --minloglevel=N for example.
  argTransform :: c -> [String] -> [String]
  argTransform _ = id

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
  withService evb cfgAPI svc c =
    Glean.withBackendWithDefaultOptions evb cfgAPI svc $ \backend ->
      runCommand evb cfgAPI backend c

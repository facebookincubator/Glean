-- Copyright (c) Facebook, Inc. and its affiliates.

module GleanCLI.Types
  ( Plugin(..)
  ) where

import Options.Applicative

import Util.EventBase
import qualified Glean.LocalOrRemote as Glean
import Glean.Util.ConfigProvider

class Plugin c where
  parseCommand :: Parser c

  runCommand
    :: (Glean.LocalOrRemote backend, ConfigProvider cfg)
    => EventBaseDataplane
    -> cfg
    -> backend
    -> c
    -> IO ()

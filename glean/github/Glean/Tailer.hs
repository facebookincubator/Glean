-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Tailer
  ( TailerOptions(..)
  , options
  , runTailer
  ) where

import Control.Exception
import Data.Default
import Options.Applicative

import Glean.Tailer.Types

data TailerOptions = TailerOptions

instance Default TailerOptions where
  def = TailerOptions

options :: Parser TailerOptions
options = pure TailerOptions

runTailer :: RunTailer TailerOptions
runTailer _ _ = throwIO $ ErrorCall $
  "tailers are not supported in this build"

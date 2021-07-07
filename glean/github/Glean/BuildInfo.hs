-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}
module Glean.BuildInfo (module Glean.BuildInfo) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time
import Language.Haskell.TH
import System.Exit
import System.Process

buildRule :: Text
buildRule = "glean"

buildMode :: Text
buildMode = "cabal"

buildTimeISO8601 :: Text
buildTimeISO8601 = $(do
  now <- liftIO getCurrentTime
  litE (stringL (show now))
  )

buildRevision :: Text
buildRevision = $(do
  (exit, rev, _) <- liftIO $
    readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
  case exit of
    ExitSuccess -> litE (stringL (head (lines rev)))
    _ -> litE (stringL "<unknown>")
  )

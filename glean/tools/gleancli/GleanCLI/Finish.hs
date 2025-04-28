{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Finish (FinishCommand, finished) where

import Control.Monad
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import Glean.LocalOrRemote

data FinishCommand
  = Finish
      { finishRepo :: Repo
      , allowZeroFacts :: Bool
      }

instance Plugin FinishCommand where
  parseCommand =
    commandParser "finish"
      (progDesc "Notify server that a database is complete") $ do
      finishRepo <- dbOpts
      allowZeroFacts <- switch
        (  long "allow-zero-facts"
        <> help "Allow creating a db without any facts"
        )
      return Finish{..}

  runCommand _ _ backend Finish{..} = do
    when (not allowZeroFacts) $ do
      stats <- Glean.predicateStats backend finishRepo Glean.ExcludeBase
      when (null stats) $
        die 6
          "finish: Database has no facts. Use --allow-zero-facts to allow this"
    finished backend finishRepo

finished
  :: LocalOrRemote b
  => b
  -> Repo
  -> IO ()
finished backend repo = Glean.finish backend repo

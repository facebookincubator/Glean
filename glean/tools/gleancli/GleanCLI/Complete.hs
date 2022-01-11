{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Complete (CompleteCommand) where

import Control.Monad
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import Glean.Schema.Util

data CompleteCommand
  = Complete
      { completeRepo :: Repo
      , completePredicates :: [SourceRef]
      }

instance Plugin CompleteCommand where
  parseCommand =
    commandParser "complete"
      (progDesc "Notify server that some predicates are complete.") $ do
      completeRepo <- repoOpts
      completePredicates <-
        many $ fmap parseRef $ strArgument (metavar "PREDICATE")
      return Complete{..}

  runCommand _ _ backend Complete{..} = do
    when (not (null completePredicates)) $
      die 1 "completing individual predicates is not supported yet"

    void $ Glean.completePredicates backend completeRepo

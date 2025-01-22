{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Complete (CompleteCommand) where

import Control.Monad
import Data.Default (def)
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import qualified Glean.Types as Thrift

data CompleteCommand
  = Complete
      { completeRepo :: Repo
      , completePredicates :: [SourceRef]
      }

instance Plugin CompleteCommand where
  parseCommand =
    commandParser "complete"
      (progDesc "Notify server that some predicates are complete.") $ do
      completeRepo <- dbOpts
      completePredicates <-
        many $ parseRef <$> strArgument
        (  metavar "PREDICATE"
        <> help ("If predicate is not specified, then all non-derived "
        <> "predicates will be marked as complete")
        )
      return Complete{..}

  runCommand _ _ backend Complete{..} = do
    when (not (null completePredicates)) $
      die 1 "completing individual predicates is not supported yet"

    void $ Glean.completePredicates backend completeRepo $
      Thrift.CompletePredicates_axiom def

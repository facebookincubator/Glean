{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Derive (DeriveCommand) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Control.Concurrent.Stream (stream)
import Util.OptParse
import Util.Text

import Glean (Repo)
import Glean.Derive
import Glean.Types (ParallelDerivation(..))
import Glean.Write

import GleanCLI.Types
import GleanCLI.Common

data DeriveCommand
  = Derive
      { deriveRepo :: Repo
      , predicates :: [(Text, Maybe ParallelDerivation)]
      , derivePageOptions :: PageOptions
      , deriveMaxConcurrency :: Int
      }

instance Plugin DeriveCommand where
  parseCommand =
    commandParser "derive" (progDesc "Derive and store a predicate") $ do
      deriveRepo <- dbOpts
      deriveMaxConcurrency <- maxConcurrencyOpt
      derivePageOptions <- pageOpts
      predicates <- many (serial <|> parallel)
      return Derive{..}
    where
    parallel = option parseParallel
      ( long "parallel"
      <> metavar "PREDICATE,OUTER[,SIZE],QUERY"
      <> help "Derive a predicate in parallel"
      )

    parseParallel = maybeReader $ \s ->
      case Text.splitOn "," (Text.pack s) of
        pred : outer : size : rest | Right n <- textToInt size -> Just
          (pred,
             Just ParallelDerivation
               { parallelDerivation_outer_predicate = outer
               , parallelDerivation_inner_query = Text.intercalate "," rest
               , parallelDerivation_min_batch_size = Just (fromIntegral n) })
        pred : outer : rest -> Just
          (pred,
             Just ParallelDerivation
               { parallelDerivation_outer_predicate = outer
               , parallelDerivation_inner_query = Text.intercalate "," rest
               , parallelDerivation_min_batch_size = Nothing })
        _ -> Nothing

    serial = (,Nothing) <$> strArgument
      ( metavar "PREDICATE"
      <> help "predicates to derive"
      )

  runCommand _ _ backend Derive{..} =
    let threads = min deriveMaxConcurrency (length predicates) in
    stream threads (forM_ predicates) $ \(pred, parallel) -> do
      derivePredicate backend deriveRepo
        (Just $ fromIntegral $ pageBytes derivePageOptions)
        (fromIntegral <$> pageFacts derivePageOptions)
        (parseRef pred)
        parallel

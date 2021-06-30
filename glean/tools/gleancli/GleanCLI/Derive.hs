-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Derive (DeriveCommand) where

import Control.Monad
import Data.Text (Text)
import Options.Applicative

import Control.Concurrent.Stream (streamWithThrow)
import Util.OptParse

import Glean (Repo)
import Glean.Derive
import Glean.Write

import GleanCLI.Types
import GleanCLI.Common

data DeriveCommand
  = Derive
      { deriveRepo :: Repo
      , predicates :: [Text]
      , derivePageOptions :: PageOptions
      , deriveMaxConcurrency :: Int
      }

instance Plugin DeriveCommand where
  parseCommand =
    commandParser "derive" (progDesc "Derive and store a predicate") $ do
      deriveRepo <- repoOpts
      deriveMaxConcurrency <- maxConcurrencyOpt
      derivePageOptions <- pageOpts
      predicates <- many $ strArgument
        ( metavar "PREDICATE"
        <> help "predicates to derive"
        )
      return Derive{..}

  runCommand _ _ backend Derive{..} =
    let threads = min deriveMaxConcurrency (length predicates) in
    streamWithThrow threads (forM_ predicates) $ \pred ->
      derivePredicate backend deriveRepo
        (Just $ fromIntegral $ pageBytes derivePageOptions)
        (fromIntegral <$> pageFacts derivePageOptions)
        (parseRef pred)

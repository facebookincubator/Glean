{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Derive
  ( derivePredicate
  ) where

import Control.Concurrent
import Data.Default
import Data.Int
import qualified Data.Text as Text

import Glean.Angle.Types
import Glean.Backend.Types as Glean
import Glean.Repo.Text (showRepo)
import Glean.Query.Thrift.Internal
import Glean.Types
import Glean.Schema.Util (showRef)
import Util.Log

-- | Compute and store the specified derived predicate
derivePredicate
  :: Glean.Backend b
  => b
  -> Repo
  -> Maybe Int64  -- ^ page size (bytes)
  -> Maybe Int64  -- ^ page size (results)
  -> SourceRef
     -- ^ predicate to derive. This will be resolved using the schema
     -- specified by the `glean.schema_id` property when the DB was
     -- created; otherwise the current schema at the time.
  -> Maybe ParallelDerivation -- ^ how to derive in parallel
  -> IO ()

derivePredicate backend repo maxBytes maxResults s parallel = loop
  where
    loop = do
      result <- Glean.deriveStored backend (const mempty) repo query
      case result of
        DerivationStatus_complete stats ->
          report "complete" $ derivationComplete_stats stats
        DerivationStatus_ongoing x -> do
          report "progress" $ derivationOngoing_stats x
          retry loop

    query = def
      { derivePredicateQuery_predicate = name
      , derivePredicateQuery_predicate_version = version
      , derivePredicateQuery_options = Just def
        { derivePredicateOptions_max_results_per_query = maxResults
        , derivePredicateOptions_max_bytes_per_query = maxBytes
        }
      , derivePredicateQuery_parallel = parallel
      }

    SourceRef name version = s

    predicate = unwords [showRepo repo, Text.unpack $ showRef s]

    report status stats = do
      putStrLn $ unwords
        [ Text.unpack $ showRef s
        , ":"
        , show $ userQueryStats_num_facts stats
        , "facts"
        ]
      vlog 1 $ unwords
        ["derivation"
        , status
        , ":"
        , predicate
        , showUserQueryStats stats
        ]

    retry :: IO a -> IO a
    retry action = do
      let sec = 1000000
      threadDelay $ 1 * sec
      action


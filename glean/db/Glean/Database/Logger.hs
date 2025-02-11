{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- {-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Glean.Database.Logger (
  logDBStatistics
) where

import Data.Text (Text)

import qualified Glean.Database.Types as Database
import qualified Glean.Types as Thrift
import Glean.Logger.Database as Logger
import Glean.RTS.Foreign.Ownership (OwnershipStats(..))
import Glean.Types
import Glean.Util.Some

logDBStatistics
  :: Database.Env -- ^ Environment
  -> Thrift.Repo  -- ^ Repo of interest
  -> [(PredicateRef, Thrift.PredicateStats)]
                  -- ^ Digested stats per query in database
  -> Maybe OwnershipStats
  -> Int          -- ^ Number of bytes backed up
  -> Text         -- ^ Backup locator
  -> IO ()
logDBStatistics env Thrift.Repo{..} preds maybeOwnershipStats size locator = do
  let preamble = mconcat
        [ Logger.SetRepoName repo_name
        , Logger.SetRepoHash repo_hash
        ]

  -- We shoehorn a summary row into the format for query rows
  let summary  = mconcat
        [ Logger.SetPredicateSize size            -- # bytes uploaded
        , Logger.SetPredicateCount factCount      -- # total facts
        , Logger.SetUploadDestination locator
        ]
      factCount = fromIntegral $ sum [predicateStats_count p| (_, p) <- preds]

  let queries  =
        [ mconcat
          [ Logger.SetPredicateName predicateRef_name
          -- TODO: add predicate hash
          , Logger.SetPredicateVersion (fromIntegral predicateRef_version)
          , Logger.SetPredicateCount $ fromIntegral predicateStats_count
          , Logger.SetPredicateSize $ fromIntegral predicateStats_size
          ]
        | (PredicateRef{..}, Thrift.PredicateStats{..}) <- preds
        ]

  let ownership
        | Just OwnershipStats{..} <- maybeOwnershipStats =
          [
            mconcat
              [ Logger.SetMetric "ownership_units"
              , Logger.SetCount $ fromIntegral numUnits
              , Logger.SetSize $ fromIntegral unitsSize
              ],
            mconcat
              [ Logger.SetMetric "ownership_sets"
              , Logger.SetCount $ fromIntegral numSets
              , Logger.SetSize $ fromIntegral setsSize
              ],
            mconcat
              [ Logger.SetMetric "ownership_fact_owners"
              , Logger.SetCount $ fromIntegral numOwnerEntries
              , Logger.SetSize $ fromIntegral ownersSize
              ],
            mconcat
              [ Logger.SetMetric "ownership_orphan_facts"
              , Logger.SetCount $ fromIntegral numOrphanFacts
              ]
          ]
        | otherwise = []

  let logStats log = case Database.envDatabaseLogger env of
        Some logger -> Logger.runLog logger $ preamble <> log

  -- We want a new row in the log table for the summary, and a new row
  -- for each query
  mapM_ logStats (summary:queries <> ownership)

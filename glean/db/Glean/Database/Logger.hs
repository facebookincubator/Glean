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

import qualified Glean.Database.Types as Database
import qualified Glean.Types as Thrift
import Glean.Logger.Database as Logger
import Glean.Types
import Glean.Util.Some


logDBStatistics
  :: Database.Env -- ^ Environment
  -> Thrift.Repo  -- ^ Repo of interest
  -> [(PredicateRef, Thrift.PredicateStats)]
                  -- ^ Digested stats per query in database
  -> Int          -- ^ Number of bytes backed up
  -> IO ()
logDBStatistics env Thrift.Repo{..} preds size = do
  let preamble = mconcat
        [ Logger.SetRepoName repo_name
        , Logger.SetRepoHash repo_hash
        ]

  -- We shoehorn a summary row into the format for query rows
  let summary  = mconcat
        [ Logger.SetPredicateCount $ length preds -- # queries
        , Logger.SetPredicateSize size            -- # bytes uploaded
        , Logger.SetUploadDestination "<TBD>"     -- TODO: not sure how to
                                                  -- determine this yet
        ]

  let queries  =
        [ mconcat
          [ Logger.SetPredicateName predicateRef_name
          , Logger.SetPredicateVersion 0
          -- TODO: add predicate hash
          , Logger.SetPredicateCount $ fromIntegral predicateStats_count
          , Logger.SetPredicateSize $ fromIntegral predicateStats_size
          ]
        | (PredicateRef{..}, Thrift.PredicateStats{..}) <- preds
        ]

  let logStats log = case Database.envDatabaseLogger env of
        Some logger -> Logger.runLog logger $ preamble <> log

  -- We want a new row in the log table for the summary, and a new row
  -- for each query
  mapM_ logStats (summary:queries)

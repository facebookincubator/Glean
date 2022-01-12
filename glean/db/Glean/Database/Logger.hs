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
import Glean.Angle.Types (PredicateRef(..))
import qualified Glean.Types as Thrift
import qualified Logger.GleanDatabaseStats as Logger


logDBStatistics
  :: Database.Env -- ^ Environment
  -> Thrift.Repo  -- ^ Repo of interest
  -> [(PredicateRef, Thrift.PredicateStats)]
                  -- ^ Digested stats per query in database
  -> Int          -- ^ Number of bytes backed up
  -> IO ()
logDBStatistics env Thrift.Repo{..} preds size = do
  let preamble = mconcat
        [ Logger.setRepoName repo_name
        , Logger.setRepoHash repo_hash
        ]

  -- We shoehorn a summary row into the format for query rows
  let summary  = mconcat
        [ Logger.setPredicateCount $ length preds -- # queries
        , Logger.setPredicateSize size            -- # bytes uploaded
        , Logger.setUploadDestination "<TBD>"     -- TODO: not sure how to
                                                  -- determine this yet
        ]

  let queries  =
        [ mconcat
          [ Logger.setPredicateName predicateRef_name
          , Logger.setPredicateVersion $ fromIntegral predicateRef_version
          , Logger.setPredicateCount $ fromIntegral predicateStats_count
          , Logger.setPredicateSize $ fromIntegral predicateStats_size
          ]
        | (PredicateRef{..}, Thrift.PredicateStats{..}) <- preds
        ]

  let logStats log = Logger.runLog (Database.envLogger env) $ preamble <> log

  -- We want a new row in the log table for the summary, and a new row
  -- for each query
  mapM_ logStats (summary:queries)

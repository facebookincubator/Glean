-- Copyright (c) Facebook, Inc. and its affiliates.

module Logger.GleanDatabaseStats (module Logger.GleanDatabaseStats) where

import Data.Monoid
import Data.Semigroup
import Data.Text

import Logger.IO

data GleanDatabaseStatsLogger = GleanDatabaseStatsLogger

instance Semigroup GleanDatabaseStatsLogger where
  _ <> _ = GleanDatabaseStatsLogger

instance Monoid GleanDatabaseStatsLogger where
  mempty = GleanDatabaseStatsLogger

setRepoName :: Text -> GleanDatabaseStatsLogger
setRepoName _ = GleanDatabaseStatsLogger

setRepoHash :: Text -> GleanDatabaseStatsLogger
setRepoHash _ = GleanDatabaseStatsLogger

setPredicateCount ::Int -> GleanDatabaseStatsLogger
setPredicateCount _ = GleanDatabaseStatsLogger

setUploadDestination :: Text -> GleanDatabaseStatsLogger
setUploadDestination _ = GleanDatabaseStatsLogger

setPredicateName :: Text-> GleanDatabaseStatsLogger
setPredicateName _ = GleanDatabaseStatsLogger

setPredicateVersion :: Int -> GleanDatabaseStatsLogger
setPredicateVersion _ = GleanDatabaseStatsLogger

setPredicateSize :: Int -> GleanDatabaseStatsLogger
setPredicateSize _ = GleanDatabaseStatsLogger

runLog :: Logger -> GleanDatabaseStatsLogger -> IO ()
runLog _ _ = return ()

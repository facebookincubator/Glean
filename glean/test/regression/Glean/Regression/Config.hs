-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Glean.Regression.Config
  ( TestConfig(..)
  ) where

import Glean.Types

-- | Test configuration
data TestConfig = TestConfig
  { testRepo :: Repo
  , testOutput :: FilePath
      -- ^ directory in which to store *all* output and temporary files
  , testRoot :: FilePath
      -- ^ directory with source files to index
  , testProjectRoot :: FilePath
      -- ^ top-level directory (fbsource/fbcode)
  , testGroup :: String
      -- ^ test group. Groups are used to run the whole set of tests
      -- multiple ways, e.g. for different platforms.
  , testSchemaVersion :: Maybe Int
      -- ^ version of 'all' schema to use in test DB
  } deriving (Show)

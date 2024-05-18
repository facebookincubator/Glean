{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module IncrementalTest (main) where

import qualified Data.ByteString.UTF8 as UTF8
import Data.Default
import System.Directory
import System.Exit
import System.FilePath

import Glean
import Glean.Regression.Indexer
import Glean.Regression.Config
import Glean.Regression.Snapshot (testMain)
import Glean.Regression.Snapshot.Driver
import qualified Glean.Clang.Test as Clang
import qualified Glean.Clang.Test.DerivePass as DerivePass
import qualified Glean.Regression.Driver.DeriveForCodemarkup as Code

main :: IO ()
main = testMain incrementalClangDriver

incrementalClangDriver :: Driver Clang.Options
incrementalClangDriver = clangDriver {
    driverCreateDatabase = createDB,
    driverIndexer = driverIndexer clangDriver
  }
  where
  clangDriver = DerivePass.driver Code.codemarkupDerivePasses

  createDB opts backend indexer test = do
    let
      repo = testRepo test
      base = repo { repo_name = (repo_name repo) <> "_base" }
      baseTestConfig = test
        { testRepo = base
        , testRoot = testRoot test </> "old"
        }

    fillDatabase backend base "" Nothing (die "repo already exists") $
      runIndexerForTest backend (indexer opts) baseTestConfig

    -- discover which files are in the new version
    let isCppFile = (`elem` [".h", ".cpp"]) . takeExtension
    files <- filter isCppFile <$> listDirectory (testRoot test)

    let deps =
          Dependencies_pruned def {
            pruned_base = base,
            pruned_units = map UTF8.fromString files,
            pruned_exclude = True
          }
    fillDatabase backend repo "" (Just deps) (die "repo already exists") $
      runIndexerForTest backend (indexer opts { Clang.clangIncremental = True }) test

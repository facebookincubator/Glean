{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Test.Repos (main) where

import Test.HUnit (Test(..), assertEqual)
import Data.Text (Text)
import qualified Data.Map as Map

import Glean.Glass.Base
import Glean.Glass.Types
import Glean.Glass.Repos
import TestRunner (testRunner)
import Glean.Init (withUnitTest)

main :: IO ()
main = withUnitTest $ testRunner $ TestList unitTests

unitTests :: [Test]
unitTests = [
  TestLabel "no-branch-no-language-setup" $
    TestCase $
      assertEqual "No filters should be applied"
        ["db1", "db2", "db3"]
        $ callRepoMapping "repo1" Nothing Nothing
  ,
  TestLabel "branch-and-language-setup" $
    TestCase $
      assertEqual "The matching branch and language db should be returned"
        ["db2"]
        $ callRepoMapping "repo1" (Just Language_Rust) (Just "branch2")
  ,
  TestLabel "unknown-branch-setup" $
    TestCase $
      assertEqual "The db with no specified branch should be returned"
        ["db3"]
        $ callRepoMapping "repo1" (Just Language_Cpp) (Just "unknownBranch")
  ,
  TestLabel "unknown-language" $
    TestCase $
      assertEqual "No db should be returned if language doesn't match"
        []
        $ callRepoMapping "repo1" (Just Language_Swift) (Just "branch1")
  ]

callRepoMapping
  :: Text
  -> Maybe Language
  -> Maybe Text
  -> [GleanDBName]
callRepoMapping repoName maybeLanguage maybeBranch =
  fromSCSRepo repoMapping
    (RepoName repoName)
    maybeBranch
    maybeLanguage

repoMapping :: RepoMapping
repoMapping = RepoMapping
  { gleanIndices = genGleanIndices
  , gleanAttrIndices = Map.empty
  }

genGleanIndices :: Map.Map RepoName [GleanDBSelector]
genGleanIndices = Map.fromList
  [ ( RepoName "repo1",
      [ GleanDBSelector
        { dbName = "db1"
        , language = Language_Cpp
        , branchName = Just "branch1"
        }
        , GleanDBSelector
        { dbName = "db2"
        , language = Language_Cpp
        , branchName = Just "branch2"
        }
        , GleanDBSelector
        { dbName = "db2"
        , language = Language_Rust
        , branchName = Just "branch2"
        }
        , GleanDBSelector
        { dbName = "db3"
        , language = Language_Cpp
        , branchName = Nothing
        }
      ]
    )
  ]

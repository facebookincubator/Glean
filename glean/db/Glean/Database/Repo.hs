{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Repo
  ( databaseSubdir
  , databasePath
  , inRepo
  ) where

import System.FilePath

import Glean.Repo.Text
import Glean.Types (Repo(..))

databaseSubdir :: Repo -> String
databaseSubdir = showRepoSep "/"

inRepo :: Repo -> String -> String
inRepo repo s = databaseSubdir repo ++ ": " ++ s

databasePath :: FilePath -> Repo -> FilePath
databasePath root repo = root </> databaseSubdir repo

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Path where

import qualified Data.Text as Text

import Glean.Glass.Base ( SymbolRepoPath(..), GleanPath(GleanPath) )
import qualified Glean.Glass.Types as Glass

-- TODO: We probably want to read the repo config from a file, or put it in a
-- server config

-- Convert repo-relative Glass normalized paths to Glean-index specific paths
toGleanPath :: SymbolRepoPath -> GleanPath
toGleanPath
  rp@SymbolRepoPath{symbolRepo=Glass.RepoName "react"} (Glass.Path path) =
    GleanPath ("react/" <> symbolPath rp)
toGleanPath SymbolRepoPath{..} = GleanPath symbolPath

-- | Site-level rules for processing index paths to the filesystem
-- Glass paths are always repo-root relative.
fromGleanPath
  :: Glass.RepoName -> GleanPath -> SymbolRepoPath
fromGleanPath repo@(Glass.RepoName "react") (GleanPath path) =
  SymbolRepoPath repo $ case Text.stripPrefix "react/" path of
    Just suff -> Glass.Path suff
    Nothing -> Glass.Path path

fromGleanPath repo (GleanPath path) = SymbolRepoPath repo $ Glass.Path path

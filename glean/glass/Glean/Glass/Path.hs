module Glean.Glass.Path where

import Data.Text

import Glean.Glass.Base
import qualified Glean.Glass.Types as Glass

-- Convert repo-relative Glass normalized paths to Glean-index specific paths
toGleanPath :: Glass.RepoName -> Glass.Path -> GleanPath
toGleanPath _ = GleanPath . Glass.unPath

-- | Site-level rules for processing index paths to the filesystem
-- Glass paths are always repo-root relative
fromGleanPath :: Glass.RepoName -> GleanPath -> Glass.Path
fromGleanPath _ = Glass.Path . gleanPath

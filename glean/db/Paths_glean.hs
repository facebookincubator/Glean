{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Stand-in for Cabal's auto-generated Paths_glean module (see
-- Distribution.Simple.Build.PathsModule) - buck2 has no equivalent
-- codegen step. Only 'getDataDir' is implemented, since that's the only
-- function Glean.Database.Config actually uses (to resolve the "$datadir"
-- placeholder in a schema location - see schemaLocation there). Cabal's own
-- generated version resolves to a `cabal install`-time prefix, itself
-- overridable via a `glean_datadir` environment variable; here the fallback
-- is the current directory instead, since a buck2 action always runs with
-- cwd = repo root (see buck2/thrift.bzl's comment on this), where
-- glean/schema/source actually lives - the env var override still works
-- the same way for anyone who needs a different value.
module Paths_glean (getDataDir) where

import Control.Exception (catch, IOException)
import System.Environment (getEnv)

getDataDir :: IO FilePath
getDataDir = getEnv "glean_datadir" `catch` \(_ :: IOException) -> return "."

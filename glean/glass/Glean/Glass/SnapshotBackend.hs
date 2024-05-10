{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-
A snapshot backend is a DB in which we store precomputed results
of some Glass queries. This isn't implemented yet in the open source version
of Glass
-}

module Glean.Glass.SnapshotBackend
  ( SnapshotBackend(..)
  , SnapshotStatus(..)
  , snapshotBackendParser
  , NilSnapshotBackend(..)
  ) where

import Data.Text ( Text )
import Options.Applicative
    ( Parser, auto, help, long, option, value )

import qualified Glean.Glass.Types as Types
import Glean.Util.Some (Some(..))
import Glean.Glass.SourceControl (ScmGeneration)
import Glean.Glass.Types (
      Revision,
      Path,
      RepoName(..),
      Path (..) )
import Glean.Glass.Tracing (GlassTracer)

class SnapshotBackend backend where
  getSnapshot
    :: GlassTracer
    -> backend
    -> RepoName
    -> Path
    -> Maybe Revision
    -> Maybe ScmGeneration
    -> IO (Either SnapshotStatus (Revision, IO (Maybe Types.DocumentSymbolListXResult)))

instance SnapshotBackend (Some SnapshotBackend) where
  getSnapshot t (Some backend) = getSnapshot t backend

data SnapshotStatus
  = Unrequested
  | DbError
  | InternalError
  | Timeout
  | NotFound
  | ExactMatch
  | CompatibleMatch
  | Ignored
  | Latest
  deriving Show

-- | Always produces the 'NilSnapshotBackend'
snapshotBackendParser :: Parser (Some SnapshotBackend)
snapshotBackendParser = Some NilSnapshotBackend <$ (option auto (mconcat
  [ long "snapshot-tier-name"
  , help "snapshot tier name (unused)"
  , value "unused"
  ]) :: Parser Text)

data NilSnapshotBackend = NilSnapshotBackend

instance SnapshotBackend NilSnapshotBackend where
  getSnapshot _ _ _ _ _ _ = return $ Left Unrequested

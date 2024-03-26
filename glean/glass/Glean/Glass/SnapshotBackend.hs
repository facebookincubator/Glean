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
import Glean.Glass.Types (
      Revision,
      Path,
      RepoName(..),
      Path (..) )

class SnapshotBackend backend where
  getSnapshot
    :: backend
    -> RepoName
    -> Path
    -> Maybe Revision
    -> IO (Either SnapshotStatus Types.DocumentSymbolListXResult)

instance SnapshotBackend (Some SnapshotBackend) where
  getSnapshot (Some backend) = getSnapshot backend

data SnapshotStatus
  = Unrequested
  | DbError
  | InternalError
  | Timeout
  | NotFound
  | ExactMatch
  | Ignored
  | Latest Revision
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
  getSnapshot _ _ _ _ = return $ Left Unrequested

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
  ( getSnapshot
  , snapshotBackend
  , SnapshotBackend
  , SnapshotTier
  , snapshotTierParser
  , snapshotDefaultTier
  ) where

import Data.Text ( Text )
import qualified Glean.Glass.Types as Types

import Options.Applicative
    ( Parser, auto, help, long, option, value )

import Glean.Glass.Types (
      Revision,
      Path,
      RepoName(..),
      Path (..) )

type SnapshotTier = ()
type SnapshotBackend = ()

snapshotDefaultTier :: SnapshotTier
snapshotDefaultTier = ()

snapshotBackend :: SnapshotTier -> SnapshotBackend
snapshotBackend _ = ()

snapshotTierParser :: Parser SnapshotTier
snapshotTierParser = () <$ (option auto (mconcat
  [ long "snapshot-tier-name"
  , help "snapshot tier name (unused)"
  , value "unused"
  ]) :: Parser Text)

getSnapshot
  :: SnapshotBackend
  -> RepoName
  -> Path
  -> Revision
  -> IO (Maybe Types.DocumentSymbolListXResult)
getSnapshot _ _ _ _ = return Nothing

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Open-source no-op stub. The real implementation (write-server backup of
-- Incomplete databases on graceful shutdown, and restore on startup) depends
-- on Meta-internal infrastructure and lives under @glean/facebook@.
module Glean.Database.Backup.Incomplete
  ( backupIncompleteDatabasesOnShutdown
  , restoreIncompleteDatabasesOnStartup
  ) where

backupIncompleteDatabasesOnShutdown :: env -> IO ()
backupIncompleteDatabasesOnShutdown _ = return ()

restoreIncompleteDatabasesOnStartup :: env -> IO ()
restoreIncompleteDatabasesOnStartup _ = return ()

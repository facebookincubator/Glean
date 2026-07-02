{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module GleanCLI.Utils
  ( disableJanitor
  , disableAutoBackups
  , setBackupUseCheckpoint
  ) where

import Glean.ServerConfig.Types

disableJanitor :: Config -> Config
disableJanitor config = config{ config_janitor_period = Nothing }

disableAutoBackups :: Config -> Config
disableAutoBackups config = config{
  config_backup = (config_backup config){
    databaseBackupPolicy_allowed = mempty
  }
}

-- | Force backups to be produced as a RocksDB Checkpoint (a @db/@ tarball) when
-- 'True', or as a BackupEngine backup when 'False'. Mirrors the
-- @config_db_backup_use_checkpoint@ ServerConfig flag so it can be toggled on
-- the command line for testing without editing a config tier.
setBackupUseCheckpoint :: Bool -> Config -> Config
setBackupUseCheckpoint useCheckpoint config =
  config{ config_db_backup_use_checkpoint = useCheckpoint }

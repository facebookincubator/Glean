{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module GleanCLI.Utils
  ( disableJanitor
  , disableAutoBackups
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

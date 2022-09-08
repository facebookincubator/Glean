{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Backup (BackupCommand) where

import Control.Monad (unless)
import Data.Text (Text)
import Options.Applicative
import System.Exit hiding (die)

import Util.IO
import Util.OptParse

import Glean (Repo(..))
import qualified Glean hiding (options)
import qualified Glean.LocalOrRemote as Glean
import Glean.Database.Backup (backupDatabase)

import GleanCLI.Types

data BackupCommand
  = Backup
      { repo :: Repo
      , locator :: Locator
      }

type Locator = Text

instance Plugin BackupCommand where
  parseCommand =
    commandParser "backup" (progDesc "Backup a database") $
      Backup <$> repo <*> locator
    where
      repo = argument (maybeReader Glean.parseRepo)
        (  metavar "NAME/HASH"
        <> help "DB to backup"
        )
      locator = strArgument
        (  metavar "SITE:PREFIX"
        <> help "where to backup to"
        )

  runCommand _ _ backend Backup{..} = case Glean.backendKind backend of
    Glean.BackendEnv env -> do
      ok <- backupDatabase env repo locator
      unless ok $ exitWith (ExitFailure 1)
    _ -> die 2 "Can't backup a remote database"

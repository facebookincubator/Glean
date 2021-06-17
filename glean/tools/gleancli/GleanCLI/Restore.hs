{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Restore (RestoreCommand) where

import Control.Concurrent
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Clock.POSIX
import Options.Applicative

import Util.IO
import Util.OptParse

import Glean (Repo(..), Database(..), DatabaseStatus(..))
import qualified Glean hiding (options)

import GleanCLI.Common
import GleanCLI.Types

data WhatToRestore
  = RestoreLocator Text
  | RestoreRepo Repo
  | RestoreRepoOnDay Text Day

data RestoreCommand
  = Restore
      { what :: WhatToRestore
      }

instance Plugin RestoreCommand where
  parseCommand =
    commandParser "restore" (progDesc "Restore a database") $
      Restore <$> what
    where
      locator = strArgument
        (  metavar "LOCATOR"
        <> help "DB location, see :list-all in glean-shell"
        )
      what =
        (RestoreLocator <$> locator) <|>
        (RestoreRepo <$> repoSlash) <|> do
          repoName <- repoNameOpt
          spec <- Left <$> repoHashOpt <|> Right <$> dayOpt
          return $ case spec of
            Left hash -> RestoreRepo (Repo repoName hash)
            Right day -> RestoreRepoOnDay repoName day

      parseDay = return .
        parseTimeOrError False defaultTimeLocale (iso8601DateFormat Nothing)

      dayOpt = option (eitherReader parseDay)
        (long "date" <> metavar "YYYY-MM-DD")

  runCommand _ _ backend Restore{..} = do
    case what of
      RestoreLocator locator -> do
        Glean.restoreDatabase backend locator
        wait locator
      RestoreRepo repo -> do
        Glean.ListDatabasesResult{..} <- Glean.listDatabases backend
          Glean.ListDatabases { listDatabases_includeBackups = True }
        case [ locator
             | Glean.Database{..} <- listDatabasesResult_databases
             , database_repo == repo
             , Just locator <- [database_location] ] of
          [] -> die 1 $ "Cannot find backup locator for " <>
            Glean.showRepo repo
          (locator:_) -> restore repo locator
      RestoreRepoOnDay repoName day -> do
        Glean.ListDatabasesResult{..} <- Glean.listDatabases backend
          Glean.ListDatabases { listDatabases_includeBackups = True }
        case [ (database_repo, locator)
             | Glean.Database{..} <- listDatabasesResult_databases
             , repo_name database_repo == repoName
             , Just t <- [database_created_since_epoch]
             , day == utctDay (posixSecondsToUTCTime $ fromIntegral $
                 Glean.unPosixEpochTime t)
             , Just locator <- [database_location] ] of
          [] -> die 1 $ "Cannot find backup locator for " <>
            Text.unpack repoName <> " on " <>
            formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
          ((repo,locator):_) -> restore repo locator
    where
      restore repo locator = do
        putStrLn $ "Restoring " <> Glean.showRepo repo <>
          " from " <> Text.unpack locator
        Glean.restoreDatabase backend locator
        wait locator

      wait locator = do
        Glean.ListDatabasesResult{..} <- Glean.listDatabases backend def
        case [ db | db <- listDatabasesResult_databases
                  , database_location db == Just locator ] of
          [] -> do
            die 1 $ "error: server claims " <> Text.unpack locator <>
              " is not being restored"
          [db]
            | database_status db == Just Glean.DatabaseStatus_Restoring ->
              threadDelay 1000000 >> wait locator
            | database_status db == Just DatabaseStatus_Complete ->
              return ()
            | otherwise ->
              die 1 $ "error: unexpected database status: " <>
                show (database_status db)
          _ -> die 1 $ "error: server has multiple DBs with the locator "
            <> Text.unpack locator

{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, OverloadedRecordDot #-}
module GleanCLI.Write (WriteCommand, FinishCommand) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Control.Concurrent.Stream (stream)
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol
import Util.Control.Exception
import Util.IO
import Util.OptParse
import Util.STM

import Glean
import Glean.LocalOrRemote (loadDbSchema)
import qualified Glean.LocalOrRemote as LocalOrRemote
import Glean.Database.Schema
import Glean.Database.Write.Batch (syncWriteDatabase)
import Glean.Types as Thrift
import Glean.Util.ThriftService (queueTimeout)
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch, syncWriteJsonBatch )

import GleanCLI.Common
import GleanCLI.Create
import GleanCLI.Finish
import GleanCLI.Types

data WriteCommand
  = Write
      { maybeCreate :: Maybe CreateOpts
      , writeRepo :: Repo
      , writeOpts :: WriteOpts
      }

data WriteOpts = WriteOpts
  { finish :: Bool
  , writeMaxConcurrency :: Int
  , useLocalCache :: Bool
  , sendQueueSettings :: Glean.SendAndRebaseQueueSettings
  , writeFileFormat :: FileFormat
  , factsSource :: FactsSource
  }

parseWriteOpts :: Parser WriteOpts
parseWriteOpts = do
  finish <- finishOpt
  writeMaxConcurrency <- maxConcurrencyOpt
  useLocalCache <- useLocalSwitchOpt
  sendQueueSettings <- Glean.sendAndRebaseQueueOptions
  writeFileFormat <- fileFormatOpt JsonFormat
  factsSource <- factsSourceOpts
  return WriteOpts {
    finish,
    writeMaxConcurrency,
    useLocalCache,
    sendQueueSettings,
    writeFileFormat,
    factsSource
  }

data FactsSource = Locations [Text] | Files [FilePath]

factsSourceOpts :: Parser FactsSource
factsSourceOpts = Files <$> fileArg <|> Locations <$> locationOpt

fileArg :: Parser [FilePath]
fileArg = many $ strArgument
  (  metavar "FILE..."
  <> help ("File(s) of facts to add to the DB. "
  <> "You can specify the format of the file with --file-format. ")
  )

locationOpt :: Parser [Text]
locationOpt = many $ strOption
  (  long "location"
  <> metavar "LOCATION"
  <> help "Location(s) of facts to be downloaded and written to the db"
  )

finishOpt :: Parser Bool
finishOpt = switch
  (  long "finish"
  <> help ("Mark the DB as complete. When a DB is complete, "
  <> "it is not possible to add any more facts to it.")
  )

useLocalSwitchOpt :: Parser Bool
useLocalSwitchOpt = switch
  (  long "use-local-cache"
  <> help ("Use a local cache to avoid resending duplicate facts. May improve "
  <> "write performance.")
  )

instance Plugin WriteCommand where
  parseCommand = createCmd <|> writeCmd
    where
    createCmd =
      commandParser "create" (progDesc (
          "Create a new standalone|stacked|incremental DB. "
          <> "Please carefully read help above to understand how various "
          <> "options are related to each other.")) $ do
        createOpts <- parseCreateOpts
        writeOpts <- parseWriteOpts
        writeRepo <- dbOpts
        return Write { writeRepo, maybeCreate = Just createOpts, writeOpts }

    writeCmd =
      commandParser "write" (progDesc (
          "Write facts to an existing DB. "
          <> "Please carefully read help above to understand how various "
          <> "options are related to each other.")) $ do
        writeRepo <- dbOpts
        writeOpts <- parseWriteOpts
        return Write { writeRepo, maybeCreate = Nothing, writeOpts }

  runCommand _ _ backend Write{..} =
    tryBracket
       (forM maybeCreate $ \createOpts -> do
          putStrLn $ "Creating DB " <> showRepo writeRepo
          alreadyExists <- createDb backend writeRepo createOpts
          when alreadyExists $ die 3 "DB create failure: already exists"
       )
       (\_ result -> case resultToFailure result of
          Just err -> die 3 $ "DB create failure: " ++ err
          Nothing -> when writeOpts.finish $ finished backend writeRepo)
       (\_ ->
          write writeOpts)
    where
    write WriteOpts{..} = do
      case factsSource of
        Files writeFiles -> writeBatches writeFiles WriteOpts{..}
        Locations locations -> writeBatchDescriptors locations WriteOpts{..}
    writeBatches writeFiles WriteOpts{useLocalCache = True, ..} = do
      dbSchema <- loadDbSchema backend writeRepo
      logMessages <- newTQueueIO
      let inventory = schemaInventory dbSchema
          queueSettings = sendQueueSettings
            { sendAndRebaseQueueAllowRemoteReferences =
                case writeFileFormat of
                  -- we expect binary batches to be self-contained.
                  BinaryFormat -> False
                  JsonFormat -> True
            }
      Glean.withSendAndRebaseQueue backend writeRepo inventory queueSettings $
        \queue ->
          stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
            batch <- case writeFileFormat of
              BinaryFormat -> do
                r <- B.readFile file
                case deserializeGen (Proxy :: Proxy Compact) r of
                  Left parseError -> die 3 $ "Parse error: " <> parseError
                  Right result -> return result
              JsonFormat -> do
                (batches, schema_id) <- fileToBatches file
                buildJsonBatch dbSchema (schemaIdToOpts schema_id) batches
            _ <- Glean.writeSendAndRebaseQueue queue batch $
              \_ -> writeTQueue logMessages $ "Wrote " <> file
            atomically (flushTQueue logMessages) >>= mapM_ putStrLn
            return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles WriteOpts{useLocalCache = False, ..}
      | LocalOrRemote.BackendEnv env <- LocalOrRemote.backendKind backend = do
        logMessages <- newTQueueIO
        case writeFileFormat of
          BinaryFormat ->
            stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
              r <- B.readFile file
              case deserializeGen (Proxy :: Proxy Compact) r of
                Left parseError -> die 3 $ "Parse error: " <> parseError
                Right batch -> do
                  void $ syncWriteDatabase env writeRepo batch
                  atomically $ writeTQueue logMessages $ "Wrote " <> file
                  atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          JsonFormat ->
            stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
              (batches, schema_id) <- fileToBatches file
              syncWriteJsonBatch
                env writeRepo batches (schemaIdToOpts schema_id)
              atomically $ writeTQueue logMessages $ "Wrote " <> file
              atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles WriteOpts{useLocalCache = False, ..} = do
      logMessages <- newTQueueIO
      let settings = sendAndRebaseQueueSendQueueSettings sendQueueSettings
      Glean.withSendQueue backend writeRepo settings $ \queue ->
        stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
          case writeFileFormat of
            BinaryFormat -> do
              r <- B.readFile file
              case deserializeGen (Proxy :: Proxy Compact) r of
                Left parseError -> die 3 $ "Parse error: " <> parseError
                Right batch ->
                  atomically $ Glean.writeSendQueue queue batch $ \_ ->
                    writeTQueue logMessages $ "Wrote " <> file
            JsonFormat -> do
              (batches, _) <- fileToBatches file
              atomically $ Glean.writeSendQueueJson queue batches $ \_ ->
                writeTQueue logMessages $ "Wrote " <> file
          atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn
    writeBatchDescriptors locations WriteOpts{..} = do
      logMessages <- newTQueueIO
      let settings = sendAndRebaseQueueSendQueueSettings sendQueueSettings
      Glean.withSendQueue backend writeRepo settings $ \queue ->
        stream writeMaxConcurrency (forM_ locations) $ \location -> do
          let batchDescriptor = Thrift.BatchDescriptor {
            Thrift.batchDescriptor_location = location,
            Thrift.batchDescriptor_format = batchFormat writeFileFormat}
          atomically
            $ Glean.writeSendQueueDescriptor queue batchDescriptor $ \_ ->
              writeTQueue logMessages $ "Wrote " <> Text.unpack location
          atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    batchFormat writeFileFormat = case writeFileFormat of
      BinaryFormat -> Thrift.BatchFormat_Binary
      JsonFormat -> Thrift.BatchFormat_JSON
    resultToFailure Right{} = Nothing
    resultToFailure (Left err) = Just (show err)

  withService evb cfgAPI svc c =
    -- Ensure the queue timeout is short to avoid OOMing the server
    LocalOrRemote.withBackend evb cfgAPI svc Nothing settings $ \backend ->
      runCommand evb cfgAPI backend c
    where
      settings (clientConf, serviceOpts) =
        (clientConf, serviceOpts{queueTimeout = Just 0.1})

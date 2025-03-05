{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Write (WriteCommand, FinishCommand) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Default
import Data.Proxy
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Encode
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
import Util.Time
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch, syncWriteJsonBatch )

import GleanCLI.Common
import GleanCLI.Finish
import GleanCLI.Types
import Data.Time.Clock (UTCTime)
import Glean.Database.Meta (utcTimeToPosixEpochTime)
import Data.ByteString (ByteString)
import Glean.Util.ThriftService (queueTimeout)

data WriteCommand
  = Write
      { writeRepo :: Repo
      , writeRepoTime :: Maybe UTCTime
      , writeHandle :: Text
      , create :: Bool
      , dependencies :: Maybe (IO Thrift.Dependencies)
      , finish :: Bool
      , properties :: [(Text,Text)]
      , writeMaxConcurrency :: Int
      , useLocalCache :: Bool
      , sendQueueSettings :: Glean.SendAndRebaseQueueSettings
      , writeFileFormat :: FileFormat
      , updateSchemaForStacked :: Bool
      , factsSource :: FactsSource
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

repoTimeOpt :: Parser UTCTime
repoTimeOpt = option readTime
  (  long "repo-hash-time"
  <> metavar "yyyy-mm-ddThh:mm:ssZ"
  <> help "Timestamp of the source data to be indexed."
  )
  where
    readTime :: ReadM UTCTime
    readTime = eitherReader $ \str ->
      case readUTC $ Text.pack str of
        Just value -> Right value
        Nothing ->
          Left "expecting UTC time e.g. 2021-01-01T12:30:00Z"

dbPropertiesOpt :: Parser [(Text, Text)]
dbPropertiesOpt = many $ option readProperty
  (  long "property"
  <> metavar "NAME=VALUE"
  <> help "Set DB's properties when creating a DB."
  )
  where
    readProperty :: ReadM (Text,Text)
    readProperty = eitherReader $ \str ->
      case break (=='=') str of
        (name, '=':value) -> Right (Text.pack name, Text.pack value)
        _other -> Left "--property: expecting NAME=VALUE"

finishOpt :: Parser Bool
finishOpt = switch
  (  long "finish"
  <> help ("Mark the DB as complete. When a DB is complete, "
  <> "it is not possible to add any more facts to it.")
  )

stackedOpt :: Parser Repo
stackedOpt = option (maybeReader Glean.parseRepo)
  (  long "stacked"
  <> metavar "DB"
  <> help ("Created DB will be stacked on top of this DB. "
  <> "For more details about its schema, see --update-schema-for-stacked.")
  )

useLocalSwitchOpt :: Parser Bool
useLocalSwitchOpt = switch
  (  long "use-local-cache"
  <> help ("Use a local cache to avoid resending duplicate facts. May improve "
  <> "write performance.")
  )

incrementalOpt :: Parser Repo
incrementalOpt = option (maybeReader Glean.parseRepo)
  (  long "incremental"
  <> metavar "DB"
  <> help "Create an incremental DB on top of this DB."
  )

splitUnits :: Text -> [ByteString]
splitUnits = map Encode.encodeUtf8 . Text.splitOn ","

extractLines :: FilePath -> IO [ByteString]
extractLines file = map Encode.encodeUtf8 . Text.lines <$> Text.readFile file

includeOptString :: Parser (IO [ByteString])
includeOptString = return . splitUnits <$> strOption
  (  long "include"
  <> metavar "unit,unit,.."
  <> help "For incremental DBs only. Include these units."
  )

includeOptFile :: Parser (IO [ByteString])
includeOptFile = extractLines <$> strOption
  (  long "include-file"
  <> metavar "FILE"
  <> help ("For incremental DBs only. Include units in FILE "
  <> "(one per line).")
  )

includeOpt :: Parser (IO [ByteString])
includeOpt = includeOptFile <|> includeOptString

excludeOptString :: Parser (IO [ByteString])
excludeOptString =  return . splitUnits <$> strOption
  (  long "exclude"
  <> metavar "unit,unit,.."
  <> help "For incremental DBs only. Exclude these units."
  )

excludeOptFile :: Parser (IO [ByteString])
excludeOptFile =  extractLines <$> strOption
  (  long "exclude-file"
  <> metavar "FILE"
  <> help ("For incremental DBs only. Exclude units in FILE "
  <> "(one per line).")
  )

excludeOpt :: Parser (IO [ByteString])
excludeOpt = excludeOptFile <|> excludeOptString

updateSchemaForStackedOpt :: Parser Bool
updateSchemaForStackedOpt = switch
  (  long "update-schema-for-stacked"
  <> help (
    "When creating a stacked DB, use the current schema instead " <>
    "of the schema from the base DB.")
  )

instance Plugin WriteCommand where
  parseCommand = createCmd <|> writeCmd
    where
    createCmd =
      commandParser "create" (progDesc (
          "Create a new standalone|stacked|incremental DB. "
          <> "Please carefully read help above to understand how various "
          <> "options are related to each other.")) $ do
        writeRepo <- dbOpts
        writeRepoTime <- optional repoTimeOpt
        finish <- finishOpt
        dependencies <- optional (stackedOptions <|> updateOptions)
        properties <- dbPropertiesOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalSwitchOpt
        sendQueueSettings <- Glean.sendAndRebaseQueueOptions
        writeFileFormat <- fileFormatOpt JsonFormat
        updateSchemaForStacked <- updateSchemaForStackedOpt
        factsSource <- factsSourceOpts
        return Write
          { create=True
          , ..
          }

    writeCmd =
      commandParser "write" (progDesc (
          "Write facts to an existing DB. "
          <> "Please carefully read help above to understand how various "
          <> "options are related to each other.")) $ do
        writeRepo <- dbOpts
        finish <- finishOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalSwitchOpt
        sendQueueSettings <- Glean.sendAndRebaseQueueOptions
        writeFileFormat <- fileFormatOpt JsonFormat
        factsSource <- factsSourceOpts
        return Write
          { create=False, writeRepoTime=Nothing
          , properties=[], dependencies=Nothing
          , updateSchemaForStacked = False
          , ..
          }

    stackedOptions :: Parser (IO Dependencies)
    stackedOptions = f <$> stackedOpt
      where
        f Repo{..} =
          return $ Thrift.Dependencies_stacked $
            Thrift.Stacked repo_name repo_hash Nothing

    updateOptions :: Parser (IO Dependencies)
    updateOptions = do
      repo <- incrementalOpt
      let
        include = (,False) <$> includeOpt
        exclude = (,True) <$> excludeOpt
      ~(units, exclude) <- include <|> exclude
      return $ fmap (prune repo exclude) units
      where
        prune :: Repo -> Bool -> [ByteString] -> Dependencies
        prune repo exclude units = Thrift.Dependencies_pruned $
          Thrift.Pruned repo units exclude Nothing

  runCommand _ _ backend Write{..} =
    tryBracket
       (when create $ do
            putStrLn $ "Creating DB using handle " ++ Text.unpack writeHandle
            deps <- maybe (return Nothing) (fmap Just) dependencies
            Thrift.KickOffResponse alreadyExists <-
              Glean.kickOffDatabase backend def
                { kickOff_repo = writeRepo
                , kickOff_fill = Just $ KickOffFill_writeHandle writeHandle
                , kickOff_properties = HashMap.fromList properties
                , kickOff_dependencies = deps
                , kickOff_repo_hash_time =
                    utcTimeToPosixEpochTime <$> writeRepoTime
                , kickOff_update_schema_for_stacked = updateSchemaForStacked
                }
            when alreadyExists $ die 3 "DB create failure: already exists"
       )
       (\_ result ->
         let mFail = resultToFailure result in
         if finish then
           finished backend writeRepo writeHandle
             Nothing Nothing (fmap Text.pack mFail)
         else
           let writeFail err = die 3 $ "DB write failure: " ++ err in
           maybe (return ()) writeFail mFail)
       (\_ ->
          write Write{..})
    where
    write Write{..} = do
      case factsSource of
        Files writeFiles -> writeBatches writeFiles Write{..}
        Locations locations -> writeBatchDescriptors locations Write{..}
    writeBatches writeFiles Write{useLocalCache = True, ..} = do
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
                batches <- fileToBatches file
                buildJsonBatch dbSchema Nothing batches
            _ <- Glean.writeSendAndRebaseQueue queue batch $
              \_ -> writeTQueue logMessages $ "Wrote " <> file
            atomically (flushTQueue logMessages) >>= mapM_ putStrLn
            return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles Write{useLocalCache = False, ..}
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
              batches <- fileToBatches file
              syncWriteJsonBatch env writeRepo batches Nothing
              atomically $ writeTQueue logMessages $ "Wrote " <> file
              atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles Write{useLocalCache = False, ..} = do
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
              batches <- fileToBatches file
              atomically $ Glean.writeSendQueueJson queue batches $ \_ ->
                writeTQueue logMessages $ "Wrote " <> file
          atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn
    writeBatchDescriptors locations Write{..} = do
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

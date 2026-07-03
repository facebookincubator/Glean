{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, OverloadedRecordDot, CPP #-}
module GleanCLI.Write (WriteCommand, FinishCommand) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import qualified System.IO as IO

import Control.Concurrent.Stream (stream)
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol
import Util.Control.Exception
import Util.IO
import Util.OptParse
import Util.STM

import Glean
import qualified Glean.Remote
import Glean.LocalOrRemote (loadDbSchema)
import qualified Glean.LocalOrRemote as LocalOrRemote
import Glean.Database.Meta (getACLMode, showACLMode, ACLMode(..))
import Glean.Database.Schema
import Glean.Database.Write.Batch (syncWriteDatabase)
import Glean.Types as Thrift
import Glean.Util.ThriftService (queueTimeout)
import Glean.Write
#if GLEAN_FACEBOOK
import JustKnobs (evalKnob)
#endif
import Glean.Write.JSON ( buildJsonBatch, syncWriteJsonBatchWithACLConfig )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LB

import GleanCLI.Common
import GleanCLI.Create
import GleanCLI.Finish
import GleanCLI.Types

#if !GLEAN_FACEBOOK
-- The indexer ACL feature is Meta-internal. In the open-source build the
-- backing JustKnob is unavailable, so the knob is always reported disabled.
evalKnob :: Text -> IO (Either Text Bool)
evalKnob _ = return (Right False)
#endif

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
  , writeACLConfigPath :: Maybe FilePath
  }

parseWriteOpts :: Parser WriteOpts
parseWriteOpts = do
  finish <- finishOpt
  writeMaxConcurrency <- maxConcurrencyOpt
  useLocalCache <- useLocalSwitchOpt
  sendQueueSettings <- Glean.sendAndRebaseQueueOptions
  writeFileFormat <- fileFormatOpt JsonFormat
  factsSource <- factsSourceOpts
  writeACLConfigPath <- aclConfigOpt
  return WriteOpts {
    finish,
    writeMaxConcurrency,
    useLocalCache,
    sendQueueSettings,
    writeFileFormat,
    factsSource,
    writeACLConfigPath
  }

aclConfigOpt :: Parser (Maybe FilePath)
aclConfigOpt = optional $ strOption
  (  long "batch-acl-config"
  <> metavar "FILE"
  <> help "Path to ACL config JSON file (path → ACL ID string mapping)"
  )

data FactsSource
  = Locations [Text] Bool
  -- ^ remote locations, async
  | Files [FilePath]
  -- ^ local files

factsSourceOpts :: Parser FactsSource
factsSourceOpts = locations <|> files
  where
    locations = Locations <$> locationOpt <*> asyncOpt
    files = Files <$> fileArg

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

asyncOpt :: Parser Bool
asyncOpt = switch
  (  long "async"
  <> help ("Don't wait till write is complete. "
  <> "Can be used only with --location. "
  <> "All writes are guaranteed to finish before db completes")
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
       (\_ -> do
          -- Log ACL mode for this write operation
          -- (unless we just created the DB)
          case maybeCreate of
            Just _ -> return ()  -- ACL mode was already logged in create
            Nothing -> do
              db <- Thrift.getDatabaseResult_database
                <$> Glean.getDatabase backend writeRepo
              let aclMode = getACLMode (Thrift.database_properties db)
              putStrLn $ "[glean write] " ++ showACLMode aclMode
          write writeOpts)
    where
    -- Retry transient channel exceptions so a write-server restart doesn't fail
    -- the async descriptor sends (the queued paths already retry via the send
    -- queue).
    retryBackend =
      Glean.Remote.backendRetryWrites backend Glean.Remote.defaultRetryPolicy
    write WriteOpts{..} = do
      validateACLOptions backend writeRepo maybeCreate
        writeFileFormat writeACLConfigPath
      case factsSource of
        Files writeFiles -> writeBatches writeFiles WriteOpts{..}
        Locations locations async
          -> writeBatchDescriptors locations async WriteOpts{..}
    writeBatches writeFiles
      WriteOpts{useLocalCache = True, ..} = do
      dbSchema <- loadDbSchema backend writeRepo
      logMessages <- newTQueueIO
      aclConfig <- loadACLConfig writeACLConfigPath
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
            _ <- Glean.writeSendAndRebaseQueue queue
              (batch { Thrift.batch_acl_config =
                aclConfig <|> Thrift.batch_acl_config batch }) $
              \_ -> writeTQueue logMessages $ "Wrote " <> file
            atomically (flushTQueue logMessages) >>= mapM_ putStrLn
            return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles
      WriteOpts{useLocalCache = False, ..}
      | LocalOrRemote.BackendEnv env <- LocalOrRemote.backendKind backend = do
        logMessages <- newTQueueIO
        aclConfig <- loadACLConfig writeACLConfigPath
        case writeFileFormat of
          BinaryFormat ->
            stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
              r <- B.readFile file
              case deserializeGen (Proxy :: Proxy Compact) r of
                Left parseError -> die 3 $ "Parse error: " <> parseError
                Right batch -> do
                  void $ syncWriteDatabase env writeRepo
                    (batch { Thrift.batch_acl_config =
                      aclConfig <|> Thrift.batch_acl_config batch })
                  atomically $ writeTQueue logMessages $ "Wrote " <> file
                  atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          JsonFormat ->
              stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
                (batches, schema_id) <- fileToBatches file
                syncWriteJsonBatchWithACLConfig
                    env writeRepo batches (schemaIdToOpts schema_id) aclConfig
                atomically $ writeTQueue logMessages $ "Wrote " <> file
                atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatches writeFiles
      WriteOpts{useLocalCache = False, ..} = do
      logMessages <- newTQueueIO
      aclConfig <- loadACLConfig writeACLConfigPath
      let settings =
            sendAndRebaseQueueSendQueueSettings sendQueueSettings
      Glean.withSendQueue backend writeRepo settings $ \queue ->
        stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
          case writeFileFormat of
            BinaryFormat -> do
              r <- B.readFile file
              case deserializeGen (Proxy :: Proxy Compact) r of
                Left parseError -> die 3 $ "Parse error: " <> parseError
                Right batch -> do
                  atomically $ Glean.writeSendQueue queue
                    (batch { Thrift.batch_acl_config =
                      aclConfig <|> Thrift.batch_acl_config batch }) $
                    \_ -> writeTQueue logMessages $ "Wrote " <> file
            JsonFormat -> do
              (batches, _) <- fileToBatches file
              atomically $ Glean.writeSendQueueJson queue batches $ \_ ->
                writeTQueue logMessages $ "Wrote " <> file
          atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    writeBatchDescriptors locations False WriteOpts{..} = do
      logMessages <- newTQueueIO
      let settings = sendAndRebaseQueueSendQueueSettings sendQueueSettings
      Glean.withSendQueue backend writeRepo settings $ \queue ->
        stream writeMaxConcurrency (forM_ locations) $ \location -> do
          let descriptor = batchDescriptor location writeFileFormat
          atomically
            $ Glean.writeSendQueueDescriptor queue descriptor $ \_ ->
              writeTQueue logMessages $ "Wrote " <> Text.unpack location
          atomically (flushTQueue logMessages) >>= mapM_ putStrLn
          return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    -- Send directly without the client queue as we're writing asynchronously.
    writeBatchDescriptors locations True WriteOpts{..} = do
      stream writeMaxConcurrency (forM_ locations) $ \location -> do
        let descriptor = batchDescriptor location writeFileFormat
        Glean.sendBatchDescriptor retryBackend writeRepo descriptor False
        return ()
    batchFormat writeFileFormat = case writeFileFormat of
      BinaryFormat -> Thrift.BatchFormat_Binary
      JsonFormat -> Thrift.BatchFormat_JSON
    batchDescriptor location fileFormat = Thrift.BatchDescriptor
      { Thrift.batchDescriptor_location = location
      , Thrift.batchDescriptor_format = batchFormat fileFormat
      }
    resultToFailure Right{} = Nothing
    resultToFailure (Left err) = Just (show err)

  withService evb cfgAPI svc c =
    -- Ensure the queue timeout is short to avoid OOMing the server
    LocalOrRemote.withBackend evb cfgAPI svc Nothing settings $ \backend ->
      runCommand evb cfgAPI backend c
    where
      settings (clientConf, serviceOpts) =
        (clientConf, serviceOpts{queueTimeout = Just 0.1})

-- | If the DB has an ACL mode configured (and the indexer_acls knob is on),
-- a standalone JSON write must supply --batch-acl-config. Only enforced on
-- standalone writes (not the create+write flow) and only for JSON format
-- (binary batches may already carry embedded ACL config from the indexer).
validateACLOptions
  :: LocalOrRemote.LocalOrRemote b
  => b
  -> Repo
  -> Maybe CreateOpts
  -> FileFormat
  -> Maybe FilePath
  -> IO ()
validateACLOptions backend repo maybeCreate fileFormat aclConfigPath =
  when (isNothing maybeCreate && fileFormat == JsonFormat) $ do
    db <- Thrift.getDatabaseResult_database <$> Glean.getDatabase backend repo
    let dbAclMode = getACLMode (Thrift.database_properties db)
    indexerAclsEnabled <-
      (== Right True) <$> evalKnob "code_indexing/glean:indexer_acls"
    when (dbAclMode /= ACLDisabled && indexerAclsEnabled
          && isNothing aclConfigPath) $
      die 3 $
        "Database has ACL mode '" ++ showACLMode dbAclMode
        ++ "' but --batch-acl-config was not provided."
        ++ " Supply an ACL config file or use a"
        ++ " database without ACL mode."

-- | Load ACL config from a JSON file, gated by the indexer_acls knob.
-- Returns Nothing if no path provided or if the knob is disabled.
-- Dies on parse failure.
loadACLConfig :: Maybe FilePath -> IO (Maybe (HashMap.HashMap Text [Text]))
loadACLConfig Nothing = return Nothing
loadACLConfig (Just configPath) = do
  knobResult <- evalKnob "code_indexing/glean:indexer_acls"
  let indexerAclsEnabled = knobResult == Right True
  if not indexerAclsEnabled
    then do
      IO.hPutStrLn IO.stderr $
        "Warning: --batch-acl-config was provided but JustKnob "
        ++ "code_indexing/glean:indexer_acls is disabled;"
        ++ " the ACL config file will be ignored."
      return Nothing
    else do
      configBytes <- LB.readFile configPath
      case Aeson.decode configBytes of
        Just (config :: Aeson.Object) -> do
          let textMap = HashMap.fromList
                [ (Key.toText k, groupIds)
                | (k, v) <- KeyMap.toList config
                , let groupIds = case v of
                        Aeson.Array arr ->
                          [ t | Aeson.String t <- foldr (:) [] arr ]
                        _ -> []
                ]
          return $ Just textMap
        Nothing -> die 3 $
          "Failed to parse ACL config from " ++ configPath

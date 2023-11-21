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
import Glean.Datasource.Scribe.Write
import Glean.Database.Write.Batch (syncWriteDatabase)
import Glean.Types as Thrift
import Glean.Util.Time
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch, syncWriteJsonBatch )

import GleanCLI.Common
import GleanCLI.Finish
import GleanCLI.Types
import Data.Time.Clock (UTCTime)
import Glean.Database.Meta (utcTimeToPosixEpochTime)
import Data.Int (Int32)
import Data.ByteString (ByteString)
import Glean.Util.ThriftService (queueTimeout)

data ScribeOptions = ScribeOptions
  { writeFromScribe :: WriteFromScribe
  , scribeCompress :: Bool
  }

data FileFormat
  = JsonFormat
  | BinaryFormat

instance Show FileFormat where
  show ff = case ff of
    JsonFormat -> "json"
    BinaryFormat -> "binary"

data WriteCommand
  = Write
      { writeRepo :: Repo
      , writeRepoTime :: Maybe UTCTime
      , writeHandle :: Text
      , writeFiles :: [FilePath]
      , create :: Bool
      , dependencies :: Maybe (IO Thrift.Dependencies)
      , scribe :: Maybe ScribeOptions
      , finish :: Bool
      , properties :: [(Text,Text)]
      , writeMaxConcurrency :: Int
      , useLocalCache :: Bool
      , sendQueueSettings :: Glean.SendAndRebaseQueueSettings
      , writeFileFormat :: FileFormat
      , updateSchemaForStacked :: Bool
      }

fileArg :: Parser [FilePath]
fileArg = many $ strArgument
  (  metavar "FILE..."
  <> help ("File(s) of facts to add to the DB. "
  <> "You can specify the format of the file with --file-format")
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

fileFormatOpt :: Parser FileFormat
fileFormatOpt = option (eitherReader parseFileFormat)
  (  long "file-format"
  <> value JsonFormat
  <> showDefault
  <> metavar "(json|binary)"
  <> help "Format of the files with facts (see FILE for more details)"
  )
  where
    parseFileFormat :: String -> Either String FileFormat
    parseFileFormat "json" = Right JsonFormat
    parseFileFormat "binary" = Right BinaryFormat
    parseFileFormat s = Left $ "unknown format: " <> s
      <> ", supported values: (json|binary)"

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

scribeCategoryOpt :: Parser Text
scribeCategoryOpt = textOption
  (  long "scribe-category"
  <> metavar "NAME"
  <> help "SCRIBE: Listen/write to this scribe category for facts."
  )

scribeCompressOpt :: Parser Bool
scribeCompressOpt = switch
  (  long "compress"
  <> help "SCRIBE: The scribe category carries compressed data."
  )

scribeBucketOpt :: Parser Int32
scribeBucketOpt = option auto
  (  long "scribe-bucket"
  <> metavar "BUCKET"
  <> help ("SCRIBE: If your scribe category has buckets, you have to "
  <> "specify the scribe bucket as well.")
  )

writeScribeOptions :: Parser (Text, Maybe PickScribeBucket, Bool)
writeScribeOptions = do
  cat <- scribeCategoryOpt
  bucket <- optional (PickScribeBucket_bucket <$> scribeBucketOpt)
  compress <- scribeCompressOpt
  return (cat, bucket, compress)

scribeOptions :: Parser ScribeOptions
scribeOptions = do
  ~(cat, bucket, compress) <- writeScribeOptions
  let
    startTime = Just . ScribeStart_start_time <$> scribeStartTimeOpt
    checkpoint = Just . ScribeStart_checkpoint <$> scribeCheckPointOpt

  start <- startTime <|> checkpoint <|> pure Nothing
  opts <- SendJsonBatchOptions <$> noBase64BinaryOpt
  return ScribeOptions
    { writeFromScribe = WriteFromScribe "" cat start (Just opts) bucket
    , scribeCompress = compress
    }

-- TODO: respect this flag when for any JSON files
-- It just happend, that this flag is respected only with 'glean create scribe'.
noBase64BinaryOpt :: Parser Bool
noBase64BinaryOpt = switch
  (  long "no-base64-binary"
  <> help ("Set this if the Thrift 'binary' type is not base64-encoded "
  <> "in JSON. You may need this if the JSON was created by the Python "
  <> "Thrift encoder.")
  )

scribeCheckPointOpt :: Parser Text
scribeCheckPointOpt = textOption
  (  long "checkpoint"
  <> metavar "STRING"
  <> help "SCRIBE: Start reading scribe data from the given checkpoint."
  )

scribeStartTimeOpt :: Parser Text
scribeStartTimeOpt = textOption
  (  long "start-time"
  <> metavar "TIME"
  <> help ("SCRIBE: Start reading data from the given timestamp. "
  <> "Accepts any format that `date -d` can understand.")
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
        writeFiles <- fileArg
        finish <- finishOpt
        scribe <- optional scribeOptions
        dependencies <- optional (stackedOptions <|> updateOptions)
        properties <- dbPropertiesOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalSwitchOpt
        sendQueueSettings <- Glean.sendAndRebaseQueueOptions
        writeFileFormat <- fileFormatOpt
        updateSchemaForStacked <- updateSchemaForStackedOpt
        return Write
          { create=True
          , ..
          }

    writeCmd =
      commandParser "write" (progDesc (
          "Write facts to an existing DB. "
          <> "Please carefully read help above to understand how various "
          <> "options are related to each other.")) $ do
        ~(writeRepo, scribe) <-
           (,Nothing) <$> dbOpts <|>
           (do
              ~(cat, bucket, compress) <- writeScribeOptions
              return (def, Just ScribeOptions
                { writeFromScribe = def
                    { writeFromScribe_category = cat
                    , writeFromScribe_bucket = bucket }
                , scribeCompress = compress }))
        writeFiles <- fileArg
        finish <- finishOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalSwitchOpt
        sendQueueSettings <- Glean.sendAndRebaseQueueOptions
        writeFileFormat <- fileFormatOpt
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
                , kickOff_fill = Just $ case scribe of
                    Nothing -> KickOffFill_writeHandle writeHandle
                    Just scribe -> KickOffFill_scribe
                      (writeFromScribe scribe)
                        { writeFromScribe_writeHandle = writeHandle }
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
    write Write{useLocalCache = True, scribe = Nothing, ..} = do
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

    write Write{useLocalCache = False, scribe = Nothing, ..}
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

    write Write{useLocalCache = False, scribe = Nothing, ..} = do
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

    write Write{useLocalCache = False, writeFileFormat = JsonFormat,
        scribe = Just scribeOptions, ..} = do
      stream writeMaxConcurrency (forM_ writeFiles) $ \file -> do
        batches <- fileToBatches file
        case scribeOptions of
          ScribeOptions
            { writeFromScribe = WriteFromScribe{..}, .. } ->
              scribeWriteBatches
                writeFromScribe_category
                (case writeFromScribe_bucket of
                  Just (PickScribeBucket_bucket n) ->
                      Just (fromIntegral n :: Int)
                  Nothing -> Nothing)
                batches
                scribeCompress

    write Write{useLocalCache = True, scribe = Just _, ..} =
      die 3 "Cannot use a local cache with scribe"
    write Write{scribe = Just _, writeFileFormat = BinaryFormat, ..} =
      die 3 "Cannot use binary format with scribe"

    resultToFailure Right{} = Nothing
    resultToFailure (Left err) = Just (show err)

  withService evb cfgAPI svc c =
    -- Ensure the queue timeout is short to avoid OOMing the server
    LocalOrRemote.withBackend evb cfgAPI svc Nothing settings $ \backend ->
      runCommand evb cfgAPI backend c
    where
      settings (clientConf, serviceOpts) =
        (clientConf, serviceOpts{queueTimeout = Just 0.1})

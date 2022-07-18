{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Write (WriteCommand, FinishCommand) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Proxy
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Control.Concurrent.Stream (stream)
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol
import Util.Control.Exception
import Util.IO
import Util.OptParse

import Glean hiding (options)
import Glean.Backend (loadDbSchema)
import qualified Glean.LocalOrRemote as LocalOrRemote
import Glean.Database.Schema
import Glean.Datasource.Scribe.Write
import Glean.Types as Thrift
import Glean.Util.Time
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch )

import GleanCLI.Common
import GleanCLI.Finish
import GleanCLI.Types
import Data.Time.Clock (UTCTime)
import Glean.Database.Meta (utcTimeToPosixEpochTime)
import Data.Int (Int32)
import Data.ByteString (ByteString)

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
      , dependencies :: Maybe Thrift.Dependencies
      , scribe :: Maybe ScribeOptions
      , finish :: Bool
      , properties :: [(Text,Text)]
      , writeMaxConcurrency :: Int
      , useLocalCache :: Maybe Glean.SendAndRebaseQueueSettings
      , writeFileFormat :: FileFormat
      , updateSchemaForStacked :: Bool
      }

fileArg :: Parser [FilePath]
fileArg = many $ strArgument
  (  metavar "FILE..."
  <> help ("Files of facts. "
  <> "You can specify the format of the file through --file-format option")
  )

repoTimeOpt :: Parser UTCTime
repoTimeOpt = option readTime
  (  long "repo-hash-time"
  <> metavar "yyyy-mm-ddThh:mm:ssZ"
  <> help "Set properties when creating a DB"
  )
  where
    readTime :: ReadM UTCTime
    readTime = eitherReader $ \str ->
      case readUTC $ Text.pack str of
        Just value -> Right value
        Nothing ->
          Left "expecting time e.g. 2021-01-01T12:30:00Z"

dbPropertiesOpt :: Parser [(Text, Text)]
dbPropertiesOpt = many $ option readProperty
  (  long "property"
  <> metavar "NAME=VALUE"
  <> help "Set properties when creating a DB"
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
  <> help "Format of the files with facts"
  )
  where
    parseFileFormat :: String -> Either String FileFormat
    parseFileFormat "json" = Right JsonFormat
    parseFileFormat "binary" = Right BinaryFormat
    parseFileFormat s = Left $ "unknown format: " <> s

finishOpt :: Parser Bool
finishOpt = switch
  (  long "finish"
  <> help "also mark the DB as complete")

stackedOpt :: Parser Repo
stackedOpt = option (maybeReader Glean.parseRepo)
  (  long "stacked"
  <> metavar "REPO"
  <> help "Create a stacked database"
  )

scribeCategoryOpt :: Parser Text
scribeCategoryOpt = textOption
  (  long "scribe-category"
  <> metavar "NAME"
  )

scribeCompressOpt :: Parser Bool
scribeCompressOpt = switch
  (long "compress")

scribeBucketOpt :: Parser Int32
scribeBucketOpt = option auto
  (  long "scribe-bucket"
  <> metavar "BUCKET"
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
  opts <- SendJsonBatchOptions <$> scribeNoBase64BinaryOpt
  return ScribeOptions
    { writeFromScribe = WriteFromScribe "" cat start (Just opts) bucket
    , scribeCompress = compress
    }

scribeNoBase64BinaryOpt :: Parser Bool
scribeNoBase64BinaryOpt = switch
  (  long "no-base64-binary"
  )

scribeCheckPointOpt :: Parser Text
scribeCheckPointOpt = textOption
  (  long "checkpoint"
  <> metavar "STRING"
  )

scribeStartTimeOpt :: Parser Text
scribeStartTimeOpt = textOption
  (  long "start-time"
  <> metavar "TIME"
  )

useLocalCacheOptions :: Parser (Maybe Glean.SendAndRebaseQueueSettings)
useLocalCacheOptions = do
    useLocalCacheFlag <- useLocalSwitchOpt
    sendAndRebaseQueue <- Glean.sendAndRebaseQueueOptions
    return $ if useLocalCacheFlag then
        Just sendAndRebaseQueue
    else
      Nothing

useLocalSwitchOpt :: Parser Bool
useLocalSwitchOpt = switch
  (  long "use-local-cache"
  <> help "use a cache to rebase facts locally"
  )

incrementalOpt :: Parser Repo
incrementalOpt = option (maybeReader Glean.parseRepo)
  (  long "incremental"
  <> metavar "REPO"
  <> help "Create an incremental database"
  )

splitUnits :: [Char] -> [ByteString]
splitUnits = map B8.pack . splitOn ","

includeOpt :: Parser [ByteString]
includeOpt = splitUnits <$> option auto
  (  long "include"
  <> metavar "unit,unit,.."
  <> help "Include these units"
  )

excludeOpt :: Parser [ByteString]
excludeOpt =  splitUnits <$> option auto
  (  long "exclude"
  <> metavar "unit,unit,.."
  <> help "Exclude these units"
  )

updateSchemaForStackedOpt :: Parser Bool
updateSchemaForStackedOpt = switch
  (  long "update-schema-for-stacked"
  <> help (
    "When creating a stacked DB, use the current schema instead " <>
    "of the schema from the base DB")
  )

instance Plugin WriteCommand where
  parseCommand = createCmd <|> writeCmd
    where
    createCmd =
      commandParser "create" (progDesc "Create a new database") $ do
        writeRepo <- repoOpts
        writeRepoTime <- optional repoTimeOpt
        writeFiles <- fileArg
        finish <- finishOpt
        scribe <- optional scribeOptions
        dependencies <- optional (stackedOptions <|> updateOptions)
        properties <- dbPropertiesOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalCacheOptions
        writeFileFormat <- fileFormatOpt
        updateSchemaForStacked <- updateSchemaForStackedOpt
        return Write
          { create=True
          , ..
          }

    writeCmd =
      commandParser "write" (progDesc "Write facts to a database") $ do
        ~(writeRepo, scribe) <-
           (,Nothing) <$> repoOpts <|>
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
        useLocalCache <- useLocalCacheOptions
        writeFileFormat <- fileFormatOpt
        return Write
          { create=False, writeRepoTime=Nothing
          , properties=[], dependencies=Nothing
          , updateSchemaForStacked = False
          , ..
          }

    stackedOptions = Thrift.Dependencies_stacked <$> stackedOpt

    updateOptions = do
      repo <- incrementalOpt
      let
        include = (,False) <$> includeOpt
        exclude = (,True) <$> excludeOpt
      ~(units, exclude) <- include <|> exclude
      return $ Thrift.Dependencies_pruned $
        Thrift.Pruned repo units exclude

  runCommand _ _ backend Write{..} =
    tryBracket
       (when create $ do
            putStrLn $ "Creating DB using handle " ++ Text.unpack writeHandle
            Thrift.KickOffResponse alreadyExists <-
              Glean.kickOffDatabase backend def
                { kickOff_repo = writeRepo
                , kickOff_fill = Just $ case scribe of
                    Nothing -> KickOffFill_writeHandle writeHandle
                    Just scribe -> KickOffFill_scribe
                      (writeFromScribe scribe)
                        { writeFromScribe_writeHandle = writeHandle }
                , kickOff_properties = HashMap.fromList properties
                , kickOff_dependencies = dependencies
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
          write
            writeRepo
            writeFiles
            writeMaxConcurrency
            scribe
            useLocalCache
            writeFileFormat)
    where
    write repo files max Nothing (Just useLocalCache) fileFormat = do
      dbSchema <- loadDbSchema backend repo
      logMessages <- newTQueueIO
      let inventory = schemaInventory dbSchema
      Glean.withSendAndRebaseQueue backend repo inventory useLocalCache $
        \queue ->
          stream max (forM_ files) $ \file -> do
            batch <- case fileFormat of
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

    write repo files max Nothing Nothing BinaryFormat =
      stream max (forM_ files) $ \file -> do
        handleAll (\e -> do throwIO $ ErrorCall $ file <> ": " <> show e) $ do
          r <- B.readFile file
          batch <- case deserializeGen (Proxy :: Proxy Compact) r of
            Left parseError -> die 3 $ "Parse error: " <> parseError
            Right result -> return result
          void $ Glean.sendBatch backend repo batch

    write repo files max scribe Nothing JsonFormat = do
      stream max (forM_ files) $ \file -> do
        batches <- fileToBatches file
        case scribe of
          Nothing ->
            void $ LocalOrRemote.sendJsonBatch backend repo batches Nothing
          Just ScribeOptions
            { writeFromScribe = WriteFromScribe{..}, .. } ->
              scribeWriteBatches
                writeFromScribe_category
                (case writeFromScribe_bucket of
                  Just (PickScribeBucket_bucket n) ->
                      Just (fromIntegral n :: Int)
                  Nothing -> Nothing)
                batches
                scribeCompress

    write _repo _files _max (Just _scribe) (Just _useLocalCache) _  =
      die 3 "Cannot use a local cache with scribe"
    write _repo _files _max (Just _scribe) Nothing BinaryFormat  =
      die 3 "Cannot use binary format with scribe"

    resultToFailure Right{} = Nothing
    resultToFailure (Left err) = Just (show err)

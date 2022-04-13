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
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Proxy
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Control.Concurrent.Stream (stream)
import Foreign.CPP.Dynamic (parseJSONWithOptions, JSONOptions(..))
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol
import Util.Control.Exception
import Util.IO
import Util.OptParse

import Glean hiding (options)
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

data ScribeOptions = ScribeOptions
  { writeFromScribe :: WriteFromScribe
  , scribeCompress :: Bool
  }

data FileFormat
  = JsonFormat
  | BinaryFormat

parseFileFormat :: String -> Either String FileFormat
parseFileFormat "json" = Right JsonFormat
parseFileFormat "binary" = Right BinaryFormat
parseFileFormat s = Left $ "unknown format: " <> s

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
      }

instance Plugin WriteCommand where
  parseCommand = createCmd <|> writeCmd
    where
    createCmd =
      commandParser "create" (progDesc "Create a new database") $ do
        writeRepo <- repoOpts
        writeRepoTime <- optional $ option readTime
          (  long "repo-hash-time"
          <> metavar "yyyy-mm-ddThh:mm:ssZ"
          <> help "Set properties when creating a DB"
          )
        writeFiles <- fileArgs
        finish <- finishOpt
        scribe <- optional scribeOptions
        dependencies <- optional (stackedOptions <|> updateOptions)
        properties <- many $ option readProperty
          (  long "property"
          <> metavar "NAME=VALUE"
          <> help "Set properties when creating a DB"
          )
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalCacheOptions
        return Write
          { create=True
          , writeFileFormat=JsonFormat
          , ..
          }

    readProperty :: ReadM (Text,Text)
    readProperty = eitherReader $ \str ->
      case break (=='=') str of
        (name, '=':value) -> Right (Text.pack name, Text.pack value)
        _other -> Left "--property: expecting NAME=VALUE"

    readTime :: ReadM UTCTime
    readTime = eitherReader $ \str ->
      case readUTC $ Text.pack str of
        Just value -> Right value
        Nothing ->
          Left "expecting time e.g. 2021-01-01T12:30:00Z"

    writeCmd =
      commandParser "write" (progDesc "Write facts to a database") $ do
        ~(writeRepo, scribe) <-
           (,Nothing) <$> repoOpts <|>
           (do
              ~(cat, bucket, compress) <- writeScribeOpts
              return (def, Just ScribeOptions
                { writeFromScribe = def
                    { writeFromScribe_category = cat
                    , writeFromScribe_bucket = bucket }
                , scribeCompress = compress }))
        writeFiles <- fileArgs
        finish <- finishOpt
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalCacheOptions
        writeInventory :: Maybe String <- optional $ strOption
          (  long "inventory"
          <> metavar "PATH"
          <> help "Deprecated - use file-format instead"
          )
        writeFileFormat <-
              optional $ option (eitherReader parseFileFormat)
                ( long "file-format"
                <> metavar "(json|binary)"
                <> help "Format of the input files"
                )
        return Write
          { create=False, writeRepoTime=Nothing
          , properties=[], dependencies=Nothing
          , writeFileFormat = fromMaybe
            (if isJust writeInventory then BinaryFormat else JsonFormat)
            writeFileFormat
          , ..
          }

    finishOpt = switch
      (  long "finish"
      <> help "also mark the DB as complete")

    writeScribeOpts :: Parser (Text, Maybe PickScribeBucket, Bool)
    writeScribeOpts = do
      cat <- textOption (long "scribe-category" <> metavar "NAME")
      bucket <- optional (PickScribeBucket_bucket <$>
        option auto (long "scribe-bucket" <> metavar "BUCKET"))
      compress <- switch (long "compress")
      return (cat, bucket, compress)

    scribeOptions :: Parser ScribeOptions
    scribeOptions = do
      ~(cat, bucket, compress) <- writeScribeOpts
      let
        startTime = Just . ScribeStart_start_time <$>
          textOption (long "start-time" <> metavar "TIME")
        checkpoint = Just . ScribeStart_checkpoint <$>
          textOption (long "checkpoint" <> metavar "STRING")
      start <- startTime <|> checkpoint <|> pure Nothing
      opts <- SendJsonBatchOptions <$> switch (long "no-base64-binary")
      return ScribeOptions
        { writeFromScribe = WriteFromScribe "" cat start (Just opts) bucket
        , scribeCompress = compress
        }

    useLocalCacheOptions
      :: Parser (Maybe Glean.SendAndRebaseQueueSettings)
    useLocalCacheOptions = do
        useLocalCacheFlag <- switch
          (  long "use-local-cache"
          <> help "use a cache to rebase facts locally"
          )
        sendAndRebaseQueue <- Glean.sendAndRebaseQueueOptions
        return $ if useLocalCacheFlag then
            Just sendAndRebaseQueue
        else
          Nothing

    stackedOptions = Thrift.Dependencies_stacked
      <$> option (maybeReader Glean.parseRepo)
      (  long "stacked"
      <> metavar "REPO"
      <> help "Create a stacked database"
      )

    updateOptions = do
      repo <- option (maybeReader Glean.parseRepo)
        (  long "incremental"
        <> metavar "REPO"
        <> help "Create an incremental database"
        )
      let
        splitUnits = map B8.pack . splitOn ","
        include = (,False) . splitUnits <$> strOption
          (  long "include"
          <> metavar "unit,unit,.."
          <> help "Include these units"
          )
        exclude = (,True) . splitUnits <$> strOption
          (  long "exclude"
          <> metavar "unit,unit,.."
          <> help "Exclude these units"
          )
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
      schemaInfo <- Glean.getSchemaInfo backend repo
      dbSchema <- fromSchemaInfo schemaInfo readWriteContent
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
                batches <- toBatches file
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
        batches <- toBatches file
        case scribe of
          Nothing -> void $ Glean.sendJsonBatch backend repo batches Nothing
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

    toBatches :: FilePath -> IO [JsonFactBatch]
    toBatches file = do
      bs <- B.readFile file
      r <- Foreign.CPP.Dynamic.parseJSONWithOptions opts bs
      val <- case r of
        Right val -> return val
        Left err -> throwIO  $ ErrorCall $ file ++ ": " ++ Text.unpack err
      case Aeson.parse parseJsonFactBatches val of
        Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
        Aeson.Success x -> return x
      where
        -- folly's default recursion limit is 100, which is not enough for us.
        opts = Foreign.CPP.Dynamic.JSONOptions
          { json_recursionLimit = Just 500 }

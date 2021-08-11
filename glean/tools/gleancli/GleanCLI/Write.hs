-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Write (WriteCommand, FinishCommand) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Proxy
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Control.Concurrent.Stream (streamWithThrow)
import Foreign.CPP.Dynamic (parseJSON)
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol
import Util.Control.Exception
import Util.IO
import Util.OptParse

import Glean hiding (options)
import Glean.Database.Schema
import Glean.Datasource.Scribe.Write
import qualified Glean.LocalOrRemote as Glean
import Glean.Types as Thrift
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch )

import GleanCLI.Common
import GleanCLI.Finish
import GleanCLI.Types

data ScribeOptions = ScribeOptions
  { writeFromScribe :: WriteFromScribe
  , scribeCompress :: Bool
  }

data WriteCommand
  = Write
      { writeRepo :: Repo
      , writeHandle :: Text
      , writeFiles :: [FilePath]
      , create :: Bool
      , dependencies :: Maybe Thrift.Dependencies
      , scribe :: Maybe ScribeOptions
      , finish :: Bool
      , properties :: [(Text,Text)]
      , writeMaxConcurrency :: Int
      , useLocalCache :: Maybe Glean.SendAndRebaseQueueSettings
      , writeInventory :: Maybe FilePath
      }

instance Plugin WriteCommand where
  parseCommand = createCmd <|> writeCmd
    where
    createCmd =
      commandParser "create" (progDesc "Create a new database") $ do
        writeRepo <- repoOpts
        writeFiles <- fileArgs
        finish <- finishOpt
        scribe <- Just <$> scribeOptions <|> pure Nothing
        dependencies <- optional (stackedOptions <|> updateOptions)
        properties <- many $ option readProperty
          (  long "property"
          <> metavar "NAME=VALUE"
          <> help "Set properties when creating a DB"
          )
        writeHandle <- handleOpt
        writeMaxConcurrency <- maxConcurrencyOpt
        useLocalCache <- useLocalCacheOptions
        return Write{create=True, writeInventory=Nothing, ..}

    readProperty :: ReadM (Text,Text)
    readProperty = eitherReader $ \str ->
      case break (=='=') str of
        (name, '=':value) -> Right (Text.pack name, Text.pack value)
        _other -> Left "--property: expecting NAME=VALUE"

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
        writeInventory <- optional $ strOption
          (  long "inventory"
          <> metavar "PATH"
          <> help "Write binary files using this schema inventory"
          )
        return Write{create=False, properties=[], dependencies=Nothing, ..}

    finishOpt = switch
      (  long "finish"
      <> help "also mark the DB as complete")

    writeScribeOpts :: Parser (Text, Maybe PickScribeBucket, Bool)
    writeScribeOpts = do
      cat <- textOption (long "scribe-category" <> metavar "NAME")
      bucket <- optional $ fmap PickScribeBucket_bucket $
        option auto (long "scribe-bucket" <> metavar "BUCKET")
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
            writeInventory)
    where
    write repo files max Nothing (Just useLocalCache) maybeUserInventory = do
      schemaInfo <- Glean.getSchemaInfo backend repo
      dbSchema <- fromSchemaInfo schemaInfo readWriteContent
      logMessages <- newTQueueIO
      let inventory = schemaInventory dbSchema
      whenJust maybeUserInventory $ \ userInventoryFilePath -> do
        userInventory <- B.readFile userInventoryFilePath
        actualInventory <- Glean.serializeInventory backend repo
        when (userInventory /= actualInventory) $
          die 3 "Schema inventory did not match"
      Glean.withSendAndRebaseQueue backend repo inventory useLocalCache $
        \queue ->
          streamWithThrow max (forM_ files) $ \file -> do
            batch <- case maybeUserInventory of
              Just _ -> do -- write from binary
                r <- B.readFile file
                case deserializeGen (Proxy :: Proxy Compact) r of
                  Left parseError -> die 3 $ "Parse error: " <> parseError
                  Right result -> return result
              Nothing -> do -- write from json
                r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
                val <- either (throwIO  . ErrorCall . ((file ++ ": ") ++) .
                  Text.unpack) return r
                batches <- case Aeson.parse parseJsonFactBatches val of
                  Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
                  Aeson.Success x -> return x
                buildJsonBatch dbSchema Nothing batches
            _ <- Glean.writeSendAndRebaseQueue queue batch $
              \_ -> writeTQueue logMessages $ "Wrote " <> file
            atomically (flushTQueue logMessages) >>= mapM_ putStrLn
            return ()
      atomically (flushTQueue logMessages) >>= mapM_ putStrLn

    write repo files max scribe Nothing Nothing = do
      streamWithThrow max (forM_ files) $
        \file -> do
          r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
          val <- either (throwIO  . ErrorCall . ((file ++ ": ") ++) .
            Text.unpack) return r
          batches <- case Aeson.parse parseJsonFactBatches val of
            Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
            Aeson.Success x -> return x
          case scribe of
            Nothing -> void $ Glean.sendJsonBatch backend repo batches Nothing
            Just ScribeOptions
              { writeFromScribe = WriteFromScribe{..}, .. } ->
                scribeWriteBatches
                  writeFromScribe_category
                  (case writeFromScribe_bucket of
                    Just (PickScribeBucket_bucket n) -> Just (fromIntegral n)
                    Nothing -> Nothing)
                  batches
                  scribeCompress

    write _repo _files _max (Just _scribe) (Just _useLocalCache) _  =
      die 3 "Cannot use a local cache with scribe"
    write _repo _files _max _ Nothing (Just _inventory) =
      die 3 "Cannot write binary without using a local cache"

    resultToFailure Right{} = Nothing
    resultToFailure (Left err) = Just (show err)

{-# LANGUAGE ApplicativeDo #-}

module GleanCLI (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Foldable
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import Data.Time.Clock.POSIX
import Options.Applicative
import System.IO

import Control.Concurrent.Stream (streamWithThrow)
import Foreign.CPP.Dynamic (parseJSON)
import Thrift.Protocol.JSON (serializeJSON)
import Util.Control.Exception
import Util.EventBase
import Util.IO
import Util.OptParse

import qualified Glean
import Glean.BuildInfo
import qualified Glean.Database.Work as Database
import Glean.Database.Schema
import Glean.Datasource.Scribe.Write
import Glean.Derive
import Glean.Types as Thrift hiding (ValidateSchema)
import qualified Glean.Types as Thrift
import Glean.Write
import Glean.Write.JSON ( buildJsonBatch )
import Glean.Util.ConfigProvider

data ScribeOptions = ScribeOptions
  { writeFromScribe :: WriteFromScribe
  , scribeCompress :: Bool
  }

data PageOptions = PageOptions
  { pageBytes :: Int
  , pageFacts :: Maybe Int
  }

data Command
  = Write
      { repo :: Repo
      , handle :: Text
      , writeFiles :: [FilePath]
      , create :: Bool
      , dependencies :: Maybe Thrift.Dependencies
      , scribe :: Maybe ScribeOptions
      , finish :: Bool
      , properties :: [(Text,Text)]
      , maxConcurrency :: Int
      , experimentalFasterWriting :: Maybe Glean.SendAndRebaseQueueSettings
      }
  | Finish
      { repo :: Repo
      , handle :: Text
      , task :: Maybe Text
      , parcel :: Maybe Int
      , failure :: Maybe Text
      }
  | Unfinish
      {repo :: Repo
      , handle :: Text
      }
  | Dump
      { repo :: Repo
      , dumpFile :: FilePath
      }
  | Delete
      { repo :: Repo
      }
  | Derive
      { repo :: Repo
      , predicates :: [Text]
      , pageOptions :: PageOptions
      , maxConcurrency :: Int
      }
  | Query
      { repoSpec :: Either Text Repo
      , query :: String
      , recurse :: Bool
      , pageOptions :: PageOptions
      , limitFacts :: Maybe Int
      , output :: Maybe FilePath
      , statsOutput :: Maybe FilePath
      , timeout :: Maybe Int64
      , omitResults :: Bool
      }
  | Restore
      { what :: WhatToRestore
      }
  | Validate
      { repo :: Repo
      , validate :: Glean.Validate
      }
  | ValidateSchema
      { file :: FilePath
      }
  | Stats
      { repo :: Repo
      , perPredicate :: Bool
      }
  | Ownership
      { repo :: Repo
      }

data Config = Config
  { cfgService :: Glean.Service
  , cfgCommand :: Command
  }

data WhatToRestore
  = RestoreLocator Text
  | RestoreRepo Repo
  | RestoreRepoOnDay Text Day

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Create, manipulate and query Glean databases")
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      cfgCommand <- asum
        [ createCmd
        , writeCmd
        , finishCmd
        , dumpCmd
        , deleteCmd
        , deriveCmd
        , queryCmd
        , restoreCmd
        , validateCmd
        , validateSchemaCmd
        , statsCommand
        , unfinishCmd
        , ownershipCmd
        ]
      return Config{..}

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

    experimentalFasterWritingOptions
      :: Parser (Maybe Glean.SendAndRebaseQueueSettings)
    experimentalFasterWritingOptions = do
        experimentalFasterWritingFlag <-
          switch (long "experimental-faster-writing")
        sendAndRebaseQueue <- Glean.sendAndRebaseQueueOptions
        return $ if experimentalFasterWritingFlag then
            Just sendAndRebaseQueue
        else
          Nothing

    unfinishCmd::Parser Command
    unfinishCmd =
      commandParser "unfinish"
        (progDesc $ "Unfinish a local database "<>
          "(turn it from complete to incomplete state)")
        $ do
        repo <- repoOpts
        handle <- handleOpt
        return Unfinish{..}

    createCmd :: Parser Command
    createCmd =
      commandParser "create" (progDesc "Create a new database") $ do
        repo <- repoOpts
        writeFiles <- fileArgs
        finish <- finishOpt
        scribe <- Just <$> scribeOptions <|> pure Nothing
        dependencies <- optional $ Thrift.Dependencies_stacked
          <$> option (maybeReader Glean.parseRepo)
          (  long "stacked"
          <> metavar "REPO"
          <> help "Create a stacked database"
          )
        properties <- many $ option readProperty
          (  long "property"
          <> metavar "NAME=VALUE"
          <> help "Set properties when creating a DB"
          )
        handle <- handleOpt
        maxConcurrency <- maxConcurrencyOpt
        experimentalFasterWriting <- experimentalFasterWritingOptions
        return Write{create=True, ..}

    readProperty :: ReadM (Text,Text)
    readProperty = eitherReader $ \str ->
      case break (=='=') str of
        (name, '=':value) -> Right (Text.pack name, Text.pack value)
        _other -> Left "--property: expecting NAME=VALUE"

    writeCmd :: Parser Command
    writeCmd =
      commandParser "write" (progDesc "Write facts to a database") $ do
        ~(repo, scribe) <-
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
        handle <- handleOpt
        maxConcurrency <- maxConcurrencyOpt
        experimentalFasterWriting <- experimentalFasterWritingOptions
        return Write{create=False, properties=[], dependencies=Nothing, ..}

    finishCmd :: Parser Command
    finishCmd =
      commandParser "finish"
        (progDesc "Notify server that a database is complete") $ do
        repo <- repoOpts
        task <- optional $ textOption
          (  long "task"
          <> metavar "NAME"
          <> internal
          )
        parcel <- optional $ option auto
          (  long "parcel"
          <> metavar "NUMBER"
          <> internal
          )
        failure <- optional $ textOption
          (  long "error"
          <> metavar "MESSAGE"
          )
        handle <- handleOpt
        return Finish{..}

    dumpCmd :: Parser Command
    dumpCmd =
      commandParser "dump"
        (progDesc "Dump the contents of the specified database into a file")
        $ do
        repo <- repoOpts
        dumpFile <- strArgument
          (  metavar "FILE"
          <> help "Destination file path"
          )
        return Dump{..}

    deleteCmd :: Parser Command
    deleteCmd =
      commandParser "delete" (progDesc "Delete a database") $ do
        Delete <$> repoOpts

    deriveCmd :: Parser Command
    deriveCmd =
      commandParser "derive" (progDesc "Derive and store a predicate") $ do
        repo <- repoOpts
        maxConcurrency <- maxConcurrencyOpt
        pageOptions <- pageOpts
        predicates <- many $ strArgument
          ( metavar "PREDICATE"
          <> help "predicates to derive"
          )
        return Derive{..}

    queryCmd :: Parser Command
    queryCmd =
      commandParser "query" (progDesc "Execute an Angle query") $ do
        repoSpec <- Left <$> repoNameOpt <|> Right <$> repoSlash
        pageOptions <- pageOpts
        recurse <- switch $ long "recursive"
          <> help "fetch nested facts (slower)"
        limitFacts <- optional $ option auto
          ( long "limit"
          <> metavar "FACTS"
          <> help "maximum number of facts to query"
          )
        output <- optional $ strOption
          ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "output the facts to a file"
          )
        statsOutput <- optional $ strOption
          ( long "stats"
          <> metavar "FILE"
          <> help "output stats to a file ('-' for stdout)"
          )
        timeout <- optional $ option auto
          ( long "timeout"
          <> metavar "MILLISECONDS"
          <> help "Override the default query timeout"
          )
        query <- strArgument
          ( metavar "QUERY"
          <> help "query to execute ('@file' to read from file, '-' for stdin)"
          )
        omitResults <- switch $ long "omit-results"
          <> help "don't print results; use with --stat to get a count of results"
        return Query{..}

    restoreCmd :: Parser Command
    restoreCmd =
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

    validateCmd :: Parser Command
    validateCmd =
      commandParser "validate" (progDesc "Validate a local database") $ do
        repo <- repoOpts
        no_typecheck <- switch
          (  long "no-typecheck"
          <> help "don't typecheck facts"
          )
        no_keys <- switch
          (  long "no-keys"
          <> help "don't verify key uniqueness"
          )
        limit <- optional $ option auto
          (  long "limit"
          <> metavar "N"
          <> help "only validate the first N facts"
          )
        return Validate
          { repo = repo
          , validate = def
              { Glean.validateTypecheck = not no_typecheck
              , Glean.validateKeys = not no_keys
              , Glean.validateLimit = limit
              }
          }

    validateSchemaCmd :: Parser Command
    validateSchemaCmd =
      commandParser "validate-schema" (progDesc "Validate a schema") $ do
        file <- strArgument
          ( metavar "FILE"
          <> help "Name of schema file"
          )
        return (ValidateSchema file)

    statsCommand :: Parser Command
    statsCommand =
      commandParser "stats" (progDesc "Get fact counts and sizes") $ do
        repo <- repoOpts
        perPredicate <- perPredicateOpt
        return Stats{..}

    ownershipCmd :: Parser Command
    ownershipCmd =
      commandParser "ownership" (progDesc "") $ do
        repo <- repoOpts
        return Ownership{ repo = repo }

    parseDay = return .
      parseTimeOrError False defaultTimeLocale (iso8601DateFormat Nothing)

    dayOpt = option (eitherReader parseDay)
      (long "date" <> metavar "YYYY-MM-DD")

    fileArgs = many $ strArgument
      (  metavar "FILE"
      <> help "File of facts (JSON)"
      )

    finishOpt = switch
      (  long "finish"
      <> help "also mark the DB as complete")

    repoOpts :: Parser Repo
    repoOpts = repoSlash <|> repoNameHash

    repoSlash = do
      option (maybeReader Glean.parseRepo)
        (  long "repo"
        <> metavar "NAME/HASH"
        <> help "identifies the repository"
        )

    repoNameHash :: Parser Repo
    repoNameHash = Repo <$> repoNameOpt <*> repoHashOpt

    repoNameOpt :: Parser Text
    repoNameOpt = textOption
        (  long "repo-name"
        <> metavar "NAME"
        <> help "name of the repository"
        )

    repoHashOpt :: Parser Text
    repoHashOpt = textOption
        (  long "repo-hash"
        <> metavar "HASH"
        <> help "hash of the repository"
        )

    handleOpt :: Parser Text
    handleOpt = textOption
      (  long "handle"
      <> metavar "HANDLE"
      <> value (buildRule <> "@" <> buildRevision)
      )

    maxConcurrencyOpt :: Parser Int
    maxConcurrencyOpt = option auto
      (  long "maxConcurrency"
      <> short 'j'
      <> metavar "NUMBER"
      <> showDefault
      <> value 20
      )

    perPredicateOpt :: Parser Bool
    perPredicateOpt = switch
      ( long "per-predicate" )

    pageOpts :: Parser PageOptions
    pageOpts = do
      pageBytes <- option auto
        ( long "page-bytes"
        <> value 1000000
        <> metavar "BYTES"
        <> help "maximum number of bytes per page"
        )
      pageFacts <- optional $ option auto
        ( long "page-facts"
        <> metavar "FACTS"
        <> help "maximum number of facts per page"
        )
      return PageOptions{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  Glean.withBackendWithDefaultOptions evb cfgAPI cfgService $ \backend -> do
    let
      finished repo handle task parcel failure = do
        Glean.workFinished backend WorkFinished
          { workFinished_work = def
            { work_repo = repo
            , work_task = fromMaybe "" task
            , work_parcelIndex = maybe 0 fromIntegral parcel
            , work_handle = handle
            }
          , workFinished_outcome = case failure of
              Nothing -> Outcome_success def
              Just msg -> Outcome_failure (Thrift.Failure msg)
          }

      write repo files max Nothing (Just fasterWriting) = do
        schemaInfo <- Glean.getSchemaInfo backend repo
        dbSchema <- fromSchemaInfo schemaInfo readWriteContent
        logMessages <- newTQueueIO
        let inventory = schemaInventory dbSchema
        Glean.withSendAndRebaseQueue backend repo inventory fasterWriting $
          \queue ->
            streamWithThrow max (forM_ files) $ \file -> do
              r <- Foreign.CPP.Dynamic.parseJSON =<< B.readFile file
              val <- either (throwIO  . ErrorCall . ((file ++ ": ") ++) .
                Text.unpack) return r
              batches <- case Aeson.parse parseJsonFactBatches val of
                Aeson.Error str -> throwIO $ ErrorCall $ file ++ ": " ++ str
                Aeson.Success x -> return x
              batch <- buildJsonBatch dbSchema Nothing batches
              _ <- Glean.writeSendAndRebaseQueue queue batch $
                \_ -> writeTQueue logMessages $ "Wrote " <> file
              atomically (flushTQueue logMessages) >>= mapM_ putStrLn
              return ()
        atomically (flushTQueue logMessages) >>= mapM_ putStrLn

      write repo files max scribe _fasterWriting =
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

      resultToFailure Right{} = Nothing
      resultToFailure (Left err) = Just (show err)

    case cfgCommand of
      Write{..} ->
        tryBracket
           (when create $ do
                putStrLn $ "Creating DB using handle " ++ Text.unpack handle
                void $ Glean.kickOffDatabase backend def
                  { kickOff_repo = repo
                  , kickOff_fill = Just $ case scribe of
                      Nothing -> KickOffFill_writeHandle handle
                      Just scribe -> KickOffFill_scribe
                        (writeFromScribe scribe)
                          { writeFromScribe_writeHandle = handle }
                  , kickOff_properties = HashMap.fromList properties
                  , kickOff_dependencies = dependencies
                  }
           )
           (\_ result ->
             let mFail = resultToFailure result in
             if finish then
               finished repo handle Nothing Nothing (fmap Text.pack mFail)
             else
               let writeFail err = die 3 $ "DB write failure: " ++ err in
               maybe (return ()) writeFail mFail)
           (\_ ->
              write
                repo
                writeFiles
                maxConcurrency
                scribe
                experimentalFasterWriting)

      Finish{..} -> finished repo handle task parcel failure

      Unfinish{..} -> do
        case Glean.backendKind backend of
          Glean.BackendEnv env -> do
            Database.unfinishDatabase env repo handle
          _ -> die 5 "It is NOT possible to unfinish a remote database"


      Delete{..} -> void $ Glean.deleteDatabase backend repo

      Dump{..} -> dumpJsonToFile backend repo dumpFile

      Validate{..} -> case Glean.backendKind backend of
        Glean.BackendEnv env -> Glean.validate env repo validate
        _ -> die 2 "Can't validate a remote database"

      ValidateSchema{..} -> do
        str <- B.readFile file
        Glean.validateSchema backend (Thrift.ValidateSchema str)

      Restore{..} -> do
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
                     unPosixEpochTime t)
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
            ListDatabasesResult{..} <- Glean.listDatabases backend def
            case [ db | db <- listDatabasesResult_databases
                      , database_location db == Just locator ] of
              [] -> do
                die 1 $ "error: server claims " <> Text.unpack locator <>
                  " is not being restored"
              [db]
                | database_status db == Just DatabaseStatus_Restoring ->
                  threadDelay 1000000 >> wait locator
                | database_status db == Just DatabaseStatus_Complete ->
                  return ()
                | otherwise ->
                  die 1 $ "error: unexpected database status: " <>
                    show (database_status db)
              _ -> die 1 $ "error: server has multiple DBs with the locator "
                <> Text.unpack locator

      Derive{..} ->
        let threads = min maxConcurrency (length predicates) in
        streamWithThrow threads (forM_ predicates) $ \pred ->
          derivePredicate backend repo
            (Just $ fromIntegral $ pageBytes pageOptions)
            (fromIntegral <$> pageFacts pageOptions)
            (parseRef pred)

      Query{..} -> do
        query_bytes <- case query of
          "-" -> B.hGetContents stdin
          '@':path -> B.readFile path
          _ -> return $ Text.encodeUtf8 $ Text.pack query

        repo <- case repoSpec of
          Left name -> Glean.getLatestRepo backend name
          Right repo -> return repo

        let with_output f = case output of
              Just path -> withFile path WriteMode f
              Nothing -> f stdout

            with_stats_output f = case statsOutput of
              Just path ->
                (if path == "-" then ($ stdout) else withFile path WriteMode) $
                \out -> f $ B8.hPutStrLn out . maybe "{}" serializeJSON
              Nothing -> f $ const $ return ()

        with_output $ \h_out -> with_stats_output $ \print_stats -> do
        let subtract_limit Nothing _ = Just Nothing
            subtract_limit (Just m) n
              | m > n = Just $ Just (m-n)
              | otherwise = Nothing

            loop cont limit = do
              UserQueryResults{..} <- Glean.userQuery backend repo def
                { userQuery_query = query_bytes
                , userQuery_encodings = [UserQueryEncoding_json
                    def{ userQueryEncodingJSON_expand_results = recurse }]
                , userQuery_options = Just def
                    { userQueryOptions_max_results =
                        case (limit, fromIntegral <$> pageFacts pageOptions) of
                          (Just m, Just n) -> Just $ m `min` n
                          (x,y) -> x <|> y
                    , userQueryOptions_max_bytes =
                        Just $ fromIntegral $ pageBytes pageOptions
                    , userQueryOptions_max_time_ms = timeout
                    , userQueryOptions_continuation = cont
                    , userQueryOptions_syntax = QuerySyntax_ANGLE
                    , userQueryOptions_recursive = recurse
                    , userQueryOptions_omit_results = omitResults
                    }
                }
              n <- case userQueryResults_results of
                UserQueryEncodedResults_json UserQueryResultsJSON{..} -> do
                  mapM_ (B8.hPutStrLn h_out) userQueryResultsJSON_facts
                  return $ length userQueryResultsJSON_facts
                _ -> die 1 "error: unexpected results encoding"
              print_stats userQueryResults_stats
              case userQueryResults_continuation of
                Just new_cont
                  | Just new_limit <- subtract_limit limit $ fromIntegral n ->
                      loop (Just new_cont) new_limit
                _ -> return ()
        loop Nothing $ fromIntegral <$> limitFacts

      Stats{..} -> do
        stats <- Map.toList <$> Glean.predicateStats backend repo
        let totalCount = sum [ predicateStats_count
              | (_name, PredicateStats{..}) <- stats ]
            totalSize = sum [ predicateStats_size
              | (_name, PredicateStats{..}) <- stats ]
        putStrLn $ unwords
          ["total:"
          , show (length stats)
          , "predicates"
          , show totalCount
          , "facts"
          , show totalSize
          , "bytes" ]
        when perPredicate $ do
          SchemaInfo{..} <- Glean.getSchemaInfo backend repo
          let format (pid, Thrift.PredicateStats{..}) =
                let Just PredicateRef{..} =
                      Map.lookup pid schemaInfo_predicateIds
                    name = Text.unpack predicateRef_name <> "."
                            <> show predicateRef_version
                in unwords
                  [ "predicate:"
                  , name
                  , show predicateStats_count
                  , "facts"
                  , show predicateStats_size
                  , "bytes" ]
          mapM_ putStrLn (sort (map format stats))

      Ownership{..} -> case Glean.backendKind backend of
        Glean.BackendEnv env -> Glean.computeOwnership env repo
        _ -> die 2 "Need local database to compute ownership"

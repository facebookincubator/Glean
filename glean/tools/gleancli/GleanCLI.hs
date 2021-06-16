{-# LANGUAGE ApplicativeDo, TypeApplications, AllowAmbiguousTypes #-}

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
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Proxy
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

import qualified Glean hiding (options)
import qualified Glean.LocalOrRemote as Glean
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

data Config = Config
  { cfgService :: Glean.Service
  , cfgCommand :: PluginCommand
  }

class Plugin c where
  parseCommand :: Parser c
  runCommand :: Glean.LocalOrRemote b => b -> c -> IO ()

data PluginType where
  PluginType :: forall c . Plugin c => Proxy c -> PluginType

plugin :: forall c . Plugin c => PluginType
plugin = PluginType (Proxy @c)

data PluginCommand where
  PluginCommand :: forall c . Plugin c => c -> PluginCommand

data UnfinishCommand
  = Unfinish
      { repo :: Repo
      , handle :: Text
      }

instance Plugin UnfinishCommand where
  parseCommand =
    commandParser "unfinish"
      (progDesc $ "Unfinish a local database "<>
        "(turn it from complete to incomplete state)")
      $ do
      repo <- repoOpts
      handle <- handleOpt
      return Unfinish{..}

  runCommand backend Unfinish{..} = do
    case Glean.backendKind backend of
      Glean.BackendEnv env -> do
        Database.unfinishDatabase env repo handle
      _ -> die 5 "It is NOT possible to unfinish a remote database"


data ScribeOptions = ScribeOptions
  { writeFromScribe :: WriteFromScribe
  , scribeCompress :: Bool
  }

data PageOptions = PageOptions
  { pageBytes :: Int
  , pageFacts :: Maybe Int
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
      , experimentalFasterWriting :: Maybe Glean.SendAndRebaseQueueSettings
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
        experimentalFasterWriting <- experimentalFasterWritingOptions
        return Write{create=True, ..}

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
        experimentalFasterWriting <- experimentalFasterWritingOptions
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

  runCommand backend Write{..} =
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
            experimentalFasterWriting)
    where
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

    write repo files max scribe _fasterWriting = do
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


finished
  :: Glean.Backend b
  => b
  -> Repo
  -> Text -- ^ handle
  -> Maybe Text -- ^ task
  -> Maybe Int -- ^ parcel
  -> Maybe Text -- ^ failure?
  -> IO ()
finished backend repo handle task parcel failure = do
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
    `catch` \e@Retry{} ->
       die 1 $
         "finish: " <> show e <> "\n" <>
         "  This error indicates that previous write or derive\n" <>
         "  operations have not completed yet. Please ensure that\n" <>
         "  all writing operations have completed before invoking\n" <>
         "  'glean finish'"


data FinishCommand
  = Finish
      { finishRepo :: Repo
      , finishHandle :: Text
      , task :: Maybe Text
      , parcel :: Maybe Int
      , failure :: Maybe Text
      }

instance Plugin FinishCommand where
  parseCommand =
    commandParser "finish"
      (progDesc "Notify server that a database is complete") $ do
      finishRepo <- repoOpts
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
      finishHandle <- handleOpt
      return Finish{..}

  runCommand backend Finish{..} =
    finished backend finishRepo finishHandle task parcel failure

data DumpCommand
  = Dump
      { dumpRepo :: Repo
      , dumpFile :: FilePath
      }

instance Plugin DumpCommand where
  parseCommand =
    commandParser "dump"
      (progDesc "Dump the contents of the specified database into a file")
      $ do
      dumpRepo <- repoOpts
      dumpFile <- strArgument
        (  metavar "FILE"
        <> help "Destination file path"
        )
      return Dump{..}

  runCommand backend Dump{..} =
    Glean.dumpJsonToFile backend dumpRepo dumpFile

data DeleteCommand
  = Delete
      { deleteRepo :: Repo
      }

instance Plugin DeleteCommand where
  parseCommand =
    commandParser "delete" (progDesc "Delete a database") $ do
      Delete <$> repoOpts

  runCommand backend Delete{..} =
    void $ Glean.deleteDatabase backend deleteRepo

data DeriveCommand
  = Derive
      { deriveRepo :: Repo
      , predicates :: [Text]
      , derivePageOptions :: PageOptions
      , deriveMaxConcurrency :: Int
      }

instance Plugin DeriveCommand where
  parseCommand =
    commandParser "derive" (progDesc "Derive and store a predicate") $ do
      deriveRepo <- repoOpts
      deriveMaxConcurrency <- maxConcurrencyOpt
      derivePageOptions <- pageOpts
      predicates <- many $ strArgument
        ( metavar "PREDICATE"
        <> help "predicates to derive"
        )
      return Derive{..}

  runCommand backend Derive{..} =
    let threads = min deriveMaxConcurrency (length predicates) in
    streamWithThrow threads (forM_ predicates) $ \pred ->
      derivePredicate backend deriveRepo
        (Just $ fromIntegral $ pageBytes derivePageOptions)
        (fromIntegral <$> pageFacts derivePageOptions)
        (parseRef pred)

data QueryCommand
  = Query
      { repoSpec :: Either Text Repo
      , query :: String
      , recurse :: Bool
      , queryPageOptions :: PageOptions
      , limitFacts :: Maybe Int
      , output :: Maybe FilePath
      , statsOutput :: Maybe FilePath
      , timeout :: Maybe Int64
      , omitResults :: Bool
      }

instance Plugin QueryCommand where
  parseCommand =
    commandParser "query" (progDesc "Execute an Angle query") $ do
      repoSpec <- Left <$> repoNameOpt <|> Right <$> repoSlash
      queryPageOptions <- pageOpts
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

  runCommand backend Query{..} = do
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
                    case (limit, fromIntegral <$> pageFacts queryPageOptions) of
                      (Just m, Just n) -> Just $ m `min` n
                      (x,y) -> x <|> y
                , userQueryOptions_max_bytes =
                    Just $ fromIntegral $ pageBytes queryPageOptions
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

  runCommand backend Restore{..} = do
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



data ValidateCommand
  = Validate
      { validateRepo :: Repo
      , validate :: Glean.Validate
      }

instance Plugin ValidateCommand where
  parseCommand =
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
        { validateRepo = repo
        , validate = def
            { Glean.validateTypecheck = not no_typecheck
            , Glean.validateKeys = not no_keys
            , Glean.validateLimit = limit
            }
        }

  runCommand backend Validate{..} = case Glean.backendKind backend of
    Glean.BackendEnv env -> Glean.validate env validateRepo validate
    _ -> die 2 "Can't validate a remote database"

data ValidateSchemaCommand
  = ValidateSchema
      { file :: FilePath
      }

instance Plugin ValidateSchemaCommand where
  parseCommand =
    commandParser "validate-schema" (progDesc "Validate a schema") $ do
      file <- strArgument
        ( metavar "FILE"
        <> help "Name of schema file"
        )
      return (ValidateSchema file)

  runCommand backend ValidateSchema{..} = do
    str <- B.readFile file
    Glean.validateSchema backend (Thrift.ValidateSchema str)

data StatsCommand
  = Stats
      { statsRepo :: Repo
      , perPredicate :: Bool
      }

instance Plugin StatsCommand where
  parseCommand =
    commandParser "stats" (progDesc "Get fact counts and sizes") $ do
      statsRepo <- repoOpts
      perPredicate <- switch ( long "per-predicate" )
      return Stats{..}

  runCommand backend Stats{..} = do
    stats <- Map.toList <$> Glean.predicateStats backend statsRepo
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
      SchemaInfo{..} <- Glean.getSchemaInfo backend statsRepo
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

data OwnershipCommand
  = Ownership
      { ownershipRepo :: Repo
      }

instance Plugin OwnershipCommand where
  parseCommand =
    commandParser "ownership" (progDesc "") $ do
      ownershipRepo <- repoOpts
      return Ownership{..}

  runCommand backend Ownership{..} = case Glean.backendKind backend of
    Glean.BackendEnv env -> Glean.computeOwnership env ownershipRepo
    _ -> die 2 "Need local database to compute ownership"

repoOpts :: Parser Repo
repoOpts = repoSlash <|> repoNameHash

repoSlash :: Parser Repo
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

fileArgs :: Parser [FilePath]
fileArgs = many $ strArgument
  (  metavar "FILE"
  <> help "File of facts (JSON)"
  )

plugins :: [PluginType]
plugins =
  [ plugin @WriteCommand
  , plugin @FinishCommand
  , plugin @UnfinishCommand
  , plugin @DumpCommand
  , plugin @DeleteCommand
  , plugin @DeriveCommand
  , plugin @QueryCommand
  , plugin @RestoreCommand
  , plugin @ValidateCommand
  , plugin @ValidateSchemaCommand
  , plugin @StatsCommand
  , plugin @OwnershipCommand
  ]

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Create, manipulate and query Glean databases")
  where
    parser :: Parser Config
    parser = do
      cfgService <- Glean.options
      cfgCommand <- asum
        [ PluginCommand <$> parseCommand @c
        | PluginType (Proxy :: Proxy c) <- plugins
        ]
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  Glean.withBackendWithDefaultOptions evb cfgAPI cfgService $ \backend -> do
    let
    case cfgCommand of
      PluginCommand c -> runCommand backend c

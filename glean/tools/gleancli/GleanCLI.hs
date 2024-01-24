{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, ApplicativeDo, TypeApplications, AllowAmbiguousTypes #-}

module GleanCLI (main) where

import Control.Exception
import Control.Monad
import qualified Data.Bifunctor
import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.Default
import Data.Either
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.List (isInfixOf)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import Options.Applicative.Help (vcat)
import System.IO

import Util.EventBase
import Util.IO
import Util.OptParse
import Util.Timing (timeIt, showTime, showAllocs)
import System.Exit (exitWith, ExitCode(..))

import qualified Glean
import Glean.Init
import qualified Glean.LocalOrRemote as Glean
import Glean.Database.Config (cfgSchemaSource, cfgSchemaId, cfgServerConfig)
import qualified Glean.Database.Config as GleanDB
import Glean.Database.Schema
  (newDbSchema, readWriteContent, DbSchema (schemaInventory))
import Glean.Database.Schema.Types
  (SchemaSelector(SpecificSchemaId, LatestSchemaAll))
import qualified Glean.Database.Work as Database
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.Schema.Util
import qualified Glean.ServerConfig.Types as Server
import Glean.Types as Thrift hiding (ValidateSchema)
import qualified Glean.Types as Thrift
import Glean.Impl.ConfigProvider
import Glean.Util.ConfigProvider
import Glean.Util.ShellPrint
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Shell

import GleanCLI.Backup
import GleanCLI.Common
import GleanCLI.Complete
import GleanCLI.Derive
import GleanCLI.Index
import GleanCLI.Restore
import GleanCLI.Query
import GleanCLI.Types
import GleanCLI.Utils
import GleanCLI.Write

#if GLEAN_FACEBOOK
import GleanCLI.Facebook
import qualified Glean.Database.Backup.Manifold as Manifold
#endif

data Config = Config
  { cfgService :: Glean.Service
  , cfgCommand :: PluginCommand
  }

data PluginType where
  PluginType :: forall c . Plugin c => Proxy c -> PluginType

plugin :: forall c . Plugin c => PluginType
plugin = PluginType (Proxy @c)

data PluginCommand where
  PluginCommand :: forall c . Plugin c => c -> PluginCommand

plugins :: [PluginType]
plugins =
  [ plugin @WriteCommand
  , plugin @FinishCommand
  , plugin @UnfinishCommand
  , plugin @ListCommand
  , plugin @LatestDbCommand
  , plugin @StatusCommand
  , plugin @DumpCommand
  , plugin @DeleteCommand
  , plugin @DeriveCommand
  , plugin @QueryCommand
  , plugin @BackupCommand
  , plugin @RestoreCommand
  , plugin @ValidateCommand
  , plugin @ValidateSchemaCommand
  , plugin @StatsCommand
  , plugin @PropertiesCommand
  , plugin @SetPropertyCommand
  , plugin @WriteSerializedInventoryCommand
  , plugin @ShellCommand
  , plugin @CompleteCommand
  , plugin @IndexCommand
  , plugin @ScriptCommand
#if GLEAN_FACEBOOK
  , plugin @FacebookPlugin
#endif
  ]

options :: ParserInfo (Config, ConfigOptions ConfigAPI)
options = info (((,) <$> parser <*> configOptions) <**> helper <**> helpAll)
  (fullDesc <> progDesc "Create, manipulate and query Glean databases")
  where
#if MIN_VERSION_optparse_applicative(0,16,0)
    parseError = ShowHelpText Nothing
#else
    parseError = ShowHelpText
#endif

    helpAll :: Parser (a -> a)
    helpAll = abortOption parseError $ mconcat
      [ long "help-all"
      , help "Show all possible options."
      , hidden
      ]

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
  let
    spec =
      setTransformGflags tr $
      setPrefs subparserInline $
      setFromFilePrefix '@' $
      parserInfo options

    tr (opts, _) = case cfgCommand opts of
      PluginCommand c -> argTransform c
  in
  withOptionsGen spec $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  case cfgCommand of
    PluginCommand c ->
      withService evb cfgAPI
        (liftServerConfig (serverConfigTransform c) $
          withRemoteBackups evb cfgService)
        c

withRemoteBackups :: EventBaseDataplane -> Glean.Service -> Glean.Service
withRemoteBackups _evb = liftConfig
#if GLEAN_FACEBOOK
  (Manifold.withManifoldBackups _evb)
#else
  id
#endif

liftConfig
  :: (GleanDB.Config -> GleanDB.Config)
  -> Glean.Service -> Glean.Service
liftConfig f (Glean.Local cfg log) = Glean.Local (f cfg) log
liftConfig _ other@Glean.Remote{} = other

liftServerConfig
  :: (Server.Config -> Server.Config)
  -> Glean.Service -> Glean.Service
liftServerConfig f =
  liftConfig $ \cfg -> cfg{cfgServerConfig = f <$> cfgServerConfig cfg}

-- -----------------------------------------------------------------------------
-- Commands

-- A few small commands that don't deserve their own modules.

data ScriptCommand
  = Script
      { scriptInput :: String
      , scriptTime :: Maybe String
      , scriptVerbose :: Bool
      }

instance Plugin ScriptCommand where
  parseCommand =
    commandParser "script"
      (progDesc "Execute multiple commands"
      <> footerDoc (Just $ vcat
          [ "Example: glean --db-memory script <<EOF"
          , "create --db foo/1"
          , "write --db foo/1 --finish /some/batch"
          , "query --db foo/1 \"src.File _\""
          , "EOF"
          ]))
      $ do
          scriptVerbose <- switch (
            short 'v' <>
            long "verbose" <>
            help "Print executed commands"
            )
          scriptTime <- optional $ strOption (
            short 't' <>
            long "time" <>
            metavar "FILE" <>
            help "Print duration of individual commands (- means stdout)"
            )
          scriptInput <- strArgument (
            metavar "FILE" <>
            help ("File containing commands to execute (- means read from "
            <> "stdin). Commands are specified just like they would be on the "
            <> "command line. Commands can span multiple lines. A non-indented "
            <> "line starts a new command which spans all subsequent indented "
            <> "lines. Multiple words can be grouped into one argument by "
            <> " quoting (\"...\"). Lines which start with # are ignored.")
            )
          return Script{..}

  runCommand evb cfg backend Script{..} = do
    args <- split [] [] <$> case scriptInput of
      "-" -> getContents
      file -> readFile file
    cmds <- forM args $ handleParseResult . execParserPure
      (prefs subparserInline)
      (info
        (asum
        [ PluginCommand <$> parseCommand @c
        | PluginType (Proxy :: Proxy c) <- plugins
        ])
        mempty)
    let withTimeFile f = case scriptTime of
          Nothing -> f $ const $ return ()
          Just "-" -> f $ \s -> putStrLn s >> hFlush stdout
          Just path -> withFile path WriteMode $ f . hPutStrLn
    withTimeFile $ \time ->
      forM_ (zip3 [1..] (map head args) cmds) $
        \(i, name, PluginCommand cmd) -> do
          when scriptVerbose $
            putStrLn $ "Step " <> show (i::Int) <> ": " <> name
          (t,b,_) <- timeIt $ runCommand evb cfg backend cmd
          time $ unwords [show (i::Int), name, showTime t, showAllocs b]
    where
      split ws cs "" = filter (not . null) $ reverse $ reverse ws : cs
      split ws cs ('#':s) = split ws cs $ dropWhile (/= '\n') s
      split ws cs ('\n':x:s)
        | isSpace x = split ws cs s
        | otherwise = split [] (reverse ws : cs) (x:s)
      split ws cs ('\\':x:s)
        | isSpace x = split ws cs s
      split ws cs (x:s)
        | isSpace x = split ws cs s
      split ws cs s = case word "" s of (w,t) -> split (w:ws) cs t

      word w "" = (reverse w, "")
      word w ('"':s) = string w s
      word w ('\\':c:s)
        | isSpace c = (reverse w, '\\':c:s)
        | otherwise = word (c:w) s
      word w (c:s)
        | isSpace c = (reverse w, c:s)
        | otherwise = word (c:w) s

      string w "" = (reverse w, "")
      string w ('"':s) = word w s
      string w ('\\':c:s) = string (c:w) s
      string w (c:s) = string (c:w) s

data UnfinishCommand
  = Unfinish
      { repo :: Repo
      , handle :: Text
      }

instance Plugin UnfinishCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "unfinish"
      (progDesc $ "Unfinish a local database "<>
        "(turn it from complete to incomplete state)")
      $ do
      repo <- dbOpts
      handle <- handleOpt
      return Unfinish{..}

  runCommand _ _ backend Unfinish{..} = do
    case Glean.backendKind backend of
      Glean.BackendEnv env -> do
        Database.unfinishDatabase env repo handle
      _ -> die 5 "It is NOT possible to unfinish a remote database"

data ListCommand
  = List
      { listDbNames :: [String]
      , listFormat :: Maybe ShellPrintFormat
      , listVerbosity :: DbVerbosity
      }

instance Plugin ListCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "list"
      (progDesc "List databases which match DBNAME")
      $ do
      listDbNames <- many $ strArgument (metavar "DBNAME")
      listFormat <- shellFormatOpt
      listVerbosity <- flag DbSummarise DbDescribe (
        short 'v' <>
        long "verbose" <>
        help "include more details in tty/plain formats"
        )
      return List{..}

  runCommand _ _ backend List{..} = do
    r <- Glean.listDatabases backend def
    let
      repoFilter db str =
        str `isInfixOf` Glean.showRepo repo
        where repo = Thrift.database_repo db
      xs = Thrift.listDatabasesResult_databases r
      dbs = filter f xs
      f db = null listDbNames || any (repoFilter db) listDbNames
    putShellPrintLn listFormat $ dbs `withFormatOpts` listVerbosity

newtype LatestDbCommand
  = LatestDb { dbName :: String }

instance Plugin LatestDbCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "db-latest"
      (progDesc "Return latest DBNAME instance available")
      $ do
      dbName <- strArgument (metavar "DBNAME")
      return LatestDb{..}

  runCommand _ _ backend LatestDb{..} = do
    repoHash <- Thrift.repo_hash
      <$> Glean.getLatestRepo backend (Text.pack dbName)
    putStrLn $ Text.unpack repoHash

data StatusCommand
  = Status
      { statusRepo :: Repo
      , statusFormat :: Maybe ShellPrintFormat
      , statusSetExitCode :: Bool
      }

instance Plugin StatusCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "status"
      (progDesc "Get the status of a db")
      $ do
      statusRepo <- dbOpts
      statusFormat <- shellFormatOpt
      statusSetExitCode <- switch
        (  short 'e'
        <> help "Set an error status code if the db is not complete"
        )
      return Status{..}

  runCommand _ _ backend Status{..} = do
    db <-
      Thrift.getDatabaseResult_database <$>
        Glean.getDatabase backend statusRepo
    putShellPrintLn statusFormat $ db `withFormatOpts` DbSummarise
    when statusSetExitCode $ case exitCode db of
      ExitFailure code -> exitWith $ ExitFailure code
      _ -> return ()
    where
    exitCode db = case Thrift.database_status db of
      Thrift.DatabaseStatus_Complete -> ExitSuccess
      Thrift.DatabaseStatus_Available -> ExitFailure 107
      Thrift.DatabaseStatus_Incomplete -> ExitFailure 101
      Thrift.DatabaseStatus_Restoring -> ExitFailure 102
      Thrift.DatabaseStatus_Broken -> ExitFailure 103
      Thrift.DatabaseStatus_Restorable -> ExitFailure 104
      Thrift.DatabaseStatus_Finalizing -> ExitFailure 105
      Thrift.DatabaseStatus_Missing -> ExitFailure 106

data PropertiesCommand
  = Properties
      { propertiesRepo :: Repo
      , propertiesFormat :: Maybe ShellPrintFormat
      }

instance Plugin PropertiesCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "properties"
      (progDesc "Get the properties of a db")
      $ do
      propertiesRepo <- dbOpts
      propertiesFormat <- shellFormatOpt
      return Properties{..}

  runCommand _ _ backend Properties{..} = do
    db <-
      Thrift.getDatabaseResult_database <$>
        Glean.getDatabase backend propertiesRepo
    putShellPrintLn propertiesFormat $ Thrift.database_properties db

data DumpCommand
  = Dump
      { dumpRepo :: Repo
      , dumpFile :: FilePath
      }

instance Plugin DumpCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "dump"
      (progDesc "Dump the contents of the specified database into a file")
      $ do
      dumpRepo <- dbOpts
      dumpFile <- strArgument
        (  metavar "FILE"
        <> help "Destination file path"
        )
      return Dump{..}

  runCommand _ _ backend Dump{..} =
    Glean.dumpJsonToFile backend dumpRepo dumpFile

newtype DeleteCommand
  = Delete
      { deleteRepo :: Repo
      }

instance Plugin DeleteCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "delete" (progDesc "Delete a database") $ do
      Delete <$> dbOpts

  runCommand _ _ backend Delete{..} =
    void $ Glean.deleteDatabase backend deleteRepo


data ValidateCommand
  = Validate
      { validateRepo :: Repo
      , validate :: Glean.Validate
      }

instance Plugin ValidateCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "validate" (progDesc "Validate a local database") $ do
      repo <- dbOpts
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

  runCommand _ _ backend Validate{..} = case Glean.backendKind backend of
    Glean.BackendEnv env -> Glean.validate env validateRepo validate
    _ -> die 2 "Can't validate a remote database"

newtype ValidateSchemaCommand
  = ValidateSchema
      { file :: FilePath
      }

instance Plugin ValidateSchemaCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "validate-schema" (progDesc "Validate a schema") $ do
      file <- strArgument
        ( metavar "FILE"
        <> help "Name of schema file"
        )
      return (ValidateSchema file)

  runCommand _ _ backend ValidateSchema{..} = do
    str <- B.readFile file
    Glean.validateSchema backend (Thrift.ValidateSchema str)
      `catch` \e@Thrift.Exception{} -> do
        hPrint stderr e
        exitWith (ExitFailure 55)

data StatsCommand
  = Stats
      { statsRepo :: Repo
      , perPredicate :: Bool
      , excludeBase :: Bool
      , statsPredicates :: [Text]
      , statsFormat :: Maybe ShellPrintFormat
      , statsSetExitCode :: Bool
      }

instance Plugin StatsCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "stats" (progDesc "Get fact counts and sizes") $ do
      statsRepo <- dbOpts
      perPredicate <- switch ( long "per-predicate" )
      excludeBase <- switch ( long "exclude-base" )
      statsPredicates <- many $ strArgument (metavar "PREDICATE")
      statsFormat <- shellFormatOpt
      statsSetExitCode <- switch
        (  short 'e'
        <> help
           "Set an error status code if there are no facts for the predicate(s)"
        )
      return Stats{..}

  runCommand _ _ backend Stats{..} = do
    xs <- Map.toList <$>
      Glean.predicateStats backend statsRepo
        (if excludeBase then Glean.ExcludeBase else Glean.IncludeBase)
    schemaInfo <- Glean.getSchemaInfo backend (Just statsRepo)
      def { getSchemaInfo_omit_source = True }
    let
      matchRefsArgs = parseRef <$> statsPredicates
      preds = map (Data.Bifunctor.first (lookupPid schemaInfo)) xs
      filterPred :: Either Thrift.Id PredicateRef -> Bool
      filterPred ref =
        (perPredicate && null matchRefsArgs) -- argument given and
        || any (refMatches ref) matchRefsArgs -- reference matches argument
      refMatches (Right lhs) rhs = predicateMatches lhs rhs
      refMatches (Left _) _ = False
      statsFormatOpts = StatsFormatOpts {
        showTotal = True,
        sortBySize = False }
      refIsPresent ref =
        any ((`refMatches` ref) . fst) preds
      matchRefsArgsNotPresent =
        filter (not . refIsPresent) matchRefsArgs
    putShellPrintLn statsFormat $
      (filterPred, preds) `withFormatOpts` statsFormatOpts
    when (not $ null matchRefsArgsNotPresent) $ do
      forM_ matchRefsArgsNotPresent $ \ref -> do
        hPutStrLn stderr $ Text.unpack $
          "No facts found for: " <> showRef ref
      when statsSetExitCode $
        exitWith $ ExitFailure 100
    where
      lookupPid SchemaInfo{..} pid =
        maybe (Left pid) Right $
          Map.lookup pid schemaInfo_predicateIds
      predicateMatches PredicateRef{..} SourceRef{..} =
          predicateRef_name == sourceRefName &&
          maybe True (== predicateRef_version) sourceRefVersion

data SetPropertyCommand
  = SetProperty
      { setPropRepo :: Repo
      , properties :: [Either Text (Text,Text)]
          -- Left: remove, Right: update/add
      }

instance Plugin SetPropertyCommand where
  parseCommand =
    commandParser "set-property" (progDesc "") $ do
      setPropRepo <- dbOpts
      properties <- many $ argument readOption (metavar "NAME=VALUE or -NAME")
      return SetProperty{..}
    where
    readOption = maybeReader $ \s ->
      case s of
        '-':name -> Just (Left (Text.pack name))
        _ | [name, value] <- splitOn "=" s ->
            Just (Right (Text.pack name, Text.pack value))
          | otherwise -> Nothing

  runCommand _ _ backend SetProperty{..} =
    void $ Glean.updateProperties backend setPropRepo
      (HashMap.fromList (rights properties)) (lefts properties)


data WriteSerializedInventoryCommand
  = WriteSerializedInventory
      { writeSerializedInventoryRepo :: Maybe Repo
      , outputFile :: FilePath
      }

instance Plugin WriteSerializedInventoryCommand where
  serverConfigTransform _ = disableJanitor . disableAutoBackups

  parseCommand =
    commandParser "write-serialized-inventory" (progDesc desc) $ do
      writeSerializedInventoryRepo <- optional dbOpts
      outputFile <- strArgument (metavar "OUTPUT_FILE")
      return WriteSerializedInventory{..}
    where
      desc = "Generate a schema inventory for a schema id (use --schema-id)" ++
             " or a DB"

  withService evb cfgAPI svc WriteSerializedInventory{..} = do
    inventory <- case writeSerializedInventoryRepo of
      Just repo ->
        Glean.withBackendWithDefaultOptions evb cfgAPI svc Nothing $ \backend ->
          Glean.serializeInventory backend repo
      Nothing -> do
        dbSchema <- case svc of
          Glean.Local cfg _ -> do
            index <- ThriftSource.load cfgAPI (cfgSchemaSource cfg)
            let selector =
                  maybe LatestSchemaAll SpecificSchemaId $ cfgSchemaId cfg
            newDbSchema Nothing index selector readWriteContent
          Glean.Remote{} ->
            throwIO $ userError "Please specify either a schema id or a db"
        return $ Inventory.serialize $ schemaInventory dbSchema

    withFile outputFile WriteMode $ \hdl -> do
      B.hPutStr hdl inventory

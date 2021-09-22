-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP, ApplicativeDo, TypeApplications, AllowAmbiguousTypes #-}

module GleanCLI (main) where

import Control.Exception
import Control.Monad
import qualified Data.Bifunctor
import qualified Data.ByteString as B
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Data.List.Split
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.IO
import System.Environment

import Util.Control.Exception
import Util.EventBase
import Util.IO
import Util.OptParse
import System.Exit (exitWith, ExitCode(..))

import qualified Glean hiding (options)
import Glean.Init
import qualified Glean.LocalOrRemote as Glean
import qualified Glean.Database.Work as Database
import Glean.Schema.Util
import Glean.Types as Thrift hiding (ValidateSchema)
import qualified Glean.Types as Thrift
import Glean.Impl.ConfigProvider
import Glean.Util.ConfigProvider
import Glean.Util.ShellPrint
import Glean.Shell

import GleanCLI.Common
import GleanCLI.Complete
import GleanCLI.Derive
import GleanCLI.Restore
import GleanCLI.Query
import GleanCLI.Types
import GleanCLI.Write

#if FACEBOOK
import GleanCLI.Facebook
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
  , plugin @StatusCommand
  , plugin @DumpCommand
  , plugin @DeleteCommand
  , plugin @DeriveCommand
  , plugin @QueryCommand
  , plugin @RestoreCommand
  , plugin @ValidateCommand
  , plugin @ValidateSchemaCommand
  , plugin @StatsCommand
  , plugin @PropertiesCommand
  , plugin @OwnershipCommand
  , plugin @SetPropertyCommand
  , plugin @WriteSerializedInventoryCommand
  , plugin @ShellCommand
  , plugin @CompleteCommand
#if FACEBOOK
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
main = do
  ((Config{..}, cfgOpts), leftOverArgs) <- do
    args <- getArgs
    r <- tryAll $ partialParse (prefs subparserInline) options args
    case r of
      Left e -> withGflags ["--help" | "--help-all" <- args] $ throwIO e
      Right r -> return r

  case cfgCommand of
    PluginCommand c ->
      withGflags (argTransform c leftOverArgs) $
      withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \cfgAPI ->
      withService evb cfgAPI cfgService c


-- -----------------------------------------------------------------------------
-- Commands

-- A few small commands that don't deserve their own modules.

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

  runCommand _ _ backend Unfinish{..} = do
    case Glean.backendKind backend of
      Glean.BackendEnv env -> do
        Database.unfinishDatabase env repo handle
      _ -> die 5 "It is NOT possible to unfinish a remote database"

data ListCommand
  = List
      { listDbNames :: [String]
      , listFormat :: Maybe ShellPrintFormat
      }

instance Plugin ListCommand where
  parseCommand =
    commandParser "list"
      (progDesc "List databases which match REPONAME")
      $ do
      listDbNames <- many $ strArgument (metavar "REPONAME")
      listFormat <- shellFormatOpt
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
    putShellPrintLn listFormat $ dbs `withFormatOpts` DbSummarise

data StatusCommand
  = Status
      { statusRepo :: Repo
      , statusFormat :: Maybe ShellPrintFormat
      , statusSetExitCode :: Bool
      }

instance Plugin StatusCommand where
  parseCommand =
    commandParser "status"
      (progDesc "Get the status of a db")
      $ do
      statusRepo <- repoOpts
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
  parseCommand =
    commandParser "properties"
      (progDesc "Get the properties of a db")
      $ do
      propertiesRepo <- repoOpts
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

  runCommand _ _ backend Dump{..} =
    Glean.dumpJsonToFile backend dumpRepo dumpFile

data DeleteCommand
  = Delete
      { deleteRepo :: Repo
      }

instance Plugin DeleteCommand where
  parseCommand =
    commandParser "delete" (progDesc "Delete a database") $ do
      Delete <$> repoOpts

  runCommand _ _ backend Delete{..} =
    void $ Glean.deleteDatabase backend deleteRepo


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

  runCommand _ _ backend Validate{..} = case Glean.backendKind backend of
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

  runCommand _ _ backend ValidateSchema{..} = do
    str <- B.readFile file
    Glean.validateSchema backend (Thrift.ValidateSchema str)

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
  parseCommand =
    commandParser "stats" (progDesc "Get fact counts and sizes") $ do
      statsRepo <- repoOpts
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
    schemaInfo <- Glean.getSchemaInfo backend statsRepo
    let
      matchRefs = parseRef <$> statsPredicates
      preds = map (Data.Bifunctor.first (lookupPid schemaInfo)) xs
      filterPred :: Either Thrift.Id PredicateRef -> Bool
      filterPred ref =
        (perPredicate && null matchRefs) || refMatches ref
      refMatches (Right pref) = any (predicateMatches pref) matchRefs
      refMatches (Left _) = False
      statsFormatOpts = StatsFormatOpts {
        showTotal = True,
        sortBySize = False }
    putShellPrintLn statsFormat $
      (filterPred, preds) `withFormatOpts` statsFormatOpts
    when (not $ any (refMatches . fst) preds) $ do
      hPutStrLn stderr $ Text.unpack $
        "No facts found for: "
        <> Text.intercalate "," (map showSourceRef matchRefs)
      when statsSetExitCode $
        exitWith $ ExitFailure 100
    where
      lookupPid SchemaInfo{..} pid =
        maybe (Left pid) Right $
          Map.lookup pid schemaInfo_predicateIds
      predicateMatches PredicateRef{..} SourceRef{..} =
          predicateRef_name == sourceRefName &&
          maybe True (== predicateRef_version) sourceRefVersion

data OwnershipCommand
  = Ownership
      { ownershipRepo :: Repo
      }

instance Plugin OwnershipCommand where
  parseCommand =
    commandParser "ownership" (progDesc "") $ do
      ownershipRepo <- repoOpts
      return Ownership{..}

  runCommand _ _ backend Ownership{..} = case Glean.backendKind backend of
    Glean.BackendEnv env -> Glean.computeOwnership env ownershipRepo
    _ -> die 2 "Need local database to compute ownership"

data SetPropertyCommand
  = SetProperty
      { setPropRepo :: Repo
      , properties :: [(Text,Text)]
      }

instance Plugin SetPropertyCommand where
  parseCommand =
    commandParser "set-property" (progDesc "") $ do
      setPropRepo <- repoOpts
      properties <- many $ argument readOption (metavar "NAME=VALUE")
      return SetProperty{..}
    where
    readOption = maybeReader $ \s ->
      case splitOn "=" s of
        [name, value] -> Just (Text.pack name, Text.pack value)
        _ -> Nothing

  runCommand _ _ backend SetProperty{..} =
    void $ Glean.updateProperties backend setPropRepo
      (HashMap.fromList properties) []

data WriteSerializedInventoryCommand
  = WriteSerializedInventory
      { writeSerializedInventoryRepo :: Repo
      , outputFile :: FilePath
      }

instance Plugin WriteSerializedInventoryCommand where
  parseCommand =
    commandParser "write-serialized-inventory" (progDesc "") $ do
      writeSerializedInventoryRepo <- repoOpts
      outputFile <- strArgument (metavar "OUTPUT_FILE")
      return WriteSerializedInventory{..}

  runCommand _ _ backend WriteSerializedInventory{..} = do
    inventory <- Glean.serializeInventory backend writeSerializedInventoryRepo
    withFile outputFile WriteMode $ \hdl -> do
      B.hPutStr hdl inventory

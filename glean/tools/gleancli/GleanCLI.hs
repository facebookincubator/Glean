-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP, ApplicativeDo, TypeApplications, AllowAmbiguousTypes #-}

module GleanCLI (main) where

import Control.Monad
import qualified Data.ByteString as B
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List (sort, isInfixOf)
import Data.List.Split
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

import Util.EventBase
import Util.IO
import Util.TimeSec
import Util.OptParse
import System.IO

import qualified Glean hiding (options)
import qualified Glean.LocalOrRemote as Glean
import qualified Glean.Database.Work as Database
import Glean.Types as Thrift hiding (ValidateSchema)
import qualified Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.ShellPrint

import GleanCLI.Common
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
  , plugin @DumpCommand
  , plugin @DeleteCommand
  , plugin @DeriveCommand
  , plugin @QueryCommand
  , plugin @RestoreCommand
  , plugin @ValidateCommand
  , plugin @ValidateSchemaCommand
  , plugin @StatsCommand
  , plugin @OwnershipCommand
  , plugin @SetPropertyCommand
#if FACEBOOK
  , plugin @FacebookPlugin
#endif
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
    case cfgCommand of
      PluginCommand c -> runCommand evb cfgAPI backend c


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
    isTTY <- hIsTerminalDevice stdout
    let format = fromMaybe (if isTTY then TTY else PlainText) listFormat
    t0 <- now
    putStrLn $ shellPrint False format t0 dbs

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
      }

instance Plugin StatsCommand where
  parseCommand =
    commandParser "stats" (progDesc "Get fact counts and sizes") $ do
      statsRepo <- repoOpts
      perPredicate <- switch ( long "per-predicate" )
      excludeBase <- switch ( long "exclude-base" )
      return Stats{..}

  runCommand _ _ backend Stats{..} = do
    stats <- Map.toList <$>
      Glean.predicateStats backend statsRepo
        (if excludeBase then Glean.ExcludeBase else Glean.IncludeBase)
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

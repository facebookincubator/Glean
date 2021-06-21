{-# LANGUAGE ApplicativeDo, CPP #-}
module Glean.Database.Config (
  Config(..), options,
  schemaSourceConfig,
  catSchemaFiles,
  schemaSourceFiles,
  schemaSourceFilesFromDir,
  schemaSourceDir,
  schemaSourceFile,
  schemaSourceParser,
  schemaSourceOption,
  parseSchemaDir
) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Default
import Data.List
import qualified Data.Text as Text
import Options.Applicative
import System.FilePath

import Util.IO (listDirectoryRecursive)

import Glean.Angle.Types
import qualified Glean.Database.Catalog.Local.Files as Catalog.Local.Files
import qualified Glean.Database.Catalog.Store as Catalog
import Glean.Database.Storage
import qualified Glean.Database.Storage.Memory as Memory
import qualified Glean.Database.Storage.RocksDB as RocksDB
import Glean.DefaultConfigs
import qualified Glean.Recipes.Types as Recipes
import Glean.Schema.Resolve
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Some
import Glean.Util.Trace (Listener)
import Glean.Util.ThriftSource (ThriftSource)
import qualified Glean.Util.ThriftSource as ThriftSource
import qualified Glean.Tailer as Tailer

data Config = Config
  { cfgRoot :: FilePath
  , cfgSchemaSource :: ThriftSource (SourceSchemas, Schemas)
  , cfgSchemaDir :: Maybe FilePath
      -- ^ Records whether we're reading the schema from a directory
      -- of source files or not, because some clients (the shell) want
      -- to know this, and it's not avaialble from the ThriftSource.
  , cfgSchemaOverride :: Bool
      -- ^ If True, when merging the schema stored in the DB with the
      -- current schema, predicates and types from the DB schema are
      -- replaced with those from the current schema. NOTE: this
      -- option is dangerous and is only for testing.
  , cfgSchemaVersion :: Maybe Version
      -- ^ If set, this is the version of the "all" schema that is
      -- used to resolve unversioned predicates in a query.
  , cfgRecipeConfig :: ThriftSource Recipes.Config
  , cfgServerConfig :: ThriftSource ServerConfig.Config
  , cfgStorage :: FilePath -> ServerConfig.Config -> IO (Some Storage)
  , cfgCatalogStore :: FilePath -> IO (Some Catalog.Store)
  , cfgReadOnly :: Bool
  , cfgMockWrites :: Bool
  , cfgTailerOpts :: Tailer.TailerOptions
  , cfgListener :: Listener
      -- ^ A 'Listener' which might get notified about various events related
      -- to databases. This is for testing support only.
  }

instance Show Config where
  show c = unwords [ "Config {"
    , "cfgRoot: " <> cfgRoot c
    , "cfgServerConfig: " <> show (cfgServerConfig c)
    , "}" ]

instance Default Config where
  def = Config
    { cfgRoot = "."
    , cfgSchemaSource = ThriftSource.value (error "undefined schema")
    , cfgSchemaDir = Nothing
    , cfgSchemaOverride = False
    , cfgSchemaVersion = Nothing
    , cfgRecipeConfig = def
    , cfgServerConfig = def
    , cfgStorage = \root scfg -> Some <$> RocksDB.newStorage root scfg
    , cfgCatalogStore = return . Some . Catalog.Local.Files.local
    , cfgReadOnly = False
    , cfgMockWrites = False
    , cfgTailerOpts = def
    , cfgListener = mempty
    }

-- | Read the schema definition from the ConfigProvider
schemaSourceConfig :: ThriftSource (SourceSchemas, Schemas)
schemaSourceConfig =
  ThriftSource.configWithDeserializer schemaConfigPath
     parseAndResolveSchema

-- | Read the schema files from the source tree
schemaSourceFiles :: ThriftSource (SourceSchemas, Schemas)
schemaSourceFiles = schemaSourceFilesFromDir schemaSourceDir

-- | Read the schema from a single file
schemaSourceFile :: FilePath -> ThriftSource (SourceSchemas, Schemas)
schemaSourceFile f = ThriftSource.fileWithDeserializer f parseAndResolveSchema

-- | Read schema files from the given directory
schemaSourceFilesFromDir :: FilePath -> ThriftSource (SourceSchemas, Schemas)
schemaSourceFilesFromDir = ThriftSource.once . parseSchemaDir

-- | Read schema files from a directory
parseSchemaDir :: FilePath -> IO (SourceSchemas, Schemas)
parseSchemaDir dir = do
  str <- catSchemaFiles =<< listDirectoryRecursive dir
  case parseAndResolveSchema str of
    Left err -> throwIO $ ErrorCall $ err
    Right schema -> return schema

-- | Concatenate the contents of all the .angle files, prepending the
-- contents of VERSION if that file exists, and adding "#FILE" annotations
-- so that errors can still be attributed to the right location.
catSchemaFiles :: [FilePath] -> IO ByteString
catSchemaFiles files = do
  let sorted = sort files
  version <- mapM B.readFile (filter ((=="VERSION") . takeFileName) sorted)
  strs <- forM (filter ((== ".angle") . takeExtension) sorted) $ \file -> do
    str <- B.readFile file
    return ("#FILE " <> UTF8.fromString file <> "\n" <> str)
  return $ B.concat (version ++ ("# @" <> "generated\n" : strs))

-- | path to the dir of schema files in the source tree
schemaSourceDir :: FilePath
schemaSourceDir = "glean/schema/source"

-- | Allow short \"config\" and \"dir\"  to choose the defaults from
-- 'schemaSourceConfig' and 'schemaSourceDir' as well as full explicit
-- \"config:PATH\", \"dir:PATH\", and \"file:PATH\" sources.
schemaSourceParser
  :: String
  -> Either String (Maybe FilePath, ThriftSource (SourceSchemas, Schemas))
schemaSourceParser "config" = Right (Nothing, schemaSourceConfig)
schemaSourceParser "dir" =
  Right (Just schemaSourceDir, schemaSourceFilesFromDir schemaSourceDir)
schemaSourceParser s
  | ("dir", ':':path) <- break (==':') s =
    Right (Just path, ThriftSource.once $ parseSchemaDir path)
  -- default to interpreting the argument as a directory:
  | ':' `notElem` s =
    Right (Just s, ThriftSource.once $ parseSchemaDir s)
  | otherwise =
    (Nothing,) <$> ThriftSource.parseWithDeserializer s parseAndResolveSchema

-- | Deprecated --db-schema option; use --schema instead.
dbSchemaSourceOption
  :: Parser (Maybe FilePath, ThriftSource (SourceSchemas, Schemas))
dbSchemaSourceOption = option (eitherReader schemaSourceParser)
  (  long "db-schema"
  <> hidden
  <> metavar "(dir | config | file:PATH | dir:PATH | config:PATH)"
  <> value (Nothing, schemaSourceConfig))

schemaSourceOption
  :: Parser (Maybe FilePath, ThriftSource (SourceSchemas, Schemas))
schemaSourceOption = option (eitherReader schemaSourceParser)
  (  long "schema"
  <> metavar "(dir | config | file:FILE | dir:DIR | config:PATH | DIR)"
  <> value (Nothing, schemaSourceConfig))

options :: Parser Config
options = do
  cfgRoot <- strOption (long "db-root")
  ~(cfgSchemaDir, cfgSchemaSource) <-
    schemaSourceOption <|> dbSchemaSourceOption
  cfgSchemaOverride <- switch (long "db-schema-override")
  cfgSchemaVersion <- optional $ option auto
    ( long "schema-version"
    <> metavar "VERSION"
    <> help (
      "version of \"all\" schema to use when resolving references " <>
      "to unversioned predicates in a query (mostly for testing purposes; " <>
      "defaults to the latest version)")
    )

  cfgRecipeConfig <- recipesConfigThriftSource
  cfgServerConfig <-
    serverConfigThriftSource <|>
    serverConfigTier <|>
    pure def  -- default settings if no option given
  cfgStorage <- storageOption
  cfgReadOnly <- switch (long "db-read-only")
  cfgMockWrites <- switch (long "db-mock-writes")
  cfgTailerOpts <- Tailer.options
  return Config
    { cfgCatalogStore = cfgCatalogStore def
    , cfgListener = mempty
    , .. }
  where
    recipesConfigThriftSource = option (eitherReader ThriftSource.parse)
      (  long "recipe-config"
      <> metavar "(file:PATH | config:PATH)"
      <> value defaultRecipesConfigSource )

    serverConfigThriftSource = option (eitherReader ThriftSource.parse)
      (  long "server-config"
      <> metavar "(file:PATH | config:PATH)" )

    serverConfigTier =
      ThriftSource.config . Text.pack . (serverConfigPath </>) <$>
      strOption
        (  long "tier"
        <> metavar "TIER"
        <> help "specifies the server configuration to load")

    storageOption = option (eitherReader parseStorage)
        (  long "storage"
        <> metavar "(rocksdb | memory)"
        <> value (cfgStorage def))

    parseStorage
      :: String
      -> Either String (FilePath -> ServerConfig.Config -> IO (Some Storage))
    parseStorage "rocksdb" = Right $
      \root scfg -> Some <$> RocksDB.newStorage root scfg
    parseStorage "memory" = Right $ \_ _ -> Some <$> Memory.newStorage
    parseStorage s = Left $ "unsupported storage '" ++ s ++ "'"

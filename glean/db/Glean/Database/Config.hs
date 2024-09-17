{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, CPP #-}
module Glean.Database.Config (
  DataStore(..),
  fileDataStore,
  tmpDataStore,
  memoryDataStore,
  Config(..),
  DebugFlags(..),
  options,
  processSchema,
  processSchemaCached,
  processOneSchema,
  SchemaIndex(..),
  schemaForSchemaId,
  ProcessedSchema(..),
  legacySchemaSourceConfig,
  catSchemaFiles,
  schemaSourceFiles,
  schemaSourceFilesFromDir,
  schemaSourceDir,
  schemaSourceFile,
  schemaSourceIndexFile,
  schemaSourceParser,
  schemaSourceOption,
  parseSchemaDir,
  parseSchemaIndex,
  -- testing
  processSchemaForTesting,
) where

import Control.Exception
import Control.Monad
import Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

import Control.Trace (Tracer)
import Thrift.Protocol.JSON
import Thrift.Util
import Util.IO (listDirectoryRecursive)
import Util.Log (logInfo)

import Glean.Angle.Types
import qualified Glean.Database.Backup.Backend as Backup -- from glean/util
import qualified Glean.Database.Backup.Mock as Backup.Mock
import Glean.Database.Catalog (Catalog)
import qualified Glean.Database.Catalog.Local.Files as Catalog
import qualified Glean.Database.Catalog.Local.Memory as Catalog
import qualified Glean.Database.Catalog.Store as Catalog
import Glean.Database.Schema.ComputeIds
import Glean.Database.Storage
import qualified Glean.Database.Storage.Memory as Memory
import qualified Glean.Database.Storage.RocksDB as RocksDB
import Glean.Database.Trace
import qualified Glean.Internal.Types as Internal
import Glean.DefaultConfigs
import Glean.Logger.Database
import Glean.Logger.Server
import qualified Glean.Recipes.Types as Recipes
import Glean.Schema.Resolve
import Glean.Schema.Types
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types
import Glean.Util.ConfigProvider as Config
import Glean.Util.Observed (Observed)
import Glean.Util.ShardManager
import Glean.Util.Some
import Glean.Util.Trace (Listener)
import Glean.Util.ThriftSource (ThriftSource)
import qualified Glean.Util.ThriftSource as ThriftSource

data DataStore = DataStore
  { withDataStore
      :: forall a. ServerConfig.Config
      -> (forall c s. (Catalog.Store c, Storage s) => c -> s -> IO a)
      -> IO a
  , dataStoreTag :: String
  }

fileDataStore :: FilePath -> DataStore
fileDataStore path = DataStore
  { withDataStore = \scfg f -> do
      rocksdb <- RocksDB.newStorage path scfg
      f (Catalog.fileCatalog path) rocksdb
  , dataStoreTag = "rocksdb:" <> path
  }

tmpDataStore :: DataStore
tmpDataStore = DataStore
  { withDataStore = \scfg f -> withSystemTempDirectory "glean" $ \tmp -> do
      logInfo $ "Storing temporary DBs in " <> tmp
      rocksdb <- RocksDB.newStorage tmp scfg
      f (Catalog.fileCatalog tmp) rocksdb
  , dataStoreTag = "rocksdb:{TMP}"
  }

memoryDataStore :: DataStore
memoryDataStore = DataStore
  { withDataStore = \_ f -> do
      cat <- Catalog.memoryCatalog
      mem <- Memory.newStorage
      f cat mem
  , dataStoreTag = "memory"
  }

data Config = Config
  { cfgDataStore :: DataStore
  , cfgSchemaSource :: ThriftSource SchemaIndex
  , cfgUpdateSchema :: Bool
      -- ^ When True (the default), the schema for open DBs is updated
      -- whenever the global schema changes.
  , cfgSchemaDir :: Maybe FilePath
      -- ^ Records whether we're reading the schema from a directory
      -- of source files or not, because some clients (the shell) want
      -- to know this, and it's not avaialble from the ThriftSource.
  , cfgSchemaId :: Maybe SchemaId
      -- ^ If set, this is the version of the schema that is used to
      -- interpret a query.
  , cfgRecipeConfig :: ThriftSource Recipes.Config
  , cfgServerConfig :: ThriftSource ServerConfig.Config
  , cfgReadOnly :: Bool
  , cfgMockWrites :: Bool
  , cfgListener :: Listener
      -- ^ A 'Listener' which might get notified about various events related
      -- to databases. This is for testing support only.
  , cfgShardManager
    :: forall a
     . Catalog
     -> Observed ServerConfig.Config
     -> (SomeShardManager -> IO a)
     -> IO a
  , cfgServerLogger :: Some GleanServerLogger
    -- ^ Logger for server requests and other events
  , cfgDatabaseLogger :: Some GleanDatabaseLogger
    -- ^ Logger for recording stats of databases produced
  , cfgBackupBackends :: HashMap Text (Some Backup.Backend)
    -- ^ Backup backends
  , cfgEnableRecursion :: Bool
    -- ^ Enable experimental support for recursion
  , cfgFilterAvailableDBs :: [Repo] -> IO [Repo]
    -- ^ Filter out DBs not currently available in the server tier
  , cfgTracer :: Tracer GleanTrace
  , cfgDebug :: DebugFlags
  }

data DebugFlags = DebugFlags
  { tcDebug :: !Bool
  , queryDebug :: !Bool
  }

instance Default DebugFlags where
  def = DebugFlags { tcDebug = False, queryDebug = False }

instance Semigroup DebugFlags where
  a <> b = DebugFlags
    { tcDebug = tcDebug a || tcDebug b
    , queryDebug = queryDebug a || queryDebug b
    }

instance Monoid DebugFlags where
  mempty = def

instance Show Config where
  show c = unwords [ "Config {"
    , "cfgDataStore: " <> dataStoreTag (cfgDataStore c)
    , "cfgServerConfig: " <> show (cfgServerConfig c)
    , "}" ]

instance Default Config where
  def = Config
    { cfgDataStore = fileDataStore "."
    , cfgSchemaSource = ThriftSource.value (error "undefined schema")
    , cfgUpdateSchema = True
    , cfgSchemaDir = Nothing
    , cfgSchemaId = Nothing
    , cfgRecipeConfig = def
    , cfgServerConfig = def
    , cfgReadOnly = False
    , cfgMockWrites = False
    , cfgListener = mempty
    , cfgShardManager = \_ _ k -> k $ SomeShardManager noSharding
    , cfgServerLogger = Some NullGleanServerLogger
    , cfgDatabaseLogger = Some NullGleanDatabaseLogger
    , cfgBackupBackends = HashMap.fromList [("mock", Backup.Mock.mock)]
    , cfgEnableRecursion = False
    , cfgFilterAvailableDBs = return
    , cfgTracer = mempty
    , cfgDebug = def
    }

data SchemaIndex = SchemaIndex
  { schemaIndexCurrent :: ProcessedSchema
  , schemaIndexOlder :: [ProcessedSchema]
  }

schemaForSchemaId :: SchemaIndex -> SchemaId -> Maybe ProcessedSchema
schemaForSchemaId SchemaIndex{..} id = find (containsId id) instances
  where
    instances = schemaIndexCurrent : schemaIndexOlder
    containsId id = Map.member id . hashedSchemaEnvs . procSchemaHashed

-- | The schema that we've read from the filesystem or the configs. We
-- need this in three forms:
--
-- * SourceSchemas: the parsed source, which we'll store back in DBs
--   that we create
--
-- * ResolvedSchemas: after name resolution, which is needed to
--   support "evolves". TODO: just keep the bits we need from this
--
-- * HashedSchema: after hashing all the names (ComputeIds)
--
data ProcessedSchema = ProcessedSchema
  { procSchemaSource :: SourceSchemas
  , procSchemaResolved :: ResolvedSchemas
  , procSchemaHashed :: HashedSchema
  }

processOneSchema
  :: Map SchemaId Version
  -> ByteString
  -> Either String SchemaIndex
processOneSchema versions str =
  case processSchema versions str of
    Left str -> Left str
    Right schema -> Right (SchemaIndex schema [])

processSchema
  :: Map SchemaId Version
  -> ByteString
  -> Either String ProcessedSchema
processSchema = processSchemaForTesting HashMap.toList

-- | Testing version of 'processSchema' that accepts a custom @HashMap.toList@
--   function to weed out ordering assumptions
processSchemaForTesting
  :: (forall k v . HashMap k v -> [(k,v)])
  -> Map SchemaId Version
  -> ByteString
  -> Either String ProcessedSchema
processSchemaForTesting toList versions str =
  case parseAndResolveSchema str of
    Left str -> Left str
    Right (ss, r) -> Right $
      ProcessedSchema ss r (computeIds toList (schemasResolved r) versions)

processSchemaCached
  :: Map SchemaId Version
  -> SchemaParserCache
  -> ByteString
  -> Either String (SchemaParserCache, ProcessedSchema)
processSchemaCached versions cache str =
  case parseAndResolveSchemaCached cache str of
    Left str -> Left str
    Right (ss, r, newcache) ->
      Right (
        newcache,
        ProcessedSchema ss r
          (computeIds HashMap.toList (schemasResolved r) versions)
      )

-- | Read the schema definition from the ConfigProvider
legacySchemaSourceConfig :: ThriftSource SchemaIndex
legacySchemaSourceConfig =
  ThriftSource.configWithDeserializer legacySchemaConfigPath
     (processOneSchema Map.empty)

-- | Read the default schema index from the ConfigProvider
defaultSchemaSourceIndexConfig :: ThriftSource SchemaIndex
defaultSchemaSourceIndexConfig = schemaSourceIndexConfig schemaConfigPath

-- | Read the schema files from the source tree
schemaSourceFiles :: ThriftSource SchemaIndex
schemaSourceFiles = schemaSourceFilesFromDir schemaSourceDir

-- | Read the schema from a single file
schemaSourceFile :: FilePath -> ThriftSource SchemaIndex
schemaSourceFile f = ThriftSource.fileWithDeserializer f
  (processOneSchema Map.empty)

-- | Read a schema index from a file
schemaSourceIndexFile :: FilePath -> ThriftSource SchemaIndex
schemaSourceIndexFile = ThriftSource.once . parseSchemaIndex

-- | Read schema files from the given directory
schemaSourceFilesFromDir :: FilePath -> ThriftSource SchemaIndex
schemaSourceFilesFromDir = ThriftSource.once . parseSchemaDir

-- | Read a schema index from the ConfigProvider. This will watch for
-- changes to the index but not any of the instance files, since those
-- are expected to be immutable.
schemaSourceIndexConfig :: Text -> ThriftSource SchemaIndex
schemaSourceIndexConfig key = ThriftSource.genericConfig
  key
  deserializeJSON
  loadInstances
  Nothing
  where
    loadInstances
      :: ConfigProvider cfg
      => cfg
      -> Internal.SchemaIndex
      -> IO SchemaIndex
    loadInstances cfg Internal.SchemaIndex{..} = do
      let proc Internal.SchemaInstance{..} = do
            let
              versions = Map.mapKeys SchemaId schemaInstance_versions
              instanceKey = Text.pack $
                takeDirectory (Text.unpack key) </>
                Text.unpack schemaInstance_file
            str <- lift $ Config.get cfg instanceKey Right
            cache <- State.get
            case processSchemaCached versions cache str of
              Left err -> lift $ throwIO $ Exception $
                "error in schema: " <> Text.pack err <>
                "\nFile: " <> instanceKey
              Right (newcache, result) -> do
                State.put newcache
                return result
      flip evalStateT HashMap.empty $ do
        current <- proc schemaIndex_current
        older <- mapM proc schemaIndex_older
        return (SchemaIndex current (reverse older))

-- | Read schema files from a directory
parseSchemaDir :: FilePath -> IO SchemaIndex
parseSchemaDir dir = do
  str <- catSchemaFiles =<< listDirectoryRecursive dir
  case processOneSchema Map.empty str of
    Left err -> throwIO $ ErrorCall err
    Right schema -> return schema

parseSchemaIndex :: FilePath -> IO SchemaIndex
parseSchemaIndex file = do
  Internal.SchemaIndex{..} <- loadJSON file
  let proc Internal.SchemaInstance{..} = do
        let dir = takeDirectory file
        str <- B.readFile (dir </> Text.unpack schemaInstance_file)
        either (throwIO . ErrorCall) return $
          processSchema (Map.mapKeys SchemaId schemaInstance_versions) str
  current <- proc schemaIndex_current
  older <- mapM proc schemaIndex_older
  return (SchemaIndex current older)

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
  -> Either String (Maybe FilePath, ThriftSource SchemaIndex)
schemaSourceParser "config" = Right (Nothing, legacySchemaSourceConfig)
schemaSourceParser "indexconfig" =
  Right (Nothing, defaultSchemaSourceIndexConfig)
schemaSourceParser "dir" =
  Right (Just schemaSourceDir, schemaSourceFilesFromDir schemaSourceDir)
schemaSourceParser s = case break (==':') s of
  ("dir", ':':path) ->
    Right (Just path, ThriftSource.once $ parseSchemaDir path)
  ("index", ':':path) ->
    Right (Nothing, ThriftSource.once $ parseSchemaIndex path)
  ("indexconfig", ':':path) ->
    Right (Nothing, schemaSourceIndexConfig (Text.pack path))
  -- default to interpreting the argument as a directory:
  (_, "") ->
    Right (Just s, ThriftSource.once $ parseSchemaDir s)
  _otherwise -> -- handles config:PATH and file:PATH
    (Nothing,) <$> ThriftSource.parseWithDeserializer s
      (processOneSchema Map.empty)

schemaSourceOption
  :: Parser (Maybe FilePath, ThriftSource SchemaIndex)
schemaSourceOption = option (eitherReader schemaSourceParser)
  (  long "schema"
  <> metavar "(dir | config | indexconfig | file:FILE | dir:DIR | config:PATH | index:FILE | indexconfig:PATH | DIR)"
  <> value (Nothing, defaultSchemaSourceIndexConfig))

options :: Parser Config
options = do
  let
    dbRoot = fileDataStore <$> strOption (
      long "db-root" <>
      metavar "DIR" <>
      help "Directory containing databases")
    dbTmp = tmpDataStore <$ flag' () (
      long "db-tmp" <>
      help "Store databases in a temporary directory (default)")
    dbMem = memoryDataStore <$ flag' () (
      long "db-memory" <>
      help "Store databases in memory")
  cfgDataStore <- dbRoot <|> dbTmp <|> dbMem <|> pure tmpDataStore
  ~(cfgSchemaDir, cfgSchemaSource) <- schemaSourceOption
  _ignored_for_backwards_compat <- switch (long "db-schema-override")
  _cfgSchemaId <- fmap (fmap SchemaId) $ optional $ strOption
    ( long "schema-id" <> hidden ) -- ignored for backwards compat

  cfgRecipeConfig <- recipesConfigThriftSource
  cfgServerConfig <-
    serverConfigThriftSource <|>
    serverConfigTier <|>
    pure def  -- default settings if no option given
  cfgReadOnly <- switch (long "db-read-only")
  cfgMockWrites <- switch (long "db-mock-writes")
  cfgEnableRecursion <- switch
    ( long "experimental-recursion"
    <> help "Experimental support for recursive predicates. For testing only"
    <> internal
    )
  cfgDebug <- debugParser
  return Config
    { cfgListener = mempty
    , cfgUpdateSchema = True
    , cfgShardManager = cfgShardManager def
    , cfgServerLogger = cfgServerLogger def
    , cfgDatabaseLogger = cfgDatabaseLogger def
    , cfgBackupBackends = cfgBackupBackends def
    , cfgFilterAvailableDBs = return
    , cfgTracer = mempty
    , cfgSchemaId = Nothing
    , .. }
  where
    debugParser :: Parser DebugFlags
    debugParser = do
      tcDebug <- switch (long "debug-tc")
      queryDebug <- switch (long "debug-query")
      return DebugFlags{..}

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

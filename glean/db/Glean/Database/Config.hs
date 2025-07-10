{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, CPP #-}
module Glean.Database.Config (
  -- * DataStore
  DataStore(..),
  fileDataStore,
  tmpDataStore,
  memoryDataStore,

  -- * Config, and options parser
  options,
  Config(..),
  DebugFlags(..),

  -- * Finding and parsing the schema
  ServerConfig.SchemaLocation(..),
  showSchemaLocation,
  schemaLocation,
  schemaLocationOption,
  processSchema,
  processSchemaCached,
  processOneSchema,
  SchemaIndex(..),
  schemaForSchemaId,
  ProcessedSchema(..),
  catSchemaFiles,
  schemaLocationToSource,
  parseSchemaDir,
  parseSchemaIndex,
  loadSchemaIndex,

  -- * Testing only

  -- if you're using these somewhere other than a test,
  -- you should probably be using something from the API above instead.
  processSchemaForTesting,
  schemaLocationFiles,
  schemaSourceDir,
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
import Data.Maybe
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
import qualified Glean.Database.BatchLocation as BatchLocation
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

#ifdef OSS
import Paths_glean
#endif

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
  , cfgSchemaLocation :: Maybe ServerConfig.SchemaLocation
  , cfgSchemaHook :: ServerConfig.SchemaLocation -> (ThriftSource SchemaIndex, Bool)
      -- ^ Allows the client to provide the ThriftSource for the schema. This is
      -- used by the shell to update the schema in response to a user command.
      -- The Bool is True if the schema for open DBs should be updated
      -- whenever the global schema changes.
  , cfgSchemaId :: Maybe SchemaId
      -- ^ If set, this is the version of the schema that is used to
      -- interpret a query.
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
  , cfgBatchLocationParser :: Some BatchLocation.Parser
    -- ^ Batch's location parser
  , cfgEnableRecursion :: Bool
    -- ^ Enable experimental support for recursion
  , cfgFilterAvailableDBs :: [Repo] -> IO [Repo]
    -- ^ Filter out DBs not currently available on some other server
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
    , cfgSchemaLocation = Nothing
    , cfgSchemaHook = \l -> (schemaLocationToSource l, True)
    , cfgSchemaId = Nothing
    , cfgServerConfig = def
    , cfgReadOnly = False
    , cfgMockWrites = False
    , cfgListener = mempty
    , cfgShardManager = \_ _ k -> k $ SomeShardManager noSharding
    , cfgServerLogger = Some NullGleanServerLogger
    , cfgDatabaseLogger = Some NullGleanDatabaseLogger
    , cfgBackupBackends = HashMap.fromList [("mock", Backup.Mock.mock)]
    , cfgBatchLocationParser = Some BatchLocation.DefaultParser
    , cfgEnableRecursion = False
    , cfgFilterAvailableDBs = const $ return []
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
    containsId id = (== id) . hashedSchemaId . procSchemaHashed

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
  :: Maybe (SchemaId, Version)
  -> ByteString
  -> Either String SchemaIndex
processOneSchema versions str =
  case processSchema versions str of
    Left str -> Left str
    Right schema -> Right (SchemaIndex schema [])

processSchema
  :: Maybe (SchemaId, Version)
  -> ByteString
  -> Either String ProcessedSchema
processSchema = processSchemaForTesting HashMap.toList

-- | Testing version of 'processSchema' that accepts a custom @HashMap.toList@
--   function to weed out ordering assumptions
processSchemaForTesting
  :: (forall k v . HashMap k v -> [(k,v)])
  -> Maybe (SchemaId, Version)
  -> ByteString
  -> Either String ProcessedSchema
processSchemaForTesting toList versions str =
  case parseAndResolveSchema str of
    Left str -> Left str
    Right (ss, r) -> Right $
      ProcessedSchema ss r (computeIds toList (schemasResolved r) versions)

processSchemaCached
  :: Maybe (SchemaId, Version)
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

-- | Read the schema files from the source tree
schemaLocationFiles :: ServerConfig.SchemaLocation
schemaLocationFiles = ServerConfig.SchemaLocation_dir (Text.pack schemaSourceDir)

-- | Read the schema definition from the ConfigProvider
schemaSourceConfig :: Text -> ThriftSource SchemaIndex
schemaSourceConfig loc =
  ThriftSource.configWithDeserializer loc
     (processOneSchema Nothing)

-- | Read the schema from a single file
schemaSourceFile :: FilePath -> ThriftSource SchemaIndex
schemaSourceFile f = ThriftSource.fileWithDeserializer f
  (processOneSchema Nothing)

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
              instanceKey = Text.pack $
                takeDirectory (Text.unpack key) </>
                Text.unpack schemaInstance_file
            maybeVersion <- lift $ checkVersions schemaInstance_versions
            str <- lift $ Config.get cfg instanceKey Right
            cache <- State.get
            case processSchemaCached maybeVersion cache str of
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

checkVersions :: Map Text Version -> IO (Maybe (SchemaId, Version))
checkVersions versions =
  case Map.toList versions of
    [] -> return Nothing
    [(txtId, ver)] -> return (Just (SchemaId txtId, ver))
    _multiple -> throwIO $ Exception "versions must contain a single entry"

-- | Read schema files from a directory
parseSchemaDir :: FilePath -> IO SchemaIndex
parseSchemaDir dir = do
  str <- catSchemaFiles =<< listDirectoryRecursive dir
  case processOneSchema Nothing str of
    Left err -> throwIO $ ErrorCall err
    Right schema -> return schema

parseSchemaIndex :: FilePath -> IO SchemaIndex
parseSchemaIndex file = do
  Internal.SchemaIndex{..} <- loadJSON file
  let proc Internal.SchemaInstance{..} = do
        let dir = takeDirectory file
        str <- B.readFile (dir </> Text.unpack schemaInstance_file)
        maybeVersion <- checkVersions schemaInstance_versions
        either (throwIO . ErrorCall) return $ processSchema maybeVersion str
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

schemaLocationToSource
  :: ServerConfig.SchemaLocation
  -> ThriftSource SchemaIndex
schemaLocationToSource = \case
  ServerConfig.SchemaLocation_dir d -> schemaSourceFilesFromDir (Text.unpack d)
  ServerConfig.SchemaLocation_file f -> schemaSourceFile (Text.unpack f)
  ServerConfig.SchemaLocation_index i -> schemaSourceIndexFile (Text.unpack i)
  ServerConfig.SchemaLocation_config c -> schemaSourceConfig c
  ServerConfig.SchemaLocation_indexconfig c -> schemaSourceIndexConfig c
  ServerConfig.SchemaLocation_EMPTY{} -> error "schemaLocationToSource"

-- | The logic for obtaining the SchemaLocation for the schema, given
-- the Config and ServerConfig. The location is given by, in order of preference:
--   - the Config (--schema flag)
--   - the ServerConfig (schema_location field)
--   - the default schema location: Glean.DefaultConfigs.defaultSchemaLocation
--
-- Also here we replace "$datadir" with the datadir supplied by Cabal, so that
-- we can install the schema in $datadir when Glean is installed.
schemaLocation :: Config -> ServerConfig.Config -> IO ServerConfig.SchemaLocation
schemaLocation cfg server_cfg = do
#ifdef OSS
  datadir <- Text.replace "$datadir" . Text.pack <$> getDataDir
#else
  datadir <- return id
#endif
  let loc = fromMaybe defaultSchemaLocation $
        cfgSchemaLocation cfg <|>
        ServerConfig.config_schema_location server_cfg
  case loc of
    ServerConfig.SchemaLocation_dir d ->
      return (ServerConfig.SchemaLocation_dir (datadir d))
    ServerConfig.SchemaLocation_file f ->
      return (ServerConfig.SchemaLocation_file (datadir f))
    ServerConfig.SchemaLocation_index i ->
      return (ServerConfig.SchemaLocation_index (datadir i))
    other ->
      return other

-- | Find and load the SchemaIndex, taking into account command line
-- flags, the ServerConfig, and the cfgSchemaHook. This is a
-- convenience function used by the CLI.
loadSchemaIndex :: ConfigProvider c => Config -> c -> IO SchemaIndex
loadSchemaIndex cfg cfgAPI = do
  serverConfig <- ThriftSource.load cfgAPI (cfgServerConfig cfg)
  loc <- schemaLocation cfg serverConfig
  let (schemaSource, _) = cfgSchemaHook cfg loc
  ThriftSource.load cfgAPI schemaSource

showSchemaLocation :: ServerConfig.SchemaLocation -> String
showSchemaLocation = \case
  ServerConfig.SchemaLocation_dir d -> "dir:" <> Text.unpack d
  ServerConfig.SchemaLocation_file d -> "file:" <> Text.unpack d
  ServerConfig.SchemaLocation_index d -> "index:" <> Text.unpack d
  ServerConfig.SchemaLocation_config d -> "config:" <> Text.unpack d
  ServerConfig.SchemaLocation_indexconfig d -> "indexconfig:" <> Text.unpack d
  _ -> error "showSchemaLocation"

-- | Allow short \"indexconfig\" and \"dir\"  to choose the defaults from
-- 'schemaSourceConfig' and 'schemaSourceDir' as well as full explicit
-- \"config:PATH\", \"dir:PATH\", and \"file:PATH\" sources.
schemaLocationParser
  :: String
  -> Either String ServerConfig.SchemaLocation
schemaLocationParser "indexconfig" =
  Right (ServerConfig.SchemaLocation_index schemaConfigPath)
schemaLocationParser "dir" =
  Right (ServerConfig.SchemaLocation_dir (Text.pack schemaSourceDir))
schemaLocationParser s = case break (==':') s of
  ("dir", ':':path) ->
    Right (ServerConfig.SchemaLocation_dir (Text.pack path))
  ("index", ':':path) ->
    Right (ServerConfig.SchemaLocation_index (Text.pack path))
  ("indexconfig", ':':path) ->
    Right (ServerConfig.SchemaLocation_indexconfig (Text.pack path))
  -- default to interpreting the argument as a directory:
  ("file", ':':path) ->
    Right (ServerConfig.SchemaLocation_file (Text.pack path))
  ("config", ':':path) ->
    Right (ServerConfig.SchemaLocation_config (Text.pack path))
  (_, "") ->
    Right (ServerConfig.SchemaLocation_dir (Text.pack s))
  _otherwise -> -- handles config:PATH and file:PATH
    Left "invalid schema location"

schemaLocationOption
  :: Parser ServerConfig.SchemaLocation
schemaLocationOption = option (eitherReader schemaLocationParser)
  (  long "schema"
  <> metavar "(dir | config | indexconfig | file:FILE | dir:DIR | config:PATH | index:FILE | indexconfig:PATH | DIR)"
  )

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
  cfgSchemaLocation <- optional schemaLocationOption
  _ignored_for_backwards_compat <- switch (long "db-schema-override")

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
    , cfgSchemaHook = cfgSchemaHook def
    , cfgShardManager = cfgShardManager def
    , cfgServerLogger = cfgServerLogger def
    , cfgDatabaseLogger = cfgDatabaseLogger def
    , cfgBackupBackends = cfgBackupBackends def
    , cfgBatchLocationParser = cfgBatchLocationParser def
    , cfgFilterAvailableDBs = const $ return []
    , cfgTracer = mempty
    , cfgSchemaId = Nothing
    , .. }
  where
    debugParser :: Parser DebugFlags
    debugParser = do
      tcDebug <- switch (long "debug-tc")
      queryDebug <- switch (long "debug-query")
      return DebugFlags{..}

    serverConfigThriftSource = option (eitherReader ThriftSource.parse)
      (  long "server-config"
      <> metavar "(file:PATH | config:PATH)" )

    serverConfigTier =
      ThriftSource.config . Text.pack . (serverConfigPath </>) <$>
      strOption
        (  long "tier"
        <> metavar "TIER"
        <> help "specifies the server configuration to load")

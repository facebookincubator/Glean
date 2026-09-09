{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, OverloadedRecordDot #-}
module GleanCLI.Create (
  CreateOpts(..),
  parseCreateOpts,
  createDb,
) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Encode
import Data.Time.Clock (UTCTime)
import Options.Applicative

import Glean
import qualified Glean.Remote
import Glean.Database.Meta
  (getACLMode, showACLMode, utcTimeToPosixEpochTime)
import Glean.Types as Thrift
import Util.IO (die)
import Util.Time


data CreateOpts = CreateOpts
  { dependencies :: Maybe DependencyOpts
  , writeRepoTime :: Maybe UTCTime
  , properties :: [(Text,Text)]
  , updateSchemaForStacked :: Bool
  , aclMode :: Maybe ACLMode
  , aclConfigPath :: Maybe FilePath
  }

-- | ACL mode for the database
data ACLMode
  = ACLActive      -- ^ Basic ACL support (glean.acl = "enabled")
  | ACLEnforced    -- ^ Strict mode (error on facts without units)
  | ACLPermissive  -- ^ Permissive mode (no directory inheritance)
  deriving (Eq, Show)

aclModeToProperty :: ACLMode -> (Text, Text)
aclModeToProperty ACLActive = ("glean.acl", "enabled")
aclModeToProperty ACLEnforced = ("glean.acl", "enforced")
aclModeToProperty ACLPermissive = ("glean.acl", "permissive")

parseCreateOpts :: Parser CreateOpts
parseCreateOpts = do
  writeRepoTime <- optional repoTimeOpt
  dependencies <- optional dependencyOpts
  properties <- dbPropertiesOpt
  updateSchemaForStacked <- updateSchemaForStackedOpt
  aclMode <- optional aclModeOpt
  aclConfigPath <- optional aclConfigOpt
  return CreateOpts {
    writeRepoTime,
    dependencies,
    properties,
    updateSchemaForStacked,
    aclMode,
    aclConfigPath
  }

aclConfigOpt :: Parser FilePath
aclConfigOpt = strOption
  (  long "acl-config"
  <> metavar "FILE"
  <> help ("Path to the ACL config JSON file (directory path -> list of ACL "
  <> "group IDs). Required with --acl; pass an empty object for an "
  <> "all-public DB.")
  )

aclModeOpt :: Parser ACLMode
aclModeOpt = option readACLMode
  (  long "acl"
  <> metavar "MODE"
  <> help "ACL mode for the database: enabled, enforced, or permissive"
  )
  where
    readACLMode :: ReadM ACLMode
    readACLMode = eitherReader $ \str ->
      case str of
        "enabled" -> Right ACLActive
        "enforced" -> Right ACLEnforced
        "permissive" -> Right ACLPermissive
        _ -> Left "--acl: expecting enabled, enforced, or permissive"

repoTimeOpt :: Parser UTCTime
repoTimeOpt = option readTime
  (  long "repo-hash-time"
  <> metavar "yyyy-mm-ddThh:mm:ssZ"
  <> help "Timestamp of the source data to be indexed."
  )
  where
    readTime :: ReadM UTCTime
    readTime = eitherReader $ \str ->
      case readUTC $ Text.pack str of
        Just value -> Right value
        Nothing ->
          Left "expecting UTC time e.g. 2021-01-01T12:30:00Z"

data DependencyOpts = DependencyOpts Repo (Maybe PruneOpts)

data PruneOpts = PruneOpts ExcludeOpt UnitsOpt

data ExcludeOpt = Exclude | Include
  deriving Eq

data UnitsOpt
  = Units [ByteString]
  | UnitsFromFile FilePath

readUnits :: UnitsOpt -> IO [ByteString]
readUnits (Units these) = return these
readUnits (UnitsFromFile file) = extractLines file

getDependencies :: DependencyOpts -> IO Dependencies
getDependencies (DependencyOpts Repo{..} Nothing) =
  return $ Thrift.Dependencies_stacked $
    Thrift.Stacked repo_name repo_hash Nothing
getDependencies (DependencyOpts repo (Just (PruneOpts exclude unitsOpt))) = do
  units <- readUnits unitsOpt
  return $ Thrift.Dependencies_pruned $
    Thrift.Pruned repo units (exclude == Exclude) Nothing

dependencyOpts :: Parser DependencyOpts
dependencyOpts = stackedOpt <|> updateOptions

updateOptions :: Parser DependencyOpts
updateOptions = do
  repo <- incrementalOpt
  prune <- includeOpt <|> excludeOpt
  return $ DependencyOpts repo (Just prune)

stackedOpt :: Parser DependencyOpts
stackedOpt =
  (\repo -> DependencyOpts repo Nothing) <$>
     option (maybeReader Glean.parseRepo)
  (  long "stacked"
  <> metavar "DB"
  <> help ("Created DB will be stacked on top of this DB. "
  <> "For more details about its schema, see --update-schema-for-stacked.")
  )

incrementalOpt :: Parser Repo
incrementalOpt = option (maybeReader Glean.parseRepo)
  (  long "incremental"
  <> metavar "DB"
  <> help "Create an incremental DB on top of this DB."
  )

splitUnits :: Text -> [ByteString]
splitUnits = map Encode.encodeUtf8 . Text.splitOn ","

extractLines :: FilePath -> IO [ByteString]
extractLines file = map Encode.encodeUtf8 . Text.lines <$> Text.readFile file

includeOptString :: Parser UnitsOpt
includeOptString = Units . splitUnits <$> strOption
  (  long "include"
  <> metavar "unit,unit,.."
  <> help "For incremental DBs only. Include these units."
  )

includeOptFile :: Parser UnitsOpt
includeOptFile = UnitsFromFile <$> strOption
  (  long "include-file"
  <> metavar "FILE"
  <> help ("For incremental DBs only. Include units in FILE "
  <> "(one per line).")
  )

includeOpt :: Parser PruneOpts
includeOpt = PruneOpts Include <$> (includeOptFile <|> includeOptString)

excludeOptString :: Parser UnitsOpt
excludeOptString =  Units . splitUnits <$> strOption
  (  long "exclude"
  <> metavar "unit,unit,.."
  <> help "For incremental DBs only. Exclude these units."
  )

excludeOptFile :: Parser UnitsOpt
excludeOptFile =  UnitsFromFile <$> strOption
  (  long "exclude-file"
  <> metavar "FILE"
  <> help ("For incremental DBs only. Exclude units in FILE "
  <> "(one per line).")
  )

excludeOpt :: Parser PruneOpts
excludeOpt = PruneOpts Exclude <$> (excludeOptFile <|> excludeOptString)

dbPropertiesOpt :: Parser [(Text, Text)]
dbPropertiesOpt = many $ option readProperty
  (  long "property"
  <> metavar "NAME=VALUE"
  <> help "Set DB's properties when creating a DB."
  )
  where
    readProperty :: ReadM (Text,Text)
    readProperty = eitherReader $ \str ->
      case break (=='=') str of
        (name, '=':value) -> Right (Text.pack name, Text.pack value)
        _other -> Left "--property: expecting NAME=VALUE"

updateSchemaForStackedOpt :: Parser Bool
updateSchemaForStackedOpt = switch
  (  long "update-schema-for-stacked"
  <> help (
    "When creating a stacked DB, use the current schema instead " <>
    "of the schema from the base DB.")
  )

createDb :: Backend b => b -> Repo -> CreateOpts -> IO Bool
createDb backend repo opts = do
  deps <- mapM getDependencies opts.dependencies
  let aclProperty = maybe [] (pure . aclModeToProperty) opts.aclMode
      allProperties = HashMap.fromList (opts.properties ++ aclProperty)
  aclConfig <- traverse loadACLConfig opts.aclConfigPath
  putStrLn $ "[glean create] " ++ showACLMode (getACLMode allProperties)
  -- Retry transient channel exceptions so a write-server restart doesn't fail
  -- the kick-off.
  let retryBackend =
        Glean.Remote.backendRetryWrites backend Glean.Remote.defaultRetryPolicy
  Thrift.KickOffResponse alreadyExists _ _ <-
    Glean.kickOffDatabase retryBackend def
      { kickOff_repo = repo
      , kickOff_properties = allProperties
      , kickOff_dependencies = deps
      , kickOff_repo_hash_time =
          utcTimeToPosixEpochTime <$> opts.writeRepoTime
      , kickOff_update_schema_for_stacked = opts.updateSchemaForStacked
      , kickOff_acl_config = aclConfig
      }
  return alreadyExists

-- | Load the ACL config (path -> list of ACL group IDs) from a JSON file.
-- Dies on parse failure.
loadACLConfig :: FilePath -> IO (HashMap.HashMap Text [Text])
loadACLConfig configPath = do
  -- eitherDecodeFileStrict' reports decode failures in its result but lets
  -- read failures through as exceptions, so catch those too.
  parsed <- try (Aeson.eitherDecodeFileStrict' configPath)
  case parsed of
    Right (Right config) -> return config
    Right (Left err) ->
      die 3 $ "Failed to parse ACL config from " ++ configPath ++ ": " ++ err
    Left (err :: IOException) ->
      die 3 $ "Failed to read ACL config from " ++ configPath ++ ": "
        ++ show err

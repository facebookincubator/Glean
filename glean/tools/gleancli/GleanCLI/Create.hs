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
import Glean.Database.Meta (utcTimeToPosixEpochTime)
import Glean.Types as Thrift
import Util.Time


data CreateOpts = CreateOpts
  { dependencies :: Maybe DependencyOpts
  , writeRepoTime :: Maybe UTCTime
  , properties :: [(Text,Text)]
  , updateSchemaForStacked :: Bool
  }

parseCreateOpts :: Parser CreateOpts
parseCreateOpts = do
  writeRepoTime <- optional repoTimeOpt
  dependencies <- optional dependencyOpts
  properties <- dbPropertiesOpt
  updateSchemaForStacked <- updateSchemaForStackedOpt
  return CreateOpts {
    writeRepoTime,
    dependencies,
    properties,
    updateSchemaForStacked
  }

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
  Thrift.KickOffResponse alreadyExists <-
    Glean.kickOffDatabase backend def
      { kickOff_repo = repo
      , kickOff_properties = HashMap.fromList opts.properties
      , kickOff_dependencies = deps
      , kickOff_repo_hash_time =
          utcTimeToPosixEpochTime <$> opts.writeRepoTime
      , kickOff_update_schema_for_stacked = opts.updateSchemaForStacked
      }
  return alreadyExists

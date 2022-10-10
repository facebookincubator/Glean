{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Database.Create (
  kickOffDatabase,
  updateProperties,
) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception hiding(handle)
import Control.Monad.Catch (handle)
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow

#ifdef FACEBOOK
import Facebook.Process
#endif
import Util.Defer
import Util.Log

import Glean.Backend.Remote (StackedDbOpts(..))
import Glean.BuildInfo
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Open
import Glean.Database.PredicateStats
import Glean.Database.Types
import Glean.Database.Work
import Glean.Database.Schema (
  toStoredSchema, compareSchemaPredicates, renderSchemaSource, toStoredVersions)
import Glean.Database.Schema.ComputeIds
import Glean.Database.Schema.Types
import Glean.Internal.Types
import qualified Glean.Recipes.Types as Recipes
import Glean.RTS.Foreign.Lookup (firstFreeId)
import Glean.Schema.Types (schemasHighestVersion)
import Glean.RTS.Types (lowestFid, fromPid)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed

-- | Kick off a specifc database, scheduling its tasks as necessary.
kickOffDatabase :: Env -> Thrift.KickOff -> IO Thrift.KickOffResponse
kickOffDatabase env@Env{..} Thrift.KickOff{..}
  | envReadOnly = dbError kickOff_repo "can't create database in read only mode"
  | otherwise = do
      ServerConfig.Config{..} <- Observed.get envServerConfig
      let
        -- If use_schema_id is enabled in the server config and the
        -- glean.schema_id property is set, we'll use this to decide
        -- which schema instance to store in the DB.
        schemaToUse
          | not config_use_schema_id = Storage.UseDefaultSchema
          | otherwise =
            case HashMap.lookup "glean.schema_id" kickOff_properties of
              Just id -> Storage.UseSpecificSchema (SchemaId id)
              Nothing -> Storage.UseDefaultSchema

        stackedCreate repo =
          readDatabase env repo $ \odb lookup -> do
            atomically $ do
              meta <- Catalog.readMeta envCatalog repo
              case metaCompleteness meta of
                Complete{} -> return ()
                c -> throwSTM $ InvalidDependency kickOff_repo repo $
                  "database is " <> showCompleteness c
            start <- firstFreeId lookup

            let storedSchema = toStoredSchema (odbSchema odb)

            if not kickOff_update_schema_for_stacked
              then return $ Storage.Create start
                (Storage.UseThisSchema storedSchema)
              else do

            stats <- predicateStats env repo IncludeBase

            -- If update_schema_for_stacked is enabled, then we need
            -- to check that the specified schema agrees with the
            -- stored schema in the base DB about the definitions of
            -- predicates and types. We can do a fast check using the
            -- hashes, and throw an exception if there are any
            -- differences.
            index <- Observed.get envSchemaSource
            let
              DbSchema{..} = odbSchema odb

              proc = case schemaToUse of
                Storage.UseSpecificSchema id
                  | Just proc <- schemaForSchemaId index id -> proc
                _otherwise -> schemaIndexCurrent index

              hasFacts pred = case HashMap.lookup pred predicatesById of
                Just PredicateDetails{..}
                  | Just stat <- Map.lookup (fromPid predicatePid) stats ->
                    predicateStats_count stat > 0
                _otherwise -> False

              HashedSchema{..} = procSchemaHashed proc
              errors = compareSchemaPredicates
                (filter hasFacts (HashMap.keys predicatesById))
                (HashMap.keys hashedPreds)

            chooseSchema <-
              if null errors then
                return $ Storage.UseThisSchema
                  (StoredSchema
                    (renderSchemaSource (procSchemaSource proc))
                    (storedSchema_predicateIds storedSchema)
                    -- Note: we *must* use the Pids from the base DB
                    (toStoredVersions hashedSchemaAllVersions))
              else
                throwIO $ Thrift.Exception $
                  "update_schema_for_stacked specified, but schemas are " <>
                  "incompatible: " <> Text.intercalate ", " errors

            return $ Storage.Create start chooseSchema

      mode <- case kickOff_dependencies of
        Nothing -> return $ Storage.Create lowestFid schemaToUse
        Just (Dependencies_stacked repo) -> stackedCreate repo
        Just (Dependencies_pruned update) -> stackedCreate (pruned_base update)

      -- NOTE: We don't want to load recipes (which might fail) if we don't
      -- need them.
      state <- completenessFromFill get_recipes kickOff_repo kickOff_fill
      creationTime <- envGetCurrentTime
      serverProps <- serverProperties
      fbServerProps <- facebookServerProperties
      schemaProps <- schemaProperties
      let
        allProps = mconcat
          [ kickOff_properties
          , serverProps
          , fbServerProps
          , schemaProps
          , scribeProperties kickOff_fill
          ]
        time = DBTimestamp
          { timestampCreated = creationTime
          , timestampRepoHash =
              posixEpochTimeToUTCTime <$> kickOff_repo_hash_time
          }
      version <-
        fromMaybe Storage.currentVersion . ServerConfig.config_db_create_version
        <$> Observed.get envServerConfig
      when (not $ Storage.canOpenVersion Storage.ReadWrite version) $
        dbError kickOff_repo
          "can't create databases (unsupported binary version)"
      db <- atomically $ newDB kickOff_repo
      handle
        (\Catalog.EntryAlreadyExists{} ->
            return $ Thrift.KickOffResponse True) $
        mask $ \unmask ->
        -- FIXME: There is a tiny race here where we might fail in a weird way
        -- if kick off a DB that is being deleted after it got removed from
        -- the Catalog but before it got removed from the storage. The entire
        -- concept of deleting DBs will change with the new metadata handling so
        -- it's not worth fixing at this point, especially since we aren't
        -- supposed to be kicking off DBs we've previously deleted.
        bracket_
          (Catalog.create
            envCatalog
            kickOff_repo
            (newMeta version time state allProps kickOff_dependencies) $ do
              modifyTVar' envActive $ HashMap.insert kickOff_repo db
              writeTVar (dbState db) Opening
              acquireDB db)
          (atomically $ releaseDB env db) $
          do
            -- Open the new db in Create mode which will create the physical
            -- storage. This might fail - in that case, we mark the db as failed
            opener <- asyncOpenDB env db version mode kickOff_dependencies
              (do
                -- On success, schedule the db's tasks. If this throws,
                -- 'asyncOpenDB' will close the db and call our failure action
                -- below.
                immediately $ do
                  meta <- lift $ Catalog.readMeta envCatalog kickOff_repo
                  new_meta <- handle (failTask meta) $ scheduleTasks
                    env
                    kickOff_repo
                    meta
                  lift $ Catalog.writeMeta envCatalog kickOff_repo new_meta
                logInfo $ inRepo kickOff_repo "created")
              (\exc -> atomically $ void $
                  -- If opening the db fails for any reason, mark the db as
                  -- failed.
                  Catalog.modifyMeta envCatalog kickOff_repo $ \meta ->
                    return meta
                      { metaCompleteness = Broken DatabaseBroken
                        { databaseBroken_task = ""
                        , databaseBroken_reason =
                            "couldn't create: " <> Text.pack (show exc)
                        }
                      })
            OpenDB{..} <- unmask $ Async.wait opener
            addSchemaIdProperty envCatalog kickOff_repo
              (schemaLatestVersion odbSchema)
            return $ Thrift.KickOffResponse False
  where
    addSchemaIdProperty :: Catalog.Catalog -> Repo -> SchemaId -> IO ()
    addSchemaIdProperty catalog repo hash =
      void $ atomically $ Catalog.modifyMeta catalog repo $ \meta ->
        return meta { metaProperties =
          HashMap.insertWith
            (\_ old -> old)  -- if one was provided already, keep it
            "glean.schema_id"
            (unSchemaId hash)
            (metaProperties meta)
          }

    schemaProperties = do
      ProcessedSchema{..} <- schemaIndexCurrent <$> Observed.get envSchemaSource
      currentVersion <- case schemasHighestVersion procSchemaResolved of
        Just ver -> return ver
        Nothing -> dbError kickOff_repo "missing 'all' schema"
      let version = Text.pack $ show currentVersion
      return $ HashMap.fromList [ ("glean.schema_version", version) ]

    get_recipes name = do
      rcfg <- Recipes.config_recipes <$> Observed.get envRecipeConfig
      case Map.lookup name rcfg of
        Just recipes -> do
          when (Map.null recipes) $ dbError kickOff_repo
            "can't create a database with no recipes"
          return recipes
        Nothing -> dbError kickOff_repo $
          "unknown recipe set '" ++ Text.unpack name ++ "'"

serverProperties :: IO DatabaseProperties
serverProperties = return (HashMap.fromList rev)
  where
  rev
    | Text.null buildRevision = []
    | otherwise = [ ("glean.server.build_revision", buildRevision) ]

facebookServerProperties :: IO DatabaseProperties
facebookServerProperties = do
#if FACEBOOK
  twJob <- getTupperwareJob
  return $ HashMap.fromList
    (case twJob of
      Nothing -> []
      Just job -> [ ("glean.server.tw_job", job) ])
#else
  return HashMap.empty
#endif

scribeProperties :: Maybe Thrift.KickOffFill -> DatabaseProperties
scribeProperties (Just (KickOffFill_scribe WriteFromScribe{..})) =
  HashMap.fromList $
    [ ("glean.scribe.category", writeFromScribe_category) ] ++
    (case writeFromScribe_bucket of
      Just (PickScribeBucket_bucket n) ->
        [ ("glean.scribe.bucket", showt n) ]
      _otherwise -> []) ++
    (case writeFromScribe_start of
      Just (ScribeStart_start_time t) ->
        [ ("glean.scribe.start_time", showt t) ]
      _otherwise -> [])
scribeProperties _other = mempty

updateProperties
  :: Env
  -> Repo
  -> DatabaseProperties
  -> [Text]
  -> IO ()
updateProperties env repo set unset = do
  when (envReadOnly env) $ throwIO $ Thrift.Exception
    "updateProperties: server in read-only mode"
  atomically $ void $ Catalog.modifyMeta (envCatalog env) repo $ \meta ->
    case metaCompleteness meta of
      Incomplete{} -> return meta
        { metaProperties = HashMap.union set
            $ foldr HashMap.delete (metaProperties meta) unset
        }
      c -> throwSTM $ Thrift.Exception $
        "updateProperties: database is " <> showCompleteness c

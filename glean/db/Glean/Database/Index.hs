-- TODO: Refactor this and Glean.Database.Index into sensible bits
module Glean.Database.Index (
  kickOffDatabase, listDatabases,
  closeDatabase, closeDatabases,
  closeIdleDatabase, closeIdleDatabases, deleteDatabase, asyncDeleteDatabase,
  restoreDatabase,
  updateProperties,

  -- temporary exports for the janitor
  forRestoreSitesM, ifRestoreRepo, listRestorable, restoreDatabase_,
  expireDatabase
) where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception hiding(handle)
import Control.Monad.Catch (handle)
import Control.Monad.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import System.Directory
import TextShow

#ifdef FACEBOOK
import Facebook.Process
#endif
import ServiceData.GlobalStats as Stats
import ServiceData.Types as Stats
import Util.Control.Exception (catchAll, logExceptions)
import Util.Defer
import Util.IO (safeRemovePathForcibly)
import Util.Log

import Glean.BuildInfo
import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Locator as Backup
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Stuff
import Glean.Database.Types
import Glean.Database.Work
import Glean.Database.Writes
import qualified Glean.Recipes.Types as Recipes
import Glean.RTS.Foreign.Lookup (firstFreeId)
import Glean.Database.Schema (toSchemaInfo)
import Glean.RTS.Types (lowestFid)
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.Time
import qualified Glean.Util.Warden as Warden

-- | Kick off a specifc database, scheduling its tasks as necessary.
kickOffDatabase :: Env -> Thrift.KickOff -> IO Thrift.KickOffResponse
kickOffDatabase env@Env{..} Thrift.KickOff{..}
  | envReadOnly = dbError kickOff_repo "can't create database in read only mode"
  | otherwise = do
      let
        stackedCreate repo =
          readDatabase env repo $ \odb lookup -> do
            atomically $ do
              meta <- Catalog.readMeta envCatalog repo
              case metaCompleteness meta of
                Complete{} -> return ()
                Finalizing{} -> return ()
                c -> throwSTM $ InvalidDependency kickOff_repo repo $
                  "database is " <> showCompleteness c
            start <- firstFreeId lookup
            return $ Storage.Create start (Just $ toSchemaInfo (odbSchema odb))
      mode <- case kickOff_dependencies of
        Nothing -> return $ Storage.Create lowestFid Nothing
        Just (Dependencies_stacked repo) -> stackedCreate repo
        Just (Dependencies_pruned update) -> stackedCreate (pruned_base update)

      -- NOTE: We don't want to load recipes (which might fail) if we don't
      -- need them.
      state <- completenessFromFill get_recipes kickOff_repo kickOff_fill
      time <- envGetCreationTime
      serverProps <- serverProperties
      fbServerProps <- facebookServerProperties
      let
        allProps = mconcat
          [ kickOff_properties
          , serverProps
          , fbServerProps
          , scribeProperties kickOff_fill
          ]
      version <-
        fromMaybe Storage.currentVersion . ServerConfig.config_db_create_version
        <$> Observed.get envServerConfig
      when (not $ Storage.canOpenVersion Storage.ReadWrite version) $
        dbError kickOff_repo
          "can't create databases (unsupported binary version)"
      createDirectoryIfMissing True $ databasePath envRoot kickOff_repo
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
            unmask $ void $ Async.wait opener
            return $ Thrift.KickOffResponse False
  where
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

closeDatabases :: Env -> IO ()
closeDatabases env = do
  dbs <- readTVarIO $ envActive env
  -- Cancel the tailer threads *before* we close the DBs, otherwise a
  -- tailer thread may initiate a write which will open the DB again.
  -- This is temporary until we have a better way of doing this.
  forM_ dbs $ \db -> do
    tailers <- readTVarIO (dbTailers db)
    mapM_ (Async.cancel . tailerThread) tailers
  forM_ (HashMap.keys dbs) $ closeDatabase env

isIdle :: (TimePoint -> Bool) -> DB -> OpenDB -> STM Bool
isIdle long_enough db odb = and <$> sequence
  [ (== 1) <$> readTVar (dbUsers db)  -- we are the only user
  , long_enough <$> readTVar (odbIdleSince odb)
  , case odbWriting odb of
      Just Writing{..} -> do
        writeQueueSize <- readTVar (writeQueueSize wrQueue)
        return $ writeQueueSize == 0
      Nothing -> return True
  ]

closeIf :: (DB -> DBState -> STM (Maybe OpenDB)) -> Env -> Repo -> IO ()
closeIf should_close env repo = usingActiveDatabase env repo $ \r ->
  forM_ r $ \db -> mask_ $ do
    r <- atomically $ do
      state <- readTVar $ dbState db
      o <- should_close db state
      case o of
        Just odb -> do
          idle <- isIdle (const True) db odb
          when (not idle) retry
          writeTVar (dbState db) Closing
          return $ Just (db,odb)
        Nothing -> return Nothing

    forM_ r $ \(db, odb) ->
      -- the actual closing of the DB must be uninterruptible
      uninterruptibleMask_ (closeOpenDB env odb)
        `finally` atomically (writeTVar (dbState db) Closed)

closeDatabase :: Env -> Repo -> IO ()
closeDatabase env = closeIf
  (\_ state -> case state of
    Opening -> retry
    Open odb -> do
      deleteWriteQueues env odb
      return $ Just odb
    Closing -> retry
    Closed -> return Nothing)
  env

-- | Synchronously close a database if it has been idle for more than
-- `duration`.
closeIdleDatabase :: Env -> Repo -> DiffTimePoints -> IO ()
closeIdleDatabase env repo duration = do
  now <- getTimePoint
  closeIf
    (\db state -> case state of
        Opening -> return Nothing
        Open odb -> do
          idle <- isIdle
            (\last_use -> diffTimePoints last_use now >= duration)
            db
            odb
          return $ if idle then Just odb else Nothing
        Closing -> return Nothing
        Closed -> return Nothing)
    env
    repo

forRestoreSitesM
  :: Env
  -> a
  -> (forall site. Backup.Site site => Text -> site -> IO a)
  -> IO [a]
forRestoreSitesM env@Env{..} none inner = do
  ServerConfig.DatabaseRestorePolicy{..} <-
    ServerConfig.config_restore <$> Observed.get envServerConfig
  r <- atomically $ Backup.getAllSites env
  case r of
    sites@(_:_)
      | databaseRestorePolicy_enabled
        || not (Set.null databaseRestorePolicy_override) ->
      mapM (\(prefix, site, _) -> inner prefix site) sites
    _ -> return [none]

ifRestoreRepo
  :: Env
  -> a
  -> Repo
  -> (forall site. Backup.Site site => Text -> site -> IO a)
  -> IO a
ifRestoreRepo env@Env{..} none repo inner = do
  let repoName = Thrift.repo_name repo
  ServerConfig.DatabaseRestorePolicy{..} <-
    ServerConfig.config_restore <$> Observed.get envServerConfig
  r <- atomically $ Backup.getSite env repoName
  case r of
    Just (prefix, site, _)
      | (repoName `Set.member` databaseRestorePolicy_override)
          /= databaseRestorePolicy_enabled -> inner prefix site
    _ -> return none

listDatabases :: Env -> Thrift.ListDatabases -> IO Thrift.ListDatabasesResult
listDatabases env@Env{..} Thrift.ListDatabases{..} = do
  backups <-
    if listDatabases_includeBackups
      then do
        sites <- atomically $ Backup.getAllSites env
        restorables <- mapM
          (\(prefix, site, _) -> listRestorable prefix site) sites
        return $ reposToResults $ HashMap.unions restorables
      else
        return mempty
  local <- atomically $ Catalog.getLocalDatabases envCatalog
  return Thrift.ListDatabasesResult
    { listDatabasesResult_databases =
        map Thrift.getDatabaseResult_database
        $ HashMap.elems
        $ HashMap.union local backups
    }
  where
    reposToResults = HashMap.mapWithKey
      (\repo meta -> Thrift.GetDatabaseResult
        { getDatabaseResult_database = metaToThriftDatabase
            Thrift.DatabaseStatus_Restorable
            Nothing
            repo
            meta
        , getDatabaseResult_tasks = Nothing
        })

listRestorable :: Backup.Site site => Text -> site -> IO (HashMap Repo Meta)
listRestorable prefix site =
  (HashMap.fromList . mapMaybe restorable <$> Backup.enumerate site)
  `catchAll` \exc -> do
    logError $ "couldn't list restorable databases: " ++ show exc
    return mempty
  where
    restorable (repo, props)
      | Right meta <-
          metaFromProps (Backup.toRepoLocator prefix site repo) props =
            Just (repo, meta)
      | otherwise = Nothing

closeIdleDatabases :: Env -> DiffTimePoints -> [Repo] -> IO ()
closeIdleDatabases env duration blacklist = do
  dbs <- readTVarIO $ envActive env
  let notBlacklisted = filter (not . (`elem` blacklist)) (HashMap.keys dbs)
  forM_ notBlacklisted $ \repo -> closeIdleDatabase env repo duration
  exportOpenDBStats env

-- | set a counter glean.db.<repo>.open to the number of currently
-- open DBs for that particular repo name.
exportOpenDBStats :: Env -> IO ()
exportOpenDBStats env = do
  opens <- atomically $ do
    dbs <- readTVar $ envActive env
    forM (HashMap.toList dbs) $ \(repo, db) -> do
      state <- readTVar (dbState db)
      case state of
        Open{} -> return [(Thrift.repo_name repo, 1)]
        Opening{} -> return [(Thrift.repo_name repo, 1)]
        _ -> return []
  let repoOpenCounts = HashMap.fromListWith (+) (concat opens)
  forM_ (HashMap.toList repoOpenCounts) $ \(repoNm,count) -> do
    setCounter ("glean.db." <> Text.encodeUtf8 repoNm <> ".open") count


-- | Schedule DBs for deletion or expiration
expireDatabase :: Maybe NominalDiffTime -> Env -> Repo -> IO ()
expireDatabase delay env@Env{..} repo = do
  now <- getCurrentTime
  expired <- immediately $ do
    exp <- lift $ Catalog.readExpiring envCatalog repo
    case exp of
      Just t -> return (now > t)
      Nothing -> case delay of
        Just delay | delay > 0 -> do
          meta <- lift $ Catalog.readMeta envCatalog repo
          later $ do
            logInfo $ inRepo repo $ "database is doomed " ++
              " ("  ++ showNominalDiffTime (dbAge now meta) ++ " old)"
            logInfo $ inRepo repo $ "expiring in " <> show delay <> "s"
          lift $ Catalog.writeExpiring envCatalog repo $ delay `addUTCTime` now
          return False
        _ -> return True
  when expired $ void $ asyncDeleteDatabase env repo

-- | Database deletion thread
removeDatabase :: Env -> Repo -> TMVar (Maybe DB) -> IO ()
removeDatabase env@Env{..} repo todo = uninterruptibleMask_ $
  -- This runs under uninterruptibleMask_ because there is really nothing
  -- sensible we can do if we get interrupted.
  --
  --   * We need to close the DB even if the program is shutting down.
  --   * Closing the DB itself should be uninterruptible.
  --   * Once we start deleting things we shouldn't stop until we've deleted
  --     them all.
  --
  -- So really, the only sensible interruption points is after we've closed the
  -- DB but before we start deleting which just doesn't seem worth it.
  do
    r <- atomically $ readTMVar todo
    forM_ r $ \DB{..} -> do
      logInfo $ inRepo repo "deleting"
      addStatValueType "glean.db.deleted" 1 Stats.Sum
      atomically $ do
        users <- readTVar dbUsers
        when (users /= 0) retry
      logExceptions (\s -> inRepo repo $ "while deleting: " ++s) $ do
        state <- readTVarIO dbState
        case state of
          Open odb -> closeOpenDB env odb
            `finally` atomically (writeTVar dbState Closed)
          _ -> return ()
        Catalog.delete envCatalog repo
        Storage.delete envStorage repo
        safeRemovePathForcibly $ databasePath envRoot repo
      logInfo $ inRepo repo "deleted"
  `finally` atomically (modifyTVar' envDeleting $ HashMap.delete repo)

-- | Schedule a DB for deletion and return the 'Async' which can be used to
-- obtain the result.
asyncDeleteDatabase :: Env -> Repo -> IO (Async ())
asyncDeleteDatabase env@Env{..} repo = bracket
  newEmptyTMVarIO
  (\todo -> atomically $ tryPutTMVar todo Nothing) $ \todo -> do
    remover <- Warden.spawnMask envWarden $ const $ removeDatabase env repo todo
    join $ atomically $ do
      active <- HashMap.lookup repo <$> readTVar envActive
      r <- case active of
        Just db -> do
          modifyTVar' envActive $ HashMap.delete repo
          writeTVar (dbTailers db) mempty  -- this causes the tailers to stop
          return $ Just db
        Nothing -> do
          deleting <- HashMap.lookup repo <$> readTVar envDeleting
          exists <- Catalog.exists envCatalog [Local] repo
          case deleting of
            Nothing
              | exists -> do
                db <- DB repo
                  <$> newTVar Closed
                  <*> newTVar 0
                  <*> newTVar mempty
                return $ Just db
            _ -> return Nothing
      case r of
        Just db -> do
          modifyTVar' envDeleting $ HashMap.insert repo remover
          putTMVar todo $ Just db
          return $ return remover

        Nothing -> do
          putTMVar todo Nothing
          return $ throwIO $ UnknownDatabase repo

deleteDatabase :: Env -> Repo -> IO ()
deleteDatabase env repo = asyncDeleteDatabase env repo >>= Async.wait

restoreDatabase :: Env -> Text -> IO ()
restoreDatabase env loc
  | Just (prefix, site, repo) <-
      Backup.fromRepoLocator (envBackupBackends env) loc =
        restoreDatabase_ env prefix site repo
  | otherwise = throwIO $
      Thrift.InvalidLocator $ "invalid locator '" <> loc <>  "'"

restoreDatabase_ :: Backup.Site site => Env -> Text -> site -> Repo -> IO ()
restoreDatabase_ Env{..} prefix site repo = do
  props <- Backup.inspect site repo
  case metaFromProps loc props of
    Right meta -> atomically $ Catalog.startRestoring envCatalog repo meta
    Left err -> dbError repo $ concat
      ["invalid metadata in backup '", Text.unpack loc, "': ", err]
  where
    loc = Backup.toRepoLocator prefix site repo

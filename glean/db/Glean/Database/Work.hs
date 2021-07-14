-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Work
  ( resumeWork
  , completenessFromFill
  , scheduleTasks
  , getWork
  , workCancelled
  , workHeartbeat
  , workFinished
  , reapHeartbeats
  , failTask
  , unfinishDatabase
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import TextShow

import Util.Control.Exception (tryAll)
import Util.Defer
import Util.Log

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Repo
import Glean.Database.Open (lookupActiveDatabase)
import Glean.Database.Types
import Glean.Database.Work.Controller
import Glean.Database.Work.Heartbeat
import Glean.Database.Work.Queue
import Glean.Internal.Types as Thrift
import Glean.Recipes.Types (Executor(..), Recipe(..))
import Glean.Types as Thrift
import Glean.Util.Time

import qualified Data.HashSet as HashSet
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Observed as Observed

-- | Resume work which stopped when the previous 'Env' shut down.
resumeWork :: Env -> IO ()
resumeWork env = immediately $ do
  metas <- lift $ Catalog.list (envCatalog env) [Local] $
    statusV .==. DatabaseStatus_Incomplete
  forM_ metas $ \Item{..} ->
    case metaCompleteness itemMeta of
      Incomplete (DatabaseIncomplete_tasks tasks) ->
        forM_ (HashMap.toList tasks) (\(name, Task{..}) ->
            case task_state of
              TaskState_running (TaskRunning parcels) ->
                controller resumeTask Context
                  { ctxEnv = env
                  , ctxRepo = itemRepo
                  , ctxTask = name
                  , ctxRecipe = task_recipe
                  , ctxWorkFinished = workFinished
                  }
                parcels
              _ -> return ())
        `catch` \err -> do
          new_meta <- failTask itemMeta err
          lift $ Catalog.writeMeta (envCatalog env) itemRepo new_meta
      _ -> return ()

-- | Change a database's state from Complete to Incomplete.
-- WARNING! This is for testing only, and should
-- never be used on a production database.
unfinishDatabase :: Env -> Repo -> Handle -> IO()
unfinishDatabase env@Env{..} repo writeHandle = do
  backupPolicy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  let isBackupAllowed = repo_name repo `HashSet.member`
        ServerConfig.databaseBackupPolicy_allowed backupPolicy
  if isBackupAllowed
    then do
      throwM $ Thrift.Exception
        "The backup is enabled for this Repo so we cannot unfinish it"
    else immediately $ do
      meta <- lift $ Catalog.modifyMeta envCatalog repo $ \oldmeta ->
        case Thrift.metaCompleteness oldmeta of
          Thrift.Incomplete{} -> return oldmeta
          Thrift.Complete{} -> return oldmeta
            { Thrift.metaCompleteness =
                Thrift.Incomplete $
                  Thrift.DatabaseIncomplete_tasks $ tasksFromManual writeHandle
            }
          someState -> throwM $ Thrift.Exception
            (  "Cannot unfinish a database in state: "
            <> showCompleteness someState)
      new_meta <- handle (failTask meta) $ scheduleTasks env repo meta
      lift $ Catalog.writeMeta envCatalog repo new_meta
      return ()

completenessFromFill
  :: (Text -> IO (Map Text Recipe))
  -> Repo
  -> Maybe KickOffFill
  -> IO Completeness
completenessFromFill get_recipes repo mfill = Incomplete <$> case mfill of
  Just (KickOffFill_writeHandle handle) ->
    return $ DatabaseIncomplete_tasks $ tasksFromManual handle
  Just (Thrift.KickOffFill_scribe writeFromScribe) ->
    return $ DatabaseIncomplete_tasks $ tasksFromScribe writeFromScribe
  Just (Thrift.KickOffFill_recipes recipes) ->
    DatabaseIncomplete_tasks . tasksFromRecipes <$> get_recipes recipes
  Nothing ->
    DatabaseIncomplete_tasks . tasksFromRecipes <$> get_recipes (repo_name repo)

  Just Thrift.KickOffFill_EMPTY -> dbError repo "unexpected KickOffFill_EMPTY"

tasksFromRecipes :: Map Text Recipe -> Tasks
tasksFromRecipes = HashMap.fromList . fmap (fmap mkTask) . Map.toList
  where
    mkTask recipe = Task
      { task_recipe = recipe
      , task_state = TaskState_waiting def
      }

tasksFromScribe :: WriteFromScribe -> Tasks
tasksFromScribe WriteFromScribe{..} = tasksFromRecipes $ Map.singleton "" $ def
  { recipe_executor = Executor_External
  , recipe_heartbeat = 0
  , recipe_settings = Map.fromList $
      [("handle", writeFromScribe_writeHandle)
      ,("category", writeFromScribe_category)]
      ++
      (case writeFromScribe_bucket of
        Nothing -> []
        Just (PickScribeBucket_bucket bucket) -> [("bucket", showt bucket)])
      ++
      (case writeFromScribe_start of
        Nothing -> []
        Just (ScribeStart_start_time time) -> [("start_time", time)]
        Just (ScribeStart_checkpoint c) -> [("checkpoint", c)])
      ++
      [("no_base64_binary", "true")
        | maybe False sendJsonBatchOptions_no_base64_binary
            writeFromScribe_options]
  }

tasksFromManual :: Handle -> Tasks
tasksFromManual handle = tasksFromRecipes $ Map.singleton "" $ def
  { recipe_executor = Executor_Manual
  , recipe_heartbeat = 0
  , recipe_settings = Map.fromList [("handle", handle)]
  }

-- | Schedule all ready tasks for a particular database. Produce new metadata
-- and insert anything that's newly scheduled into 'Env''s work queue.
scheduleTasks :: Env -> Repo -> Meta -> Defer IO STM Meta
scheduleTasks env repo meta = case metaCompleteness meta of
  Incomplete (DatabaseIncomplete_tasks tasks) -> handle (failTask meta) $ do
    completeness <- if all taskComplete $ HashMap.elems tasks
      then return $ Finalizing def
      else Incomplete . DatabaseIncomplete_tasks <$>
        HashMap.traverseWithKey (schedule tasks) tasks
    return $! meta { metaCompleteness = completeness }
  _ -> return meta
  where
    taskComplete Task{task_state = TaskState_finished{}} = True
    taskComplete _ = False

    complete tasks task = maybe False taskComplete $ HashMap.lookup task tasks

    schedule tasks name task@Task{..}
      | TaskState_waiting{} <- task_state
      , all (complete tasks) $ Set.toList $ recipe_dependencies task_recipe = do
          later $ logInfo $ "scheduling " ++ show name
          parcels <- controller scheduleTask Context
            { ctxEnv = env
            , ctxRepo = repo
            , ctxTask = name
            , ctxRecipe = task_recipe
            , ctxWorkFinished = workFinished
            }
          return $! task
            { task_state = TaskState_running $ TaskRunning parcels }
    schedule _ name task = do
      later $ logInfo $ "not scheduling " ++ show name
      return task

-- | Various data about a particular work parcel.
data ParcelInfo = ParcelInfo
  { piRepo :: Repo
  , piTask :: Task
  , piParcels :: Vector.Vector ParcelState
  , piParcel :: Parcel
  , piState :: ParcelState
  }

-- | Compute the 'ParcelInfo' for a particular work parcel
parcelInfo :: Env -> Parcel -> STM ParcelInfo
parcelInfo env parcel = do
  Meta{..} <- Catalog.readMeta (envCatalog env) (parcelRepo parcel)
    `catch` \UnknownDatabase{} -> throwM $ AbortWork "invalid database"
  case metaCompleteness of
    Incomplete (DatabaseIncomplete_tasks tasks)
      | Just task <- HashMap.lookup (parcelTask parcel) tasks
      , TaskState_running (TaskRunning xs) <- task_state task ->
          return ParcelInfo
            { piRepo = parcelRepo parcel
            , piTask = task
            , piParcels = xs
            , piParcel = parcel
            , piState = xs Vector.! parcelIndex parcel
            }
    _ -> throwM $ AbortWork $
      "invalid parcel " <> Text.pack (show metaCompleteness)

failTask :: Meta -> FailedTaskError -> Defer IO STM Meta
failTask meta FailedTaskError{..} = do
  -- TODO: Clean up work queue - this will happen as workers
  -- ask for work at the moment.
  later $ do
    logError $ inRepo failedTaskRepo $ Text.unpack $ mconcat $
      [ "database failed in " ]
      ++
      [ "parcel " <> Text.pack (show parcel) <> " of "
        | Just parcel <- [failedTaskParcel] ]
      ++
      [ "task '", failedTaskName, "': ", failedTaskError ]
  return meta
    { metaCompleteness =
        Broken (DatabaseBroken failedTaskName failedTaskError)
    }

failParcel :: Env -> ParcelInfo -> Text -> Defer IO STM ()
failParcel env ParcelInfo{..} reason = do
  meta <- lift $ Catalog.readMeta (envCatalog env) piRepo
  new_meta <- failTask meta FailedTaskError
    { failedTaskRepo = piRepo
    , failedTaskName = parcelTask piParcel
    , failedTaskParcel = Just $ parcelIndex piParcel
    , failedTaskError = reason
    }
  lift $ Catalog.writeMeta (envCatalog env) piRepo new_meta

updateParcel :: Env -> ParcelInfo -> UTCTime -> ParcelState -> Defer IO STM ()
updateParcel env ParcelInfo{..} time state = do
  meta <- lift $ Catalog.readMeta (envCatalog env) piRepo
  let new_parcels = piParcels Vector.// [(parcelIndex piParcel, state)]
      task_state
        | Vector.all finished new_parcels =
            TaskState_finished $ TaskFinished $ utcTimeToPosixEpochTime time
        | otherwise = TaskState_running $ TaskRunning new_parcels
      new_meta = case metaCompleteness meta of
        Incomplete (DatabaseIncomplete_tasks tasks) -> meta
          { metaCompleteness = Incomplete $ DatabaseIncomplete_tasks $
              HashMap.insert
                (parcelTask piParcel)
                (piTask { task_state = task_state })
                tasks
          }
        _ -> meta
  real_meta <- case task_state of
    TaskState_finished{} -> do
      r <- lift $ lookupActiveDatabase env piRepo
      forM_ r $ \DB{..} -> do
        tailers <- lift $ readTVar dbTailers
        forM_ (HashMap.lookup (parcelTask piParcel) tailers) $ \tailer -> do
          lift $ writeTVar dbTailers
            $! HashMap.delete (parcelTask piParcel) tailers
          later $ Async.cancel $ tailerThread tailer
      scheduleTasks env piRepo new_meta
    _ -> return new_meta
  lift $ Catalog.writeMeta (envCatalog env) piRepo real_meta
  case metaCompleteness real_meta of
    Finalizing{} -> do
      later $ logInfo $ inRepo piRepo "database finalizing"
      lift makeReadOnly
    _ -> return ()
  where
    finished ParcelState_finished{} = True
    finished _ = False

    -- When a DB is complete, make it read-only to prevent further
    -- writes. It is an error to call workFinished on the final task
    -- if there are outstanding writes in the queue.
    makeReadOnly = do
      mdb <- lookupActiveDatabase env piRepo
      forM_ mdb $ \db -> do
        st <- readTVar (dbState db)
        case st of
          Open odb@(OpenDB { odbWriting = Just Writing{..} }) -> do
            -- NB. check the active counter as well as the queue,
            -- because this will tell us if there are writes currently
            -- in progress.
            active <- readTVar (writeQueueActive wrQueue)
            empty <- isEmptyTQueue (writeQueue wrQueue)
            -- If there are outstanding writes then the client is
            -- either broken or is intentionally trying to complete
            -- the DB early. But we can't complete the DB with
            -- outstanding writes, so we'll ask the client to retry
            -- the request later.
            when (active /= 0 || not empty) $
              throwM $ Retry 10
            writeTVar (dbState db) $ Open odb { odbWriting = Nothing }
          _ -> return ()

-- | Return the 'ParcelInfo' for a work parcel that's being executed by a worker
runningParcelInfo :: Env -> Thrift.Work -> STM ParcelInfo
runningParcelInfo env work = do
  info <- parcelInfo env (fromThriftWork work)
  when (not $ ok info) $ throwM $ AbortWork "wrong parcel handle"
  return info
  where
    ok info = case piState info of
      ParcelState_running p -> work_handle work == parcelRunning_handle p
      _ -> False

-- TODO: Move to Configerator (T64608954)
heartbeatFrequency :: Int
heartbeatFrequency = 60

-- TODO: Move to Configerator (T64608954)
heartbeatTimeout :: Int -> Int
heartbeatTimeout n = n * 10

-- | Get the next available bit of work for any task from the supplied list.
getWork :: Env -> Thrift.GetWork -> IO Thrift.GetWorkResponse
getWork env@Env{..} Thrift.GetWork{..} = do
  uuid <- UUID.nextRandom
  time <- getCurrentTime
  timepoint <- getTimePoint
  let grab = do
        parcel <- lift $ readWorkQueue
          envWorkQueue
          getWork_tasks
          (fromThriftWork <$> getWork_previous)
        r <- lift $ try $ parcelInfo env parcel
        case r of
          Right info@ParcelInfo{piState =
            ParcelState_waiting (ParcelWaiting retries), ..} -> do
            props <- metaProperties <$>
              now (Catalog.readMeta envCatalog $ parcelRepo parcel)
            updateParcel env info time $ ParcelState_running def
              { parcelRunning_retries = retries
              , parcelRunning_handle = UUID.toText uuid
              , parcelRunning_runner = getWork_runner
              }
            let work = Thrift.Work
                  { work_repo = parcelRepo parcel
                  , work_task = parcelTask parcel
                  , work_parcelIndex = fromIntegral $ parcelIndex parcel
                  , work_parcelCount =
                      fromIntegral $ recipe_parcels $ task_recipe piTask
                  , work_handle = UUID.toText uuid
                  }

                heartbeat = case recipe_heartbeat $ task_recipe piTask of
                  0 -> Nothing
                  n -> Just n

            forM_ heartbeat $ \n -> lift $ void $ expectHeartbeat
              envHeartbeats
              work
              $ addToTimePoint timepoint
                  $ seconds
                  $ fromIntegral
                  $ heartbeatTimeout
                  $ fromIntegral n
            return $ Thrift.GetWorkResponse_available
              Thrift.WorkAvailable
                { workAvailable_work = work
                , workAvailable_attempt = fromIntegral retries
                , workAvailable_heartbeat = heartbeat
                , workAvailable_properties = props
                }
          Right _ -> grab
          Left (_ :: SomeException) -> grab

      unavailable = Thrift.GetWorkResponse_unavailable
        Thrift.WorkUnavailable{ workUnavailable_pause = 30 }

  immediately $ grab <|> return unavailable

fromThriftWork :: Thrift.Work -> Parcel
fromThriftWork work = Parcel
  { parcelRepo = Thrift.work_repo work
  , parcelTask = Thrift.work_task work
  , parcelIndex = fromIntegral $ Thrift.work_parcelIndex work
  }

-- | A worker has cancelled work on a particular work parcel. Reenter it into
-- the work queue.
workCancelled :: Env -> Thrift.WorkCancelled -> IO ()
workCancelled env@Env{..} Thrift.WorkCancelled{..} = do
  time <- getCurrentTime
  immediately $ do
    pi@ParcelInfo{..} <- lift $ runningParcelInfo env workCancelled_work
    case piState of
      ParcelState_running ParcelRunning{..} -> do
        lift $ deleteHeartbeat envHeartbeats workCancelled_work
        later $ logInfo $
          "work cancelled, requeueing: " ++ show workCancelled_work
        updateParcel env pi time $
          ParcelState_waiting (ParcelWaiting parcelRunning_retries)
        lift $ writeWorkQueue envWorkQueue $ fromThriftWork workCancelled_work
      _ -> return ()

-- | Handle heartbeat from a worker.
workHeartbeat :: Env -> Thrift.WorkHeartbeat -> IO ()
workHeartbeat env Thrift.WorkHeartbeat{..} = do
  -- It should be fine to get the time now, this is when the heartbeat
  -- happened.
  now <- getTimePoint
  atomically $ do
    ParcelInfo{..} <- runningParcelInfo env workHeartbeat_work
    case recipe_heartbeat $ task_recipe piTask of
      0 -> return ()
      heartbeat -> void
        $ expectHeartbeat (envHeartbeats env) workHeartbeat_work
        $ addToTimePoint now
        $ seconds
        $ fromIntegral
        $ heartbeatTimeout
        $ fromIntegral heartbeat

-- | A worker has finished work on a particular parcel with the given
-- 'Outcome'.
workFinished :: Env -> Thrift.WorkFinished -> IO ()
workFinished env Thrift.WorkFinished{..} = do
  logInfo $ "workFinished " ++ show workFinished_work
  logInfo $ "workFinished_outcome " ++ show workFinished_outcome
  time <- getCurrentTime
  immediately $ do
    info@ParcelInfo{..} <- lift $ runningParcelInfo env workFinished_work
    lift $ deleteHeartbeat (envHeartbeats env) workFinished_work
    case workFinished_outcome of
      Thrift.Outcome_success{} ->
        updateParcel env info time (ParcelState_finished def)
      Thrift.Outcome_failure (Thrift.Failure reason) ->
        failParcel env info reason
      Thrift.Outcome_EMPTY{} -> failParcel env info "invalid outcome"

-- | Continuously check for heartbeat timeouts and take necessary actions.
reapHeartbeats :: Env -> IO ()
reapHeartbeats env = forever $ do
  time <- getCurrentTime
  now <- getTimePoint
  void $ tryAll $ immediately $ do
    timeouts <- lift $ checkHeartbeats (envHeartbeats env) now
    forM_ timeouts $ \work -> do
      r <- lift $ try $ runningParcelInfo env work
      case r of
        Right info@ParcelInfo
          { piState = ParcelState_running ParcelRunning{..}, ..}
          | parcelRunning_retries < recipe_retries (task_recipe piTask) -> do
              later $ logInfo $ "work timed out, requeueing: " ++ show work
              updateParcel env info time $
                ParcelState_waiting
                  (ParcelWaiting (fromIntegral parcelRunning_retries + 1))
              lift $ writeWorkQueue (envWorkQueue env) $ fromThriftWork work
          | otherwise -> do
              later $
                logInfo $ "work timed out, not retrying again: " ++ show work
              failParcel env info $ Text.unwords
                [ "timed out after"
                , Text.pack (show $ parcelRunning_retries + 1)
                , "attempts" ]
        Right _ -> return ()
        Left (_ :: SomeException) -> return ()
  threadDelay $ heartbeatFrequency * 2 * 1000000

{-# LANGUAGE DeriveGeneric #-}
module Glean.Database.Tailer
  ( startTailer
  , EventKind(..)
  , Event(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import GHC.Generics hiding (Meta)
import System.Exit (ExitCode(..))
import System.IO

import ServiceData.GlobalStats
import ServiceData.Types
import Util.Control.Exception
import Util.Log
import Util.Text

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Exception
import Glean.Database.Repo
import Glean.Database.Stuff (lookupActiveDatabase, withActiveDatabase)
import Glean.Database.Types
import Glean.Database.Writes
import Glean.Tailer
import Glean.Tailer.Types
import Glean.Types hiding (Database, Exception)
import Glean.Util.Observed as Observed
import qualified Glean.Util.Trace
import Glean.Util.Warden (spawnMask)

-- | Tailer lifecycle events that can be listened for.
data Event = Event
  { eventKind :: EventKind
  , eventRepo :: Repo
  , eventTask :: Text
  }
  deriving(Eq,Generic,Show,Typeable)

instance Hashable Event

-- | Tailer lifecycle event kinds.
data EventKind
  = TailerStarted
      -- ^ Started for new DB
  | TailerResumed Text
      -- ^ Resumed from checkpoint
  | TailerNotStarted
      -- ^ Not started because a newer tailer exists for same repo name/category
  | TailerFinished
      -- ^ Tailer process exited with exit code 0
  | TailerFailed
      -- ^ Tailer process exited with exit code /= 0
  | TailerCancelled
      -- ^ Tailer cancelled via async exception
  | TailerDuplicate
      -- ^ Tailer cancelled because a newer tailer started for same
      -- repo name/category
  | TailerAbandoned
      -- ^ Tailer cancelled because it has been removed from dbTailers for this
      -- task (shouldn't happen in the current implementation)
  | CheckpointWritten Text
      -- ^ Tailer checkpoint committed.
  deriving(Eq,Generic,Show,Typeable)

instance Hashable EventKind

-- | Local exception type, shouldn't escape
newtype TailerException = TailerException EventKind
  deriving(Eq,Show,Typeable)

instance Exception TailerException

-- | Start a tailer. It will add and remove itself from various data structures
-- as necessary.
--
startTailer
  :: Env
  -> Repo
  -> Text  -- ^ task name
  -> Map Text Text
      -- ^ settings (from recipe_settings, expected to have at least "category")
  -> Map Text Text
      -- ^ progress (from parcel_progress, empty initially)
  -> IO ()
startTailer env repo task_name settings progress = do
  category <- case Map.lookup "category" settings of
    Just category -> return category
    Nothing ->
      dbError repo "internal error: missing category in tailer settings"

  bucket <- case Map.lookup "bucket" settings of
    Nothing -> return Nothing
    Just str -> case textToInt str of
      Left err -> dbError repo $ "internal error: invalid bucket: " <> show err
      Right n -> return (Just n)

  void $ fixIO $ \thread -> spawnMask (envWarden env) $ \unmask -> do
    let tailer = Tailer
          { tailerRepo = repo
          , tailerTask = task_name
          , tailerCategory = category
          , tailerBucket = bucket
          , tailerThread = thread
          }

    r <- try $ do
      withActiveDatabase env repo $ \db -> atomically $ do
        started <- register env db tailer
        if started
          then notify $ maybe TailerStarted TailerResumed resume
          else throwSTM $ TailerException TailerNotStarted

      unmask $ withAsync (run_tailer category) $ \runner ->
        atomically $
          waitSTM runner <|>
            (waitStop env tailer >>= throwSTM . TailerException)

    let (kind, err_msg) = classifyResult tailer r

    atomically $ do
      unregister env tailer
      notify kind

    unmask $ logResult tailer kind err_msg
  where
    notify k = Glean.Util.Trace.notify (envListener env) Event
      { eventKind = k
      , eventRepo = repo
      , eventTask = task_name
      }

    resume = Map.lookup "checkpoint" progress

    run_tailer category = do
      serverConfig <- Observed.get (envServerConfig env)
      runTailer
        TailerSettings
          { setServerConfig = serverConfig
          , setSettings = settings
          , setResume = resume
          , setOptions = envTailerOpts env
          , setLogInfo = logInfo . inRepo repo
          , setLineBuffering = "line_buffering" `Text.isSuffixOf` category
          }
        $ write repo def
            { sendJsonBatchOptions_no_base64_binary =
                Map.lookup "no_base64_binary" settings == Just "true" }

    -- The format of a line from Scribe is
    --   <pred_name> \0 <pred_version> \0 <JSON fact>
    -- or multiple facts:
    --   <pred_name> \0 <pred_version> \0 <JSON fact> ... \0 <JSON fact>
    write :: Repo -> SendJsonBatchOptions -> Entry -> IO ()
    write repo _ (Checkpoint checkpoint) = do
      -- Ptail reported a checkpoint. We can restart from here if
      -- the server dies, but first we need to wait for the
      -- preceding writes to be flushed.
      vlog 1 $ inRepo repo $ "read checkpoint: " <> BS.unpack checkpoint
      addStatValueType "glean.db.write.checkpoint.read" 1 Sum
      enqueueCheckpoint env repo $ do
        vlog 1 $ inRepo repo $ ": storing checkpoint: " <> BS.unpack checkpoint
        addStatValueType "glean.db.write.checkpoint.stored" 1 Sum
        atomically $ do
          meta <- Catalog.readMeta (envCatalog env) repo
          case metaCompleteness meta of
            Incomplete (DatabaseIncomplete_tasks tasks) -> do
              let cp = Text.decodeUtf8 checkpoint
              Catalog.writeMeta (envCatalog env) repo meta
                { metaCompleteness = Incomplete $ DatabaseIncomplete_tasks $
                    HashMap.adjust
                      (\task@Task{..} -> case task_state of
                          TaskState_running (TaskRunning parcels)
                            | ParcelState_running parcel@ParcelRunning{..}
                                  <- V.head parcels -> task
                                { task_state = TaskState_running
                                    $ TaskRunning
                                    $ V.singleton
                                    $ ParcelState_running
                                    $ parcel
                                        { parcelRunning_progress =
                                            Map.singleton "checkpoint" cp
                                        }
                                }
                          _ -> task)
                      task_name
                      tasks
                }
              notify $ CheckpointWritten cp
            _ -> return ()
    write repo json_options entry@(LineData pred bytes) = do
      let remember = False
      r <- try $ enqueueJsonBatchByteString env repo pred
        (map BL.toStrict (BL.split '\0' bytes))
        json_options
        remember
      case r of
        Right{} -> do
          addStatValueType "glean.db.write.line.ok" 1 Sum
          return ()
        Left (Retry n) -> do
          threadDelay (round (max n 10.0 * 1000000))
          write repo json_options entry
    write _ _ (Invalid msg) = do
      addStatValueType "glean.db.write.line.fail" 1 Sum
      logWarning msg
      return ()

-- | Key for envTailers
catKey :: Tailer -> TailerKey
catKey Tailer{..} = (repo_name tailerRepo, tailerCategory, tailerBucket)

-- | Wait until the tailer should stop because it has been removed from one of
-- the tailer tables.
waitStop :: Env -> Tailer -> STM EventKind
waitStop env tailer =
  do
    r <- lookupActiveDatabase env $ tailerRepo tailer
    case r of
      Just DB{..} -> do
        db_tailers <- readTVar dbTailers
        case HashMap.lookup (tailerTask tailer) db_tailers of
          Just other | tailerThread tailer == tailerThread other -> retry
          _ -> return TailerAbandoned
      Nothing -> return TailerAbandoned
  <|>
  do
    cat_tailers <- readTVar $ envTailers env
    case HashMap.lookup (catKey tailer) cat_tailers of
      Just other
        | tailerThread tailer == tailerThread other -> retry
        | otherwise -> return TailerDuplicate
      Nothing -> return TailerAbandoned
  <|>
  do
    meta <- Catalog.readMeta (envCatalog env) $ tailerRepo tailer
    case metaCompleteness meta of
      Incomplete{} -> retry
      _ -> return TailerAbandoned

register :: Env -> DB -> Tailer -> STM Bool
register Env{..} DB{..} tailer@Tailer{..} = do
  db_tailers <- readTVar dbTailers
  cat_tailers <- readTVar envTailers
  case HashMap.lookup tailerTask db_tailers of
    Nothing -> do
      ok <- case HashMap.lookup (catKey tailer) cat_tailers of
        Just Tailer{tailerRepo = other_repo} -> do
          -- there already exists a tailer for this repo_name/category,
          -- check if we're newer
          my_meta <- Catalog.readMeta envCatalog dbRepo
          other_meta <- Catalog.readMeta envCatalog other_repo
          return $ metaCreated my_meta >= metaCreated other_meta
        Nothing -> return True
      when ok $ do
        modifyTVar' dbTailers $ HashMap.insert tailerTask tailer
        modifyTVar' envTailers $ HashMap.insert (catKey tailer) tailer
      return ok

    -- there already exists a tailer for this task, don't start
    Just _ -> return False

unregister :: Env -> Tailer -> STM ()
unregister env tailer = do
  r <- lookupActiveDatabase env $ tailerRepo tailer
  forM_ r $ \DB{..} -> unregister_from dbTailers $ tailerTask tailer
  unregister_from (envTailers env) $ catKey tailer
  where
    unregister_from
      :: (Eq key, Hashable key) => TVar (HashMap key Tailer) -> key -> STM ()
    unregister_from v = modifyTVar' v . HashMap.update
      (\other -> if tailerThread tailer == tailerThread other
        then Nothing
        else Just other)

classifyResult
  :: Tailer -> Either SomeException ExitCode -> (EventKind, Maybe String)
classifyResult Tailer{..} (Left exc)
  | Just (TailerException kind) <- fromException exc = (kind, Nothing)
  | isSyncException exc =
      ( TailerFailed
      , Just
        $ show
        $ fromMaybe (dbException tailerRepo $ "tailer error: " ++ show exc)
        $ fromException exc )
  | otherwise = (TailerCancelled, Nothing)
classifyResult _ (Right ExitSuccess) = (TailerFinished, Nothing)
classifyResult _ (Right ExitFailure{}) = (TailerFailed, Nothing)

logTailer :: (String -> IO ()) -> Tailer -> String -> IO ()
logTailer log Tailer{..} msg = log $ inRepo tailerRepo $
  msg ++ " (category " ++ Text.unpack tailerCategory ++ bucket ++ ")"
  where
    bucket = case tailerBucket of
      Nothing -> ""
      Just b -> ":" ++ show b

logTailerInfo :: Tailer -> String -> IO ()
logTailerInfo = logTailer logInfo

logTailerError :: Tailer -> String -> IO ()
logTailerError = logTailer logError

logResult :: Tailer -> EventKind -> Maybe String -> IO ()
logResult tailer _ (Just msg) = logTailerError tailer msg
logResult tailer TailerNotStarted _ =
  logTailerError tailer "dropped duplicate tailer"
logResult tailer TailerFinished _ =
  logTailerInfo tailer "tailer finished"
logResult tailer TailerFailed _ =
  logTailerError tailer "tailer process failed"
logResult tailer TailerCancelled _ =
  logTailerInfo tailer "tailer cancelled"
logResult tailer TailerDuplicate _ =
  logTailerError tailer "stopped duplicate tailer"
logResult tailer TailerAbandoned _ =
  logTailerInfo tailer "tailer stopped"
logResult _ _ _ = return ()

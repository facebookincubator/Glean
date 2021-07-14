-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Delete (
  deleteDatabase,
  asyncDeleteDatabase,
  expireDatabase
) where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception hiding(handle)
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HashMap
import Data.Time

import ServiceData.GlobalStats as Stats
import ServiceData.Types as Stats
import Util.Control.Exception
import Util.Defer
import Util.IO (safeRemovePathForcibly)
import Util.Log

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Close
import Glean.Database.Meta
import Glean.Database.Repo
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Types hiding (Database)
import Glean.Util.Time
import qualified Glean.Util.Warden as Warden


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

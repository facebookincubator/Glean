-- Copyright (c) Facebook, Inc. and its affiliates.


module Glean.Util.Warden
  ( Warden
  , withWarden
  , create
  , shutdown
  , spawn
  , spawn_
  , spawnMask
  , spawnDaemon
  , adopt
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import System.IO (fixIO)

import Util.Log

-- | A 'Warden' is an owner of 'Async's which cancels them on 'shutdown'.
--
-- 'Nothing' in the MVar means the 'Warden' has been shut down.
newtype Warden = Warden (MVar (Maybe (HashSet (Async ()))))

-- | Run the action with a new 'Warden', shutting it down when the action
-- exits.
withWarden :: (Warden -> IO a) -> IO a
withWarden = bracket create shutdown

-- | Create a new 'Warden'.
create :: IO Warden
create = Warden <$> newMVar (Just mempty)

-- | Shutdown a 'Warden', cancelling all owned threads. Subsequent calls to
-- 'spawn' and 'shutdown' will be no-ops.
shutdown :: Warden -> IO ()
shutdown (Warden v) = do
  r <- swapMVar v Nothing
  mapM_ (Async.mapConcurrently_ Async.cancel) r

forget :: Warden -> Async a -> IO ()
forget (Warden v) async = modifyMVar_ v $ \x -> case x of
  Just xs -> return $! Just $! HashSet.delete (void async) xs
  Nothing -> return Nothing

-- | Spawn a thread with masked exceptions and pass an unmask function to the
-- action.
spawnMask :: Warden -> ((forall b. IO b -> IO b) -> IO a) -> IO (Async a)
spawnMask (Warden v) action = modifyMVar v $ \r -> case r of
  Just asyncs -> do
    -- Create a new thread which removes itself from the 'HashSet' when it
    -- exits.
    this <- fixIO $ \this -> mask_ $ Async.asyncWithUnmask $ \unmask ->
      action unmask `finally` forget (Warden v) this
    return (Just $ HashSet.insert (void this) asyncs, this)
  Nothing -> (,) Nothing <$> Async.async (throwIO Async.AsyncCancelled)

-- | Spawn a new thread owned by the 'Warden'.
spawn :: Warden -> IO a -> IO (Async a)
spawn warden action = spawnMask warden $ \unmask -> unmask action

-- | Spawn a new thread owned by the 'Warden'.
spawn_ :: Warden -> IO () -> IO ()
spawn_ w = void . spawn w

-- | Run the given IO action repeatedly in a separate thread, catching
-- and logging any exceptions (Sync or Async) that it throws. Ends
-- at 'Warden' shutdown.
spawnDaemon :: Warden -> String -> IO () -> IO ()
spawnDaemon w name io =
  void $ spawnMask w $ \restore ->
    forever $
      handle (\ex -> case ex of
        _ | Just Async.AsyncCancelled{} <- fromException ex -> throwIO ex
          | otherwise -> logError (name <> ": " <> show ex)) $
        restore io

-- | Adopt an existing thread which will be cancelled when 'Warden' shut down.
-- If the 'Warden' has already been shut down, the thread will be cancelled
-- immediately.
adopt :: Warden -> Async a -> IO ()
adopt (Warden v) async = modifyMVar_ v $ \r -> case r of
  Just asyncs -> do
    void $ forkIO $ do
      void $ Async.waitCatch async
      forget (Warden v) async
    return $ Just $ HashSet.insert (void async) asyncs
  Nothing -> do
    Async.cancel async
    return Nothing
